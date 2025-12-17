# nolint start

# -------- Outdated functions for merged STN-i graphs --------

#' Load and merge multiple STN-i data files from a specified folder.
#'
#' This function reads multiple `.Rdata` files containing STN-i graph objects,
#' merges them into a single graph, and extracts relevant metadata such as
#' algorithm names, problem type, best known solutions, and number of runs.
#'
#' @param input_folder The folder containing the `.Rdata` files.
#'
#' @return A list containing:
#' - `graphs`: A list of merged STN-i graph objects.
#' - `names`: Names of the algorithms extracted from the file names.
#' - `problem_type`: The type of problem (min or max) from the first file.
#' - `best_known_solution`: Best known solutions from each file.
#' - `number_of_runs`: Number of runs for each algorithm.
#'
#' @examples
#' \dontrun{
#' input_folder <- "path/to/stn_i_data"
#' merged_data <- load_stn_i_data(input_folder)
#' }
get_stns_i_data <- function(input_folder) {
  # Check if the input folder contains at least 2 .Rdata files
  files <- list.files(input_folder, pattern = "\\.Rdata$", full.names = TRUE)
  if (length(files) < 2) {
    stop("At least 2 .Rdata files required to merge STN-i data.")
  }

  # Initialize lists to store graphs and metadata
  graphs <- list()
  names <- character(length(files))
  problem_types <- c()
  number_of_runs_vec <- c()

  for (i in seq_along(files)) {
    input_file <- files[i]

    # Obtain the STN-i data from the file
    stn_i_result <- get_stn_i_data(input_file)

    STN_i <- stn_i_result$STN_i
    alg_name <- stn_i_result$network_name
    V(STN_i)$Network <- alg_name
    E(STN_i)$Network <- alg_name

    graphs[[i]] <- STN_i
    names[i] <- stn_i_result$network_name
    problem_types <- c(problem_types, stn_i_result$problem_type)
    number_of_runs_vec <- c(number_of_runs_vec, stn_i_result$number_of_runs)
  }

  if (length(unique(problem_types)) != 1) {
    stop("All input STN-i files must have the same problem type.")
  }

  return(list(
    graphs = graphs,
    names = names,
    problem_type = unique(problem_types),
    number_of_runs = number_of_runs_vec
  ))
}

#' Merge multiple STN-i graphs into a single network (manual method)
#'
#' This function builds a unified STN graph from multiple STN-i graphs,
#' resolving conflicts in node attributes (Fitness, Topology) using specified criteria,
#' and classifies shared and elite nodes. Unlike the union approach, it manually
#' constructs the graph from extracted nodes and edges.
#'
#' @param stns_i_data A list returned by `get_stns_i_data`, containing:
#'   - `graphs`: list of STN-i igraph objects
#'   - `names`: character vector of network names
#'   - `problem_type`: "min" or "max"
#'   - `number_of_runs`: vector with number of runs per network
#' @param criteria One of: "min", "max", "mean", "median", "mode" to resolve Fitness conflicts
#' @param verbose Boolean indicating whether to print debug information (default is FALSE)
#'
#' @return A list with:
#'   - `stnm`: the merged igraph object
#'   - `num_networks`: number of merged networks
#'   - `network_names`: names of the original networks
#'   - `best_known_solution`: best known solutions
#'
#' @examples
#' \dontrun{
#' merged_data <- merge_stns_i_data(stns_i_data, criteria = "mean")
#' }
merge_stns_i_data <- function(stns_i_data, criteria = "mean", verbose = FALSE) {

  # Check if the criteria is valid
  if (!criteria %in% c("min", "max", "mean", "median", "mode")) {
    stop("Invalid criteria. Choose from: 'min', 'max', 'mean', 'median', 'mode'.")
  }

  snts_i <- stns_i_data$graphs
  num_networks <- length(snts_i)
  number_of_runs <- sum(stns_i_data$number_of_runs)
  network_names <- stns_i_data$names
  problem_type <- stns_i_data$problem_type

  # Collect all nodes with debug information
  node_df_list <- list()
  for (i in seq_along(snts_i)) {
    g <- snts_i[[i]]
    
    # Debug information
    if (verbose) {
      cat(sprintf("\nProcessing network %d/%d:\n", i, length(snts_i)))
      cat(sprintf("Number of nodes: %d\n", vcount(g)))
    }
    
    # Extract and validate vertex attributes
    node_names <- V(g)$name
    node_fitness <- V(g)$Fitness
    node_topology <- V(g)$Topology
    node_count <- V(g)$Count
    node_quality <- V(g)$Quality
    node_network <- V(g)$Network
    
    # Debug attribute information
    if (verbose) {
      cat("Sample of Fitness values:", head(node_fitness), "\n")
      cat("Number of NA in Fitness:", sum(is.na(node_fitness)), "\n")
    }
    
    # Create data frame with explicit type conversion
    node_df <- data.frame(
      Node = as.character(node_names),
      Fitness = as.numeric(node_fitness),
      Topology = as.character(node_topology),
      Count = as.numeric(node_count),
      Quality = as.character(node_quality),
      Network = as.character(node_network),
      stringsAsFactors = FALSE
    )
    node_df$graph_id <- i

    if (verbose) {
      cat("Structure of node_df:\n")
      str(node_df)
    }

    node_df_list[[i]] <- node_df
  }
  nodes_all <- bind_rows(node_df_list)
  
  if (verbose) {
    cat("\nCompleted processing all networks.\n")

    # Debug combined data
    cat("\nAfter combining all networks:\n")
    cat("Total number of nodes:", nrow(nodes_all), "\n")
    cat("Number of NA in combined Fitness:", sum(is.na(nodes_all$Fitness)), "\n")

    # Debug before merge
    cat("\nBefore merging nodes:\n")
    cat("Unique nodes:", length(unique(nodes_all$Node)), "\n")
    cat("Range of Fitness values:", range(nodes_all$Fitness, na.rm = TRUE), "\n")
  }
  
  # Merge nodes with more detailed error handling
  merged_nodes <- nodes_all %>%
    group_by(Node) %>%
    summarise(
      Fitness = {
        vals <- Fitness[!is.na(Fitness)]
        if (length(vals) == 0) {
          warning(sprintf("No valid Fitness values for node %s", first(Node)))
          NA 
        } else {
          result <- switch(criteria,
            "min" = min(vals),
            "max" = max(vals),
            "mean" = mean(vals),
            "median" = median(vals),
            "mode" = as.numeric(names(sort(table(vals), decreasing = TRUE)[1]))
          )
          if (is.na(result)) {
            warning(sprintf("Calculation resulted in NA for node %s with values: %s", 
                          first(Node), paste(vals, collapse=", ")))
          }
          result
        }
      },
      Topology = {
        types <- unique(na.omit(Topology[Topology != ""]))
        if (length(types) == 0) {
          warning(sprintf("No valid Topology for node %s", first(Node)))
          "STANDARD"
        } else if ("END" %in% types) "END" 
        else if ("START" %in% types) "START" 
        else "STANDARD"
      },
      Count = sum(Count, na.rm = TRUE),
      Quality = {
        qual <- paste(Quality[!is.na(Quality) & Quality != ""], collapse = "")
        if (qual == "") warning(sprintf("Empty Quality for node %s", first(Node)))
        qual
      },
      Network = {
        net <- paste(Network[!is.na(Network) & Network != ""], collapse = "")
        if (net == "") warning(sprintf("Empty Network for node %s", first(Node)))
        net
      },
      n_networks = n(),
      .groups = "drop"
    )
    
  # Debug after merge
  if (verbose) {
    cat("\nAfter merging nodes:\n")
    cat("Number of merged nodes:", nrow(merged_nodes), "\n")
    cat("Number of NA in merged Fitness:", sum(is.na(merged_nodes$Fitness)), "\n")
    cat("Range of merged Fitness values:", range(merged_nodes$Fitness, na.rm = TRUE), "\n")
  }

  merged_nodes$Shared <- merged_nodes$n_networks > 1
  merged_nodes$Category <- mapply(function(q, shared) {
    # Split the Quality string into tags
    tags <- unlist(strsplit(q, "(?<=REGULAR)|(?<=ELITE)|(?<=BEST)", perl = TRUE))
    
    # Count occurrences of each tag (ignore BEST for this classification because it always changes)
    elite_count <- sum(tags == "ELITE")
    regular_count <- sum(tags == "REGULAR")
    #best_count <- sum(tags == "BEST")

    if (shared && elite_count > 0 && regular_count == 0) {
      "shared-elite"
    } else if (shared && elite_count > 0 && regular_count > 0) {
      "shared-mixed"
    } else if (shared && elite_count == 0) {
      "shared-regular"
    } else if (!shared && elite_count > 0) {
      "network-elite"
    } else {
      "network-regular"
    }
  }, merged_nodes$Quality, merged_nodes$Shared)

  # Rename for igraph compatibility
  names(merged_nodes)[names(merged_nodes) == "Node"] <- "name"

  # Build edge list
  edge_df_list <- list()
  for (i in seq_along(snts_i)) {
    g <- snts_i[[i]]
    edf <- data.frame(
      from = as.character(ends(g, es = E(g), names = TRUE)[,1]),
      to = as.character(ends(g, es = E(g), names = TRUE)[,2]),
      weight = E(g)$weight,
      Network = E(g)$Network,
      stringsAsFactors = FALSE
    )
    edf$weight <- E(g)$weight
    edf$Network <- E(g)$Network
    edge_df_list[[i]] <- edf
  }
  edges_all <- bind_rows(edge_df_list)

  merged_edges <- edges_all %>%
    group_by(from, to) %>%
    summarise(
      weight = sum(weight, na.rm = TRUE),
      Network = paste(Network[!is.na(Network) & Network != ""], collapse = ""),
      .groups = "drop"
    )

  # Create final graph
  merged_STN_i <- graph_from_data_frame(d = merged_edges, vertices = merged_nodes, directed = TRUE)

  # Identify and assign BEST node(s)
  fitness_vals <- V(merged_STN_i)$Fitness
  best_known_solution <- if (problem_type == "min") {
    best_known_solution <- min(fitness_vals)
  } else {
    best_known_solution <- max(fitness_vals)
  }
  best_ids <- if (problem_type == "min") {
    which(fitness_vals <= best_known_solution)
  } else {
    which(fitness_vals >= best_known_solution)
  }
  V(merged_STN_i)[best_ids]$Category <- "BEST"

  return(list(
    merged_STN_i = merged_STN_i,
    num_networks = num_networks,
    network_names = network_names,
    problem_type = problem_type,
    best_known_solution = best_known_solution,
    number_of_runs = number_of_runs
  ))
}

#' Save the merged STN-i data to a file
#' 
#'  This function saves the merged STN-i data to a specified output file path.
#' 
#'  @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#'  @param output_file_path A string specifying the path to the output file where the merged data will be saved.
#'  @param verbose A boolean indicating whether to print summary metrics before saving (default is FALSE).
#' 
#'  @return NULL
#' 
#' @examples
#' \dontrun{
#' save_merged_stn_i_data(merged_STN_i, "path/to/output.Rdata")
#' }
save_merged_stn_i_data <- function(merged_stn_i_data, output_file_path, verbose = FALSE) {
  # Ensure the file name ends in .Rdata
  if (!grepl("\\.Rdata$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".Rdata")
  }

  # Print summary metrics before saving
  g <- merged_stn_i_data$merged_STN_i
  if (verbose) {
    cat("\nSaving merged STN-i with summary metrics:\n")
    cat(" - Total nodes:", vcount(g), "\n")
    cat(" - Total edges:", ecount(g), "\n")
    cat(" - Shared nodes:", sum(V(g)$Shared), "\n")
    cat(" - shared-elite:", sum(V(g)$Category == "shared-elite"), "\n")
    cat(" - shared-regular:", sum(V(g)$Category == "shared-regular"), "\n")
    cat(" - shared-mixed:", sum(V(g)$Category == "shared-mixed"), "\n")
    cat(" - network-elite:", sum(V(g)$Category == "network-elite"), "\n")
    cat(" - network-regular:", sum(V(g)$Category == "network-regular"), "\n")
  }

  save(merged_stn_i_data, file = output_file_path)
  message(paste("Merged STN-i data saved to:", output_file_path))
}

#' Load merged STN-i data from a file
#' 
#' This function loads merged STN-i data from a specified input file and validates its structure.
#' 
#' @param input_file A string specifying the path to the input file containing the merged STN-i data.
#' 
#' @return A list containing the loaded merged STN-i data, including the merged graph, number of networks, network names, problem type, and best known solutions.
#' 
#' @examples
#' \dontrun{
#'  get_merged_stn_i_data("path/to/merged_stn_i_file.Rdata")
#' }
get_merged_stn_i_data <- function(input_file) {
  # Check if file exists
  if (!file.exists(input_file)) {
    stop(paste("Input file does not exist:", input_file), call. = FALSE)
  }

  # Load the object and retrieve its name
  loaded_name <- load(input_file)
  merged_stn_i_data <- get(loaded_name)

  # Validate structure
  expected_fields <- c(
    "merged_STN_i",
    "num_networks",
    "network_names",
    "problem_type",
    "best_known_solution",
    "number_of_runs"
  )

  if (!is.list(merged_stn_i_data) || !all(expected_fields %in% names(merged_stn_i_data))) {
    stop("The loaded file does not contain a valid merged STN-i result structure.", call. = FALSE)
  }

  return(merged_stn_i_data)
}

#' Get the palette colors for merged STN-i visualization
#'
#' This function returns a flat list of colors for merged STN-i visualization,
#' including shared categories, algorithm-specific elite/regular colors, and best node color.
#'
#' @param palette A numeric value specifying the color palette option (1 to 4).
#'
#' @return A named list of color values.
#'
#' @examples
#' \dontrun{
#' get_merged_stn_i_palette_colors(2)
#' }
get_merged_stn_i_palette_colors <- function(palette = 1) {
  palette_colors <- switch(as.character(palette),
    "1" = list(
      shapes = "black",
      shared_elite = "#FFD700",     # Gold
      shared_regular = "#CCCCCC",   # Light Gray
      shared_mixed = "#DA70D6",     # Orchid
      best = "red",             # Dark Orange
      algorithm_elite = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                          "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                          "#bcbd22", "#17becf"),
      algorithm_regular = c("#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
                            "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7",
                            "#dbdb8d", "#9edae5")
    ),
    "2" = list(
      shapes = "black",
      shared_elite = "#DAA520",     # Goldenrod
      shared_regular = "#D3D3D3",   # Light Gray
      shared_mixed = "#BA55D3",     # Medium Orchid
      best = "#FF6347",             # Tomato
      algorithm_elite = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                          "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                          "#999999", "#66c2a5"),
      algorithm_regular = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4",
                            "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec",
                            "#f2f2f2", "#b2df8a")
    ),
    "3" = list(
      shapes = "black",
      shared_elite = "#FFA500",     # Orange
      shared_regular = "#E0E0E0",   # Gray 88
      shared_mixed = "#DDA0DD",     # Plum
      best = "#B22222",             # Firebrick
      algorithm_elite = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3",
                          "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
                          "#a1d99b", "#9ecae1"),
      algorithm_regular = c("#ccece6", "#fdd0a2", "#bcbddc", "#fbb4b9",
                            "#c2e699", "#fff7bc", "#d9d9d9", "#e5f5e0",
                            "#edf8fb", "#f0f0f0")
    ),
    "4" = list(
      shapes = "black",
      shared_elite = "#C71585",     # Medium Violet Red
      shared_regular = "#B0C4DE",   # Light Steel Blue
      shared_mixed = "#9370DB",     # Medium Purple
      best = "#DC143C",             # Crimson
      algorithm_elite = c("#003f5c", "#2f4b7c", "#665191", "#a05195",
                          "#d45087", "#f95d6a", "#ff7c43", "#ffa600",
                          "#7a5195", "#ef5675"),
      algorithm_regular = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                            "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
                            "#cab2d6", "#6a3d9a")
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4.")
  )

  return(palette_colors)
}

#' Decorate the merged STN-i graph with colors and shapes
#'
#' This function decorates the merged STN-i graph by assigning colors and shapes to nodes
#' based on their category (shared, elite, regular, etc.) and topology (START, STANDARD, END).
#'
#' @param merged_STN_i The merged STN-i igraph object.
#' @param network_names A character vector of network names, used to assign algorithm colors.
#' @param problem_type A string indicating the problem type ("min" or "max").
#' @param show_shared_regular Logical; whether to show shared-regular nodes (default TRUE).
#' @param show_shared_mixed Logical; whether to show shared-mixed nodes (default TRUE).
#' @param show_regular Logical; whether to show network-regular (non-shared) nodes (default TRUE).
#' @param show_start_regular Logical; whether to show start regular nodes (default TRUE).
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#'
#' @return The decorated igraph object.
#' 
#' @examples
#' \dontrun{
#' merged_STN_i <- merged_stn_i_decorate(merged_STN_i, network_names, problem_type = "min", show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_merged_stn_i_palette_colors(1))
#' }
merged_stn_i_decorate <- function(merged_STN_i, network_names, problem_type = "min", show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors) {

  # Filter vertices according to settings
  to_remove <- c()
  if (!show_shared_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "shared-regular"]$name)
  }
  if (!show_shared_mixed) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "shared-mixed"]$name)
  }
  if (!show_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "network-regular"]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Topology == "START" & (Category == "network-regular" | Category == "shared-regular")]$name)
  }

  # Remove duplicates before deletion
  to_remove <- unique(to_remove)
  merged_STN_i <- delete_vertices(merged_STN_i, to_remove)

  # Set default color to NA
  V(merged_STN_i)$color <- NA

  # Assign colors based on category
  V(merged_STN_i)[Category == "shared-regular"]$color <- palette_colors$shared_regular
  V(merged_STN_i)[Category == "shared-elite"]$color <- palette_colors$shared_elite
  V(merged_STN_i)[Category == "shared-mixed"]$color <- palette_colors$shared_mixed
  V(merged_STN_i)[Category == "BEST"]$color <- palette_colors$best

  for (i in seq_along(network_names)) {
    network_name <- network_names[i]
    elite_color <- palette_colors$algorithm_elite[i]
    regular_color <- palette_colors$algorithm_regular[i]

    V(merged_STN_i)[Category == "network-elite" & Network == network_name]$color <- elite_color
    V(merged_STN_i)[Category == "network-regular" & Network == network_name]$color <- regular_color
  }

  # Set node shapes by topology
  V(merged_STN_i)$shape <- "circle"
  V(merged_STN_i)[Topology == "START"]$shape <- "square"
  V(merged_STN_i)[Topology == "END"]$shape <- "triangle"

  # Set node size proportional to Count (how many times node appears across networks)
  V(merged_STN_i)$size <- V(merged_STN_i)$Count + 1

  # Set edge width proportional to weight
  E(merged_STN_i)$width <- E(merged_STN_i)$weight

  # Set edge colors based on node colors
  # If both nodes have the same color, use that color for the edge;
  # otherwise, use the color of the "from" node
  edge_ends <- ends(merged_STN_i, es = E(merged_STN_i), names = FALSE)
  edge_colors <- apply(edge_ends, 1, function(e) {
    from_color <- V(merged_STN_i)$color[e[1]]
    to_color <- V(merged_STN_i)$color[e[2]]
    if (!is.na(from_color) && !is.na(to_color) && from_color == to_color) {
      return(from_color)
    } else {
      return(from_color)
    }
  })
  E(merged_STN_i)$color <- edge_colors

  return(merged_STN_i)
}

#' Create a plot for merged STN-i data
#' 
#' This function processes the input merged STN-i file, decorates the STN object,
#' and creates a plot.
#' 
#' @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#' @param show_shared Logical; whether to show shared nodes (default TRUE).
#' @param show_regular Logical; whether to show regular nodes (default TRUE).
#' @param show_elite Logical; whether to show elite nodes (default TRUE).
#' @param show_start_regular Logical; whether to show start regular nodes (default TRUE).
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#' @param zoom_quantile Numeric value between 0 and 1 to define a zoom level for the plot (default is NA, meaning no zoom).
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#'  merged_stn_i_plot_create(merged_stn_i_data, show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors = my_palette_colors)
#' }
merged_stn_i_plot_create <- function(merged_stn_i_data, show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors, zoom_quantile = NA) {

  # Obtain the merged STN-i object, network names, and problem type
  merged_STN_i <- merged_stn_i_data$merged_STN_i
  network_names = merged_stn_i_data$network_names
  problem_type <- merged_stn_i_data$problem_type

  # Decorate the STN-i object
  merged_STN_i <- merged_stn_i_decorate(merged_STN_i, network_names, problem_type, show_shared_regular, show_shared_mixed, show_regular, show_start_regular, palette_colors)

  # If zooming is enabled, extract subgraph
  if (!is.na(zoom_quantile) && zoom_quantile > 0 && zoom_quantile < 1) {
    merged_STN_i <- get_zoomed_graph(merged_STN_i, zoom_quantile, problem_type)
  }

  # Return everything needed for external plotting
  return(merged_STN_i)
}

#' Save a plot of the merged STN-i graph
#'
#' This function saves a decorated merged STN-i graph to a PDF file,
#' including legends for shared and network-specific nodes, using a given layout.
#'
#' @param output_file_path Output path for the PDF plot.
#' @param merged_STN_i A decorated igraph object of the merged STN-i.
#' @param network_names A character vector of network names, used for legend labels.
#' @param layout_data A list with layout coordinates and title.
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#' @param nsizef Numeric factor for node sizes.
#' @param ewidthf Numeric factor for edge widths.
#' @param asize Arrow size for edges.
#' @param ecurv Curvature of edges.
#'
#' @return None. Saves a PDF to the specified path.
#' 
#' @examples
#' \dontrun{
#' save_merged_stn_i_plot("output/merged_stn_i_plot.pdf", merged_STN_i, network_names, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3)
#' }
save_merged_stn_i_plot <- function(output_file_path, merged_STN_i, network_names, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3) {

  # Ensure the file name ends in .pdf
  if (!grepl("\\.pdf$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".pdf")
  }

  # Triangle shape for END nodes
  mytriangle <- function(coords, v = NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1 / 200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color, col = vertex.color,
            stars = cbind(vertex.size, vertex.size, vertex.size),
            add = TRUE, inches = FALSE)
  }
  add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

  # Legend setup
  legend.txt <- c("Start", "Standard", "End", "Best")
  legend.col <- c(palette_colors$shapes, palette_colors$shapes, palette_colors$shapes, palette_colors$best)
  legend.shape <- c(15, 16, 17, 16)

  # Shared node categories
  legend.txt <- c(legend.txt, "Shared-Regular", "Shared-Elite", "Shared-Mixed")
  legend.col <- c(legend.col, palette_colors$shared_regular, palette_colors$shared_elite, palette_colors$shared_mixed)
  legend.shape <- c(legend.shape, 16, 16, 16)

  # Network-specific categories
  for (i in seq_along(network_names)) {
    legend.txt <- c(legend.txt,
                    paste0(network_names[i], "-Regular"),
                    paste0(network_names[i], "-Elite"))
    legend.col <- c(legend.col,
                    palette_colors$algorithm_regular[i],
                    palette_colors$algorithm_elite[i])
    legend.shape <- c(legend.shape, 16, 16)
  }

  # Open PDF device
  pdf(output_file_path)

  # Compute node sizes
  maxns <- max(V(merged_STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(merged_STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(merged_STN_i)$size + 1
  } else {
    nsize <- nsizef * V(merged_STN_i)$size
  }

  ewidth <- ewidthf * E(merged_STN_i)$width
  title <- paste(layout_data$title, "\nNodes:", vcount(merged_STN_i), "Edges:", ecount(merged_STN_i))

  # Plot
  plot(merged_STN_i, layout = layout_data$coords, vertex.label = "", vertex.size = nsize, main = title, edge.width = ewidth, edge.arrow.size = asize, edge.curved = ecurv)

  legend("topleft", legend.txt, pch = legend.shape, col = legend.col, pt.bg = legend.col, cex = 0.7, pt.cex = 1.35, bty = "n")

  dev.off()

  message("Merged STN-i plot saved successfully to: ", output_file_path)
}

#' Save STN-i metrics to a CSV file
#'
#' This function saves a list of STN-i metrics to a CSV file.
#'
#' @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#'
#' @return A list of metrics extracted from the merged STN-i object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @examples
#' \dontrun{
#' merged_stn_i_data <- get_merged_stn_i_data("path/to/merged_stn_i_file.Rdata")
#' metrics <- get_merged_stn_i_metrics(merged_stn_i_data)
#' }
get_merged_stn_i_metrics <- function(merged_stn_i_data) {

  # TODO: Modificar todo esto para que sea consistente con todo lo anterior

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the merged STN-i object and metadata
  merged_STN_i <- merged_stn_i_data$merged_STN_i
  num_networks <- merged_stn_i_data$num_networks
  network_names <- merged_stn_i_data$network_names
  problem_type <- merged_stn_i_data$problem_type
  number_of_runs <- merged_stn_i_data$number_of_runs
  best_known_solution <- merged_stn_i_data$best_known_solution

  # General metrics
  metrics$network_names <- paste(network_names, collapse = ", ")
  metrics$problem_type <- problem_type
  metrics$best_known_solution <- best_known_solution
  metrics$number_of_networks <- num_networks

  # Compute normal metrics
  metrics$nodes <- vcount(merged_STN_i)
  metrics$start_nodes <- sum(V(merged_STN_i)$Topology == "START")
  metrics$standard_nodes <- sum(V(merged_STN_i)$Topology == "STANDARD")
  metrics$end_nodes <- sum(V(merged_STN_i)$Topology == "END")
  metrics$edges <- ecount(merged_STN_i)
  metrics$best_nodes <- sum(V(merged_STN_i)$Category == "BEST")
  metrics$average_degree <- mean(degree(merged_STN_i))
  metrics$average_in_degree <- mean(degree(merged_STN_i, mode = "in"))
  metrics$average_out_degree <- mean(degree(merged_STN_i, mode = "out"))

  best_ids <- which(V(merged_STN_i)$Category == "BEST")
  if (length(best_ids) > 0) {
    metrics$average_best_in_degree <- mean(degree(merged_STN_i, v = best_ids, mode = "in"), na.rm = TRUE)
    metrics$average_best_out_degree <- mean(degree(merged_STN_i, v = best_ids, mode = "out"), na.rm = TRUE)
    metrics$best_strength_in <- sum(strength(merged_STN_i, vids = best_ids,  mode="in")) / number_of_runs
    metrics$start_best_nodes <- sum(V(merged_STN_i)$Topology == "START" & V(merged_STN_i)$Category == "BEST")
    metrics$standard_best_nodes <- sum(V(merged_STN_i)$Topology == "STANDARD" & V(merged_STN_i)$Category == "BEST")
    metrics$end_best_nodes <- sum(V(merged_STN_i)$Topology == "END" & V(merged_STN_i)$Category == "BEST")
  } else {
    metrics$average_best_in_degree <- NA
    metrics$average_best_out_degree <- NA
    metrics$best_strength_in <- NA
    metrics$start_best_nodes <- NA
    metrics$standard_best_nodes <- NA
    metrics$end_best_nodes <- NA
  }

  start_ids <- which(V(merged_STN_i)$Topology == "START")
  if (length(best_ids) > 0 & length(start_ids) > 0) {
    dist_matrix <- distances(merged_STN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
    finite_distances <- dist_matrix[is.finite(dist_matrix)]
    metrics$average_path_length <- mean(finite_distances)
    metrics$paths <- length(finite_distances)
  } else {
    metrics$average_path_length <- NA
    metrics$paths <- NA
  }
  metrics$components <- components(merged_STN_i)$no

  # Compute node categories (only merged metrics)
  metrics$shared_nodes <- sum(V(merged_STN_i)$Shared == TRUE)
  metrics$shared_regular_nodes <- sum(V(merged_STN_i)$Category == "shared-regular")
  metrics$shared_elite_nodes <- sum(V(merged_STN_i)$Category == "shared-elite")
  metrics$shared_mixed_nodes <- sum(V(merged_STN_i)$Category == "shared-mixed")
  metrics$network_regular_nodes <- sum(V(merged_STN_i)$Category == "network-regular")
  metrics$network_elite_nodes <- sum(V(merged_STN_i)$Category == "network-elite")

  # Compute percentage metrics for nodes
  if (metrics$nodes > 0) {
    metrics$best_nodes_rate <- metrics$best_nodes / metrics$nodes
    metrics$start_rate <- metrics$start_nodes / metrics$nodes
    metrics$standard_rate <- metrics$standard_nodes / metrics$nodes
    metrics$end_rate <- metrics$end_nodes / metrics$nodes
    metrics$shared_rate <- metrics$shared_nodes / metrics$nodes
    metrics$shared_regular_rate <- metrics$shared_regular_nodes / metrics$nodes
    metrics$shared_elite_rate <- metrics$shared_elite_nodes / metrics$nodes
    metrics$shared_mixed_rate <- metrics$shared_mixed_nodes / metrics$nodes
    metrics$network_regular_rate <- metrics$network_regular_nodes / metrics$nodes
    metrics$network_elite_rate <- metrics$network_elite_nodes / metrics$nodes
  } else {
    metrics$best_nodes_rate <- NA
    metrics$start_rate <- NA
    metrics$standard_rate <- NA
    metrics$end_rate <- NA
    metrics$shared_rate <- NA
    metrics$shared_regular_rate <- NA
    metrics$shared_elite_rate <- NA
    metrics$shared_mixed_rate <- NA
    metrics$network_regular_rate <- NA
    metrics$network_elite_rate <- NA
  }

  # Compute degrees for shared nodes (only merged metrics)
  shared_nodes <- V(merged_STN_i)[V(merged_STN_i)$Shared == TRUE]
  if (length(shared_nodes) > 0) {
    metrics$average_shared_in_degree <- mean(degree(merged_STN_i, v = shared_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_out_degree <- mean(degree(merged_STN_i, v = shared_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_in_degree <- NA
    metrics$average_shared_out_degree <- NA
  }

  shared_regular_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-regular"]
  if (length(shared_regular_nodes) > 0) {
    metrics$average_shared_regular_in_degree <- mean(degree(merged_STN_i, v = shared_regular_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_regular_out_degree <- mean(degree(merged_STN_i, v = shared_regular_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_regular_in_degree <- NA
    metrics$average_shared_regular_out_degree <- NA
  }

  shared_elite_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-elite"]
  if (length(shared_elite_nodes) > 0) {
    metrics$average_shared_elite_in_degree <- mean(degree(merged_STN_i, v = shared_elite_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_elite_out_degree <- mean(degree(merged_STN_i, v = shared_elite_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_elite_in_degree <- NA
    metrics$average_shared_elite_out_degree <- NA
  }

  shared_mixed_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-mixed"]
  if (length(shared_mixed_nodes) > 0) {
    metrics$average_shared_mixed_in_degree <- mean(degree(merged_STN_i, v = shared_mixed_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_mixed_out_degree <- mean(degree(merged_STN_i, v = shared_mixed_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_mixed_in_degree <- NA
    metrics$average_shared_mixed_out_degree <- NA
  }

  return(metrics)
}

#' Save merged STN-i metrics to a CSV file
#'
#' This function saves a list of merged STN-i metrics to a CSV file.
#'
#' @param merged_stn_i_metrics A list of metrics extracted from merged STN-i results, typically returned by `get_merged_stn_i_metrics()`.
#' @param output_file_path A string specifying the path to the output CSV file where the metrics will be saved.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_merged_stn_i_metrics(merged_stn_i_metrics, "output/merged_stn_i_metrics.csv")
#' }
save_merged_stn_i_metrics <- function(merged_stn_i_metrics, output_file_path) {
  # Ensure the file name ends in .csv
  if (!grepl("\\.csv$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".csv")
  }

  # Convert the named list to a data frame: names as columns, values as first row
  metrics_df <- as.data.frame(merged_stn_i_metrics, stringsAsFactors = FALSE)
  # Ensure it's a single row
  if (is.list(metrics_df) && nrow(metrics_df) != 1) {
    metrics_df <- as.data.frame(t(unlist(merged_stn_i_metrics)), stringsAsFactors = FALSE)
  }

  # Save the data frame to a CSV file with semicolon separator and header
  write.table(
    metrics_df,
    file = output_file_path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    dec = "."
  )

  message(paste("Merged STN-i metrics saved to:", output_file_path))
}

# nolint end
