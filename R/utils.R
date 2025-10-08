# nolint start

#' Parse command line arguments
#' 
#' This function parses command line arguments in the format `--key=value` and returns a named list of parameters.
#' 
#' @param args A character vector of command line arguments.
#' 
#' @return A named list where each key corresponds to an argument name and its value is the argument value.
#' 
#' @examples
#' \dontrun{
#' args <- c("--input=path/to/input.txt", "--output=path/to/output.txt")
#' parsed_args <- parse_arguments(args)
#' print(parsed_args)
#' }
parse_arguments <- function(args) {
  parsed <- list()
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=")[[1]]
      if (length(parts) == 2) {
        parsed[[parts[1]]] <- parts[2]
      } else {
        stop(paste("Invalid argument format:", arg), call. = FALSE)
      }
    }
  }
  return(parsed)
}

#' Create a STN-i from a trace file
#' 
#' This function reads a trace file and creates a STN-i (Solution Trace Network) graph.
#' 
#' @param input_file A string specifying the path to the input trace file.
#' @param problem_type A string specifying the type of problem, either "min" for minimization or "max" for maximization. Default is "min".
#' @param best_known_solution A numeric value representing the best known solution for the problem. Default is NA, which means it will be calculated from the data.
#' 
#' @param number_of_runs An integer specifying the number of runs to consider from the trace file. Default is NA, which means it will use the maximum run number found in the data.
#' @param separator A string specifying the separator used in the input file. Default is "".
#' @param network_name A string specifying the name of the STN-i network. Default is "STN_i".
#' 
#' @return A list containing the STN-i graph, problem type, best known solution, number of runs, and summaries of elite and type origins.
#' 
#' @examples
#' \dontrun{
#' stn_i_result <- stn_i_create("path/to/trace_file.txt", problem_type = "min", best_known_solution = 0.5, network_name = "My_STN_i")
#' }
stn_i_create <- function(input_file, problem_type = "min", best_known_solution = NA, number_of_runs = NA, separator = "", network_name = "STN_i") {
  # Load the input file data
  trace_all <- read.table(input_file, header=T, sep = separator, colClasses=c("integer", "logical", "numeric", "character", "character", "character", "character", "character", "integer", "numeric", "character", "character", "character", "character", "character", "integer"), stringsAsFactors = F)

  # Check if the number of runs is specified, if not, use the maximum run number in the data
  if (is.na(number_of_runs)) {
    number_of_runs <- max(trace_all$Run, na.rm = TRUE)
  } else {
    if (number_of_runs < 1) {
      stop("number_of_runs must be a positive integer.", call. = FALSE)
    }
  }

  # Filter the trace data to include only the specified number of runs
  trace_all <- trace_all[trace_all$Run <= number_of_runs,]

  # Initialize lists to store nodes and edges for each run
  lnodes <- vector("list", number_of_runs)
  ledges <- vector("list", number_of_runs)

  # Initialize a summary data frame for elite and type classifications
  elite_values <- c("ELITE", "REGULAR")
  type_values <- c("START", "STANDARD", "END")
  classification_summary <- expand.grid(
    Elite = elite_values,
    Type = type_values,
    Origin_Elite = elite_values,
    Origin_Type = type_values,
    stringsAsFactors = FALSE
  )

  # Add a Count column to the classification summary (initially set to 0)
  classification_summary$Count <- 0

  # Initialize a list to store survival rates for each configuration for all runs
  configurations_survival_rates <- list()

  # Combine all runs in a single network
  for (i in (1:number_of_runs)) {
    # Take by run and remove first column run number
    trace <- trace_all[which(trace_all$Run==i),c(-1)]
    colnames(trace) <- c("path",
                        "fit1", "node1", "elite1", "origin_elite1", "type1", "origin_type1", "iteration1",
                        "fit2", "node2", "elite2", "origin_elite2", "type2", "origin_type2", "iteration2")

    # Initialize lists to store nodes and edges for the current run
    lnodes_run <- list()

    for (j in 1:nrow(trace)) {
      # Add only the right node (node2) to the location trend
      # because it's to avoid over-representing the left node (node1) in the network
      lnodes_run[[length(lnodes_run) + 1]] <- data.frame(
        Node = trace$node2[j],
        Fitness = trace$fit2[j],
        Elite = trace$elite2[j],
        Type = trace$type2[j],
        stringsAsFactors = FALSE
      )

      # Search for the classification_summary
      idx <- with(classification_summary, which(Elite == trace$elite2[j] & Type == trace$type2[j] & Origin_Elite == trace$origin_elite2[j] & Origin_Type == trace$origin_type2[j]))
      if (length(idx) == 1) {
        classification_summary$Count[idx] <- classification_summary$Count[idx] + 1
      }
    }

    # Remove self loops with path information
    lnodes[[i]] <- do.call(rbind, lnodes_run)
    ledges[[i]] <- trace[trace$path == TRUE, c("node1", "node2")]


    # Initalize a list to store survival rates for each configuration
    max_iter <- max(trace$iteration2)
    run_survival_rates <- numeric(max_iter)

    for (iter in 1:max_iter) {
      iteration_rows <- trace[trace$iteration2 == iter, ]
      total_iteration_nodes <- nrow(iteration_rows)
      elite_iteration_nodes <- sum(iteration_rows$origin_elite2 == "ELITE")
      if (total_iteration_nodes == 0) {
        run_survival_rates[iter] <- NA
      } else {
        run_survival_rates[iter] <- elite_iteration_nodes / total_iteration_nodes
      }
    }

    # Store the survival rates for the current run
    configurations_survival_rates[[i]] <- run_survival_rates
  }

  # Combine the list of nodes into one dataframe and
  # group by (Node, Fitness) to identify unique nodes and count them
  nodes <- ddply((do.call("rbind", lnodes)), .(Node, Fitness, Elite, Type), nrow) # Cambiar forma de contar nodos
  colnames(nodes) <- c("Node", "Fitness", "Elite", "Type", "Count")

  # Delete duplicates from nodes dataframe based on Node
  nodesu <- nodes[!duplicated(nodes$Node), ]

  # Combine the list of edges into one dataframe and
  # group by (node1, node2) to identify unique edges and count them
  edges <- ddply(do.call("rbind", ledges), .(node1, node2), nrow)
  colnames(edges) <- c("Start", "End", "weight")

  # Create STN_i graph and remove self loops
  STN_i <- graph_from_data_frame(d = edges, directed = T, vertices = nodesu)
  STN_i <- simplify(STN_i, remove.multiple = FALSE, remove.loops = TRUE)

  # Obtain the fitness values from graph
  fitness_vals <- V(STN_i)$Fitness

  # If best known solution is not provided, calculate it based on the problem type
  if (is.na(best_known_solution)) {
    best_known_solution <- if (problem_type == "min") {
      min(fitness_vals, na.rm = TRUE)
    } else {
      max(fitness_vals, na.rm = TRUE)
    }
  }

  # Obtain the topology ids
  standard_ids <- which(V(STN_i)$Type == "STANDARD")
  start_ids <- which(V(STN_i)$Type == "START")
  end_ids <- which(V(STN_i)$Type == "END")

  # Set the topology types: START, STANDARD and END
  V(STN_i)$Topology <- NA
  V(STN_i)[standard_ids]$Topology <- "STANDARD"
  V(STN_i)[start_ids]$Topology <- "START"
  V(STN_i)[end_ids]$Topology <- "END"

  # Obtain the quality ids (REGULAR, ELITE and BEST)
  regular_ids <- which(V(STN_i)$Elite == "REGULAR")
  elite_ids <- which(V(STN_i)$Elite == "ELITE")
  best_ids <- if (problem_type == "min") {
    which(fitness_vals <= best_known_solution)
  } else {
    which(fitness_vals >= best_known_solution)
  }

  # Set the quality types: REGULAR, ELITE and BEST
  V(STN_i)[regular_ids]$Quality <- "REGULAR"
  V(STN_i)[elite_ids]$Quality <- "ELITE"
  V(STN_i)[best_ids]$Quality <- "BEST"

  # Return all data without save
  return(list(
    STN_i = STN_i,
    network_name = network_name,
    problem_type = problem_type,
    best_known_solution = best_known_solution,
    number_of_runs = number_of_runs,
    classification_summary = classification_summary,
    configurations_survival_rates = configurations_survival_rates
  ))
}

#' Save the STN_i-i data to a R file
#'
#' This function saves the STN_i-i data to a specified output folder with a given file name.
#'
#' @param stn_i_result The result of the STN_i-i analysis, typically an object containing the STN_i graph and related data.
#' @param output_file_path A string specifying the path to the output folder where the RData file will be saved.
#'
#' @return None. The function writes the file to disk.
#'
#' @examples
#' \dontrun{
#' save_stn_i_data(stn_i_result, "output/", "custom_name.RData")
#' }
save_stn_i_data <- function(stn_i_result, output_file_path) {

  # Save the STN-i result as an RData file
  save(stn_i_result, file = output_file_path)

  cat("General STN-i summary:\n")

  cat("Problem Type:", stn_i_result$problem_type, "\n")
  cat("Number of Runs:", stn_i_result$number_of_runs, "\n")
  cat("Best Known Solution:", stn_i_result$best_known_solution, "\n")
  cat("Number of Nodes:", vcount(stn_i_result$STN_i), "\n")
  cat("Number of Edges:", ecount(stn_i_result$STN_i), "\n")
  cat("Number of Elite Nodes:", sum(V(stn_i_result$STN_i)$Quality == "ELITE"), "\n")
  cat("Number of Regular Nodes:", sum(V(stn_i_result$STN_i)$Quality == "REGULAR"), "\n")
  cat("Number of Best Nodes:", sum(V(stn_i_result$STN_i)$Quality == "BEST"), "\n")

  # Print the classification summary
  cat("\nClassification Summary:\n")
  print(stn_i_result$classification_summary)

  # Print the survival rates for each configuration
  cat("\nConfigurations Survival Rates:\n")
  for (i in seq_along(stn_i_result$configurations_survival_rates)) {
    cat(paste("Run", i, ":", paste(stn_i_result$configurations_survival_rates[[i]], collapse = ", "), "\n"))
  }

  # Print a message indicating where the file was saved
  message(paste("STN-i data saved to:", output_file_path))
}

#' Load STN-i data from a file
#' 
#' This function loads STN-i data from a specified input file and validates its structure.
#' 
#' @param input_file A string specifying the path to the input file containing the STN-i data.
#' 
#' @return A list containing the loaded STN-i data, including the graph, problem type, best known solution, number of runs, and summaries.
#' 
#' @examples
#' \dontrun{
#' stn_i_data <- get_stn_i_data("path/to/stn_i_file.RData")
#' }
get_stn_i_data <- function(input_file) {
  # Check if file exists
  if (!file.exists(input_file)) {
    stop(paste("Input file does not exist:", input_file), call. = FALSE)
  }

  # Load the object and retrieve its name
  loaded_name <- load(input_file)
  stn_i_result <- get(loaded_name)

  # Validate structure
  expected_fields <- c(
    "STN_i",
    "network_name",
    "problem_type",
    "best_known_solution",
    "number_of_runs",
    "classification_summary",
    "configurations_survival_rates"
  )

  if (!is.list(stn_i_result) || !all(expected_fields %in% names(stn_i_result))) {
    stop("The loaded file does not contain a valid STN-i result structure.", call. = FALSE)
  }

  return(stn_i_result)
}

#' Get palette colors for STN-i visualization
#'
#' This function returns a list of colors for nodes and edges based on the specified palette option.
#'
#' @param palette A numeric value specifying the color palette option. Default is 1.
#'
#' @return A list containing node and edge colors for the specified palette.
#'
#' @examples
#' \dontrun{
#' colors <- get_stn_i_palette_colors(2)
#' print(colors)
#' }
get_stn_i_palette_colors <- function(palette = 1) {
  palette_colors <- switch(as.character(palette),
    "1" = list(
      node = list(regular = "gray70", elite = "orange", best = "red", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "2" = list(
      node = list(regular = "lightblue", elite = "blue", best = "darkblue", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "3" = list(
      node = list(regular = "lightgreen", elite = "green", best = "darkgreen", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "4" = list(
      node = list(regular = "lightpink", elite = "pink", best = "darkred", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "5" = list(
      node = list(regular = "lightgray", elite = "gray", best = "black", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4, 5.")
  )

  return(palette_colors)
}

#' Get layout data for a given graph object
#'
#' This function retrieves layout data for a given graph object based on the specified layout type.
#'
#' @param g A graph object of class igraph representing the STN-i.
#' @param layout A string specifying the layout type. Options include:
#'   "fr" (Fruchterman-Reingold),
#'   "kk" (Kamada-Kawai),
#'   "circle" (circular layout),
#'   "grid" (nodes on a grid),
#'   "sphere" (nodes on a sphere),
#'   "drl" (DrL force-directed layout),
#'   "graphopt" (force-directed using physics model),
#'   "random" (random placement).
#'   Default is "fr".
#'
#' These layouts were selected for their stability and interpretability across different graph sizes.
#'
#' @return A list containing the layout title, coordinates, and layout type.
#'
#' @examples
#' \dontrun{
#' layout_data <- get_layout_data(STN_i, layout = "kk")
#' }
get_layout_data <- function(g, layout = "fr") {
  layout_data <- switch(layout,
    "fr" = list(
      title = "Fruchterman-Reingold Layout",
      coords = layout_with_fr(g),
      layout_type = "fr"
    ),
    "kk" = list(
      title = "Kamada-Kawai Layout",
      coords = layout_with_kk(g),
      layout_type = "kk"
    ),
    "circle" = list(
      title = "Circle Layout",
      coords = layout_in_circle(g),
      layout_type = "circle"
    ),
    "grid" = list(
      title = "Grid Layout",
      coords = layout_on_grid(g),
      layout_type = "grid"
    ),
    "sphere" = list(
      title = "Sphere Layout",
      coords = layout_on_sphere(g),
      layout_type = "sphere"
    ),
    "drl" = list(
      title = "DrL Layout",
      coords = layout_with_drl(g),
      layout_type = "drl"
    ),
    "graphopt" = list(
      title = "Graphopt Layout",
      coords = layout_with_graphopt(g),
      layout_type = "graphopt"
    ),
    "random" = list(
      title = "Random Layout",
      coords = layout_randomly(g),
      layout_type = "random"
    ),
    stop("Invalid layout option. Choose from: fr, kk, circle, grid, sphere, drl, graphopt, random.")
  )

  return(layout_data)
}

#' Decorate nodes and edges an STN for visualising a single algorithm STN
#' 
#' This function decorates the nodes and edges of an STN graph object for visualization purposes.
#' 
#' @param STN_i Graph object
#' @param problem_type Boolean indicating minimisation or not
#' @param show_regular Boolean indicating whether to show regular nodes
#' @param show_start_regular Boolean indicating whether to show start regular nodes
#' @param palette_colors List of colors for the different node and edge types
#' 
#' @return Decorated STN-i graph object
#' 
#' @examples
#' \dontrun{
#' STN_i <- stn_i_decorate(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1))
#' }
stn_i_decorate <- function(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, palette_colors) {

  # Filter vertices according to settings
  to_remove <- c()
  if (!show_regular) {
    to_remove <- c(to_remove, V(STN_i)[Quality == "REGULAR"]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(STN_i)[Topology == "START" & Quality == "REGULAR"]$name)
  }

  # Remove duplicates before deletion
  to_remove <- unique(to_remove)
  STN_i <- delete_vertices(STN_i, to_remove)

  # Retrieve edge list and corresponding fitness values
  edge_list <- as_edgelist(STN_i)
  node_fitness <- V(STN_i)$Fitness
  node_names <- V(STN_i)$name

  # Extract fitness of origin and destination nodes for each edge
  fitness_from <- node_fitness[match(edge_list[, 1], node_names)]
  fitness_to   <- node_fitness[match(edge_list[, 2], node_names)]

  # Assign edge types based on fitness comparison
  if (problem_type == "min") {
    E(STN_i)[fitness_to < fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to > fitness_from]$Type <- "WORSENING"
  } else {
    E(STN_i)[fitness_to > fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to < fitness_from]$Type <- "WORSENING"
  }
  E(STN_i)[fitness_to == fitness_from]$Type <- "EQUAL"

  # Assign edge colors based on type
  E(STN_i)$color[E(STN_i)$Type == "IMPROVING"] <- palette_colors$edge$improving
  E(STN_i)$color[E(STN_i)$Type == "WORSENING"] <- palette_colors$edge$worsening
  E(STN_i)$color[E(STN_i)$Type == "EQUAL"]     <- palette_colors$edge$equal

  # Set edge width proportional to number of visits (weight)
  E(STN_i)$width <- E(STN_i)$weight

  # Assign node shapes based on Topology type
  V(STN_i)[V(STN_i)$Topology == "STANDARD"]$shape <- "circle"
  V(STN_i)[V(STN_i)$Topology == "START"]$shape <- "square"
  V(STN_i)[V(STN_i)$Topology == "END"]$shape <- "triangle"

  # Assign node colors based on Quality
  V(STN_i)[V(STN_i)$Quality == "REGULAR"]$color <- palette_colors$node$regular
  V(STN_i)[V(STN_i)$Quality == "ELITE"]$color <- palette_colors$node$elite
  V(STN_i)[V(STN_i)$Quality == "BEST"]$color <- palette_colors$node$best

  # Set node size proportional to in-degree (number of incoming visits)
  V(STN_i)$size <- strength(STN_i, mode = "in") + 1

  # Slightly increase the size of BEST nodes for visibility
  V(STN_i)[V(STN_i)$Quality == "BEST"]$size <- V(STN_i)[V(STN_i)$Quality == "BEST"]$size + 0.5

  return(STN_i)
}

#' Function to create a plot for STN-i data
#'
#' This function processes the input STN-i file, decorates the STN object, and prepares layout data for plotting.
#'
#' @param input_file Path to the input STN-i file.
#' @param show_regular Boolean indicating whether to show regular nodes in the plot (default is TRUE).
#' @param show_start_regular Boolean indicating whether to show start regular nodes in the plot (default is TRUE).
#' @param palette_colors List of colors for the different node and edge types.
#' @param zoom_quantile Numeric value between 0 and 1 to define a zoom level for the plot (default is NA, meaning no zoom).
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#' result <- stn_i_plot_create("path/to/stn_i_file.RData", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1), zoom_quantile = 0.5)
#' }
stn_i_plot_create <- function(input_file, show_regular = TRUE, show_start_regular = TRUE, palette_colors, zoom_quantile = NA) {
  # Load the STN-i data
  stn_i_result <- get_stn_i_data(input_file)

  # Obtain the STN object and problem type
  STN_i <- stn_i_result$STN_i
  problem_type <- stn_i_result$problem_type

  # Decorate the STN-i object
  STN_i <- stn_i_decorate(STN_i, problem_type, show_regular, show_start_regular, palette_colors)

  # If zooming is enabled, extract subgraph
  if (!is.na(zoom_quantile) && zoom_quantile > 0 && zoom_quantile < 1) {
    STN_i <- get_zoomed_graph(STN_i, zoom_quantile, problem_type)
  }

  # Return everything needed for external plotting
  return(STN_i)
}

#' Save a plot of the STN-i graph
#' 
#' This function saves a plot of the STN-i graph to a specified PDF file, applying custom shapes and colors for nodes and edges.
#' 
#' @param output_file_path A string specifying the path to the output PDF file.
#' @param STN_i A graph object of class igraph representing the STN-i.
#' @param layout_data A list containing layout information, including coordinates and title for the plot.
#' @param palette_colors A list of colors for nodes and edges, as returned by `get_stn_i_palette_colors()`.
#' @param nsizef A numeric factor to adjust node sizes (default is 1).
#' @param ewidthf A numeric factor to adjust edge widths (default is 0.5).
#' @param asize A numeric value for the arrow size of edges (default is 0.3).
#' @param ecurv A numeric value for the curvature of edges (default is 0.3).
#' 
#' @return None. The function saves the plot to the specified PDF file.
#' 
#' @examples
#' \dontrun{
#' save_stn_i_plot("output/stn_i_plot.pdf", STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3)
#' }
save_stn_i_plot <- function(output_file_path, STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3) {
  # Ensure the file name ends in .pdf
  if (!grepl("\\.pdf$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".pdf")
  }

  # Define triangle shape
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
  # Register triangle as a custom shape (reuses circle clipping)
  add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

  # Define legend components based on the palette
  legend.txt <- c("Start", "Standard", "End", "Regular", "Elite", "Best", "Improve", "Equal", "Worse")
  legend.col <- c(
    palette_colors$node$shapes, palette_colors$node$shapes, palette_colors$node$shapes,
    palette_colors$node$regular, palette_colors$node$elite, palette_colors$node$best,
    palette_colors$edge$improving, palette_colors$edge$equal, palette_colors$edge$worsening
  )
  legend.shape <- c(15, 21, 17, 21, 21, 21, NA, NA, NA)
  legend.lty <- c(NA, NA, NA, NA, NA, NA, 1, 1, 1)

  # Open PDF device
  pdf(output_file_path)

  # Compute adjusted node sizes
  maxns <- max(V(STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(STN_i)$size + 1
  } else {
    nsize <- nsizef * V(STN_i)$size
  }

  ewidth <- ewidthf * E(STN_i)$width
  title <- paste(layout_data$title, "Nodes:", vcount(STN_i), "Edges:", ecount(STN_i), "Comp:", components(STN_i)$no)

  # Plot graph
  plot(STN_i, layout = layout_data$coords, vertex.label = "", vertex.size = nsize, main = title, edge.width = ewidth, edge.arrow.size = asize, edge.curved = ecurv)

  # Add legend
  legend("topleft", legend.txt, pch = legend.shape, col = legend.col, pt.bg = legend.col, lty = legend.lty, cex = 0.7, pt.cex = 1.35, bty = "n")

  # Close PDF device
  dev.off()

  message("STN-i plot saved successfully to: ", output_file_path)
}

#' Load and merge multiple STN-i data files from a specified folder.
#'
#' This function reads multiple `.RData` files containing STN-i graph objects,
#' merges them into a single graph, and extracts relevant metadata such as
#' algorithm names, problem type, best known solutions, and number of runs.
#'
#' @param input_folder The folder containing the `.RData` files.
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
  # Check if the input folder contains at least 2 .RData files
  files <- list.files(input_folder, pattern = "\\.RData$", full.names = TRUE)
  if (length(files) < 2) {
    stop("At least 2 .RData files required to merge STN-i data.")
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
merge_stns_i_data <- function(stns_i_data, criteria = "mean") {

  # Check if the criteria is valid
  if (!criteria %in% c("min", "max", "mean", "median", "mode")) {
    stop("Invalid criteria. Choose from: 'min', 'max', 'mean', 'median', 'mode'.")
  }

  snts_i <- stns_i_data$graphs
  num_networks <- length(snts_i)
  number_of_runs <- sum(stns_i_data$number_of_runs)
  network_names <- stns_i_data$names
  problem_type <- stns_i_data$problem_type

  # Collect all nodes
  node_df_list <- list()
  for (i in seq_along(snts_i)) {
    g <- snts_i[[i]]
    node_df <- data.frame(
      Node = V(g)$name,
      Fitness = V(g)$Fitness,
      Topology = V(g)$Topology,
      Count = V(g)$Count,
      Quality = V(g)$Quality,
      Network = V(g)$Network,
      stringsAsFactors = FALSE
    )
    node_df$graph_id <- i
    node_df_list[[i]] <- node_df
  }
  nodes_all <- bind_rows(node_df_list)

  # Merge nodes
  merged_nodes <- nodes_all %>%
    group_by(Node) %>%
    summarise(
      Fitness = {
        vals <- Fitness[!is.na(Fitness)]
        if (length(vals) == 0) NA else switch(criteria,
          "min" = min(vals),
          "max" = max(vals),
          "mean" = mean(vals),
          "median" = median(vals),
          "mode" = as.numeric(names(sort(table(vals), decreasing = TRUE)[1]))
        )
      },
      Topology = {
        types <- unique(na.omit(Topology[Topology != ""]))
        if ("END" %in% types) "END" else if ("START" %in% types) "START" else "STANDARD"
      },
      Count = sum(Count, na.rm = TRUE),
      Quality = paste(Quality[!is.na(Quality) & Quality != ""], collapse = ""),
      Network = paste(Network[!is.na(Network) & Network != ""], collapse = ""),
      n_networks = n(),
      .groups = "drop"
    )

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
#' 
#'  @return NULL
#' 
#' @examples
#' \dontrun{
#' save_merged_stn_i_data(merged_STN_i, "path/to/output.RData")
#' }
save_merged_stn_i_data <- function(merged_stn_i_data, output_file_path) {
  # Ensure the file name ends in .RData
  if (!grepl("\\.RData$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".RData")
  }

  # Print summary metrics before saving
  g <- merged_stn_i_data$merged_STN_i
  cat("\nSaving merged STN-i with summary metrics:\n")
  cat(" - Total nodes:", vcount(g), "\n")
  cat(" - Total edges:", ecount(g), "\n")
  cat(" - Shared nodes:", sum(V(g)$Shared), "\n")
  cat(" - shared-elite:", sum(V(g)$Category == "shared-elite"), "\n")
  cat(" - shared-regular:", sum(V(g)$Category == "shared-regular"), "\n")
  cat(" - shared-mixed:", sum(V(g)$Category == "shared-mixed"), "\n")
  cat(" - network-elite:", sum(V(g)$Category == "network-elite"), "\n")
  cat(" - network-regular:", sum(V(g)$Category == "network-regular"), "\n")

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
#'  get_merged_stn_i_data("path/to/merged_stn_i_file.RData")
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

  # Set node size proportional to in-degree (number of incoming visits)
  V(merged_STN_i)$size <- strength(merged_STN_i, mode = "in") + 1

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

#' Get zoomed subgraph of STN-i based on fitness quantile
#' 
#' This function extracts a subgraph from the given STN-i graph where the vertex fitness values are
#' either below or above a quantile threshold depending on the problem type.
#' 
#' @param graph An igraph object representing the STN-i graph or merged STN-i graph.
#' @param quantile_value A numeric value between 0 and 1 indicating the quantile threshold.
#' @param problem_type Either "min" (default) or "max" to define optimization direction.
#' 
#' @return An igraph object representing the zoomed subgraph.
#' 
#' @examples
#' zoomed_graph <- get_zoomed_graph(my_graph, quantile_value = 0.25, problem_type = "min")
get_zoomed_graph <- function(graph, quantile_value = 0.25, problem_type = "min") {

  if (!"Fitness" %in% vertex_attr_names(graph)) {
    stop("Graph must have a 'Fitness' vertex attribute for zooming.")
  }

  if (!(problem_type %in% c("min", "max"))) {
    stop("Invalid problem_type. Must be 'min' or 'max'.")
  }

  fitness_values <- V(graph)$Fitness
  threshold <- quantile(fitness_values, probs = quantile_value, na.rm = TRUE)

  # Select nodes based on the threshold and problem type
  if (problem_type == "min") {
    selected_nodes <- V(graph)[fitness_values <= threshold]
  } else {
    selected_nodes <- V(graph)[fitness_values >= threshold]
  }

  subgraph <- induced_subgraph(graph, selected_nodes)
  subgraph <- delete_vertices(subgraph, degree(subgraph) == 0)
  return(subgraph)
}

#' Get metrics from a single STN-i result
#'
#' This function extracts various metrics from a single STN-i result object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @param stn_i_result A list containing the STN-i result, including the graph object and metadata.
#'
#' @return A list of metrics extracted from the STN-i result, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @examples
#'
#' \dontrun{
#' stn_i_result <- get_stn_i_data("path/to/stn_i_file.RData")
#' metrics <- get_stn_i_metrics(stn_i_result)
#' }
get_stn_i_metrics <- function(stn_i_result) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the STN-i and metadata
  STN_i <- stn_i_result$STN_i
  network_name <- stn_i_result$network_name
  problem_type <- stn_i_result$problem_type
  best_known_solution <- stn_i_result$best_known_solution
  number_of_runs <- stn_i_result$number_of_runs
  survival_rates <- stn_i_result$survival_rates
  classification_summary <- stn_i_result$classification_summary
  total_configurations <- sum(classification_summary$Count)
  configurations_survival_rates <- stn_i_result$configurations_survival_rates

  # Retrieve edge list and corresponding fitness values
  edge_list <- as_edgelist(STN_i)
  node_fitness <- V(STN_i)$Fitness
  node_names <- V(STN_i)$name

  # Extract fitness of origin and destination nodes for each edge
  fitness_from <- node_fitness[match(edge_list[, 1], node_names)]
  fitness_to   <- node_fitness[match(edge_list[, 2], node_names)]

  # Assign edge types based on fitness comparison
  if (problem_type == "min") {
    E(STN_i)[fitness_to < fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to > fitness_from]$Type <- "WORSENING"
  } else {
    E(STN_i)[fitness_to > fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to < fitness_from]$Type <- "WORSENING"
  }
  E(STN_i)[fitness_to == fitness_from]$Type <- "EQUAL"

  # Assign STN-i direct data
  metrics$network_name <- network_name
  metrics$problem_type <- problem_type
  metrics$best_known_solution <- best_known_solution
  metrics$number_of_runs <- number_of_runs

  # Compute nodes quantity metrics
  metrics$nodes <- vcount(STN_i)
  metrics$regular_nodes <- sum(V(STN_i)$Quality == "REGULAR")
  metrics$elite_nodes <- sum(V(STN_i)$Quality == "ELITE")
  metrics$start_nodes <- sum(V(STN_i)$Topology == "START")
  metrics$standard_nodes <- sum(V(STN_i)$Topology == "STANDARD")
  metrics$end_nodes <- sum(V(STN_i)$Topology == "END")

  # Compute edges quantity metrics
  metrics$edges <- ecount(STN_i)
  metrics$worsening_edges <- sum(E(STN_i)$Type == "WORSENING")
  metrics$equal_edges <- sum(E(STN_i)$Type == "EQUAL")
  metrics$improving_edges <- sum(E(STN_i)$Type == "IMPROVING")
  metrics$edges_tendency <- metrics$improving_edges / (metrics$improving_edges + metrics$worsening_edges)

  # Compute initialization process metrics
  metrics$regular_start_nodes <- sum(V(STN_i)$Topology == "START" & V(STN_i)$Quality == "REGULAR")
  metrics$elite_start_nodes <- sum(V(STN_i)$Topology == "START" & V(STN_i)$Quality == "ELITE")
  start_nodes <- sum(classification_summary$Count[classification_summary$Type == "START"])
  if (start_nodes > 0) {
    metrics$regular_start_configuration_rate <- sum(classification_summary$Count[classification_summary$Origin_Elite == "REGULAR" & classification_summary$Type == "START"]) / start_nodes
    metrics$elite_start_configuration_rate <- sum(classification_summary$Count[classification_summary$Origin_Elite == "ELITE" & classification_summary$Type == "START"]) / start_nodes
  } else {
    metrics$regular_start_configuration_rate <- NA
    metrics$elite_start_configuration_rate <- NA
  }

  # Compute global configuration distribution metrics
  if (total_configurations > 0) {
    metrics$regular_configuration_rate <- sum(classification_summary$Count[classification_summary$Origin_Elite == "REGULAR"]) / total_configurations
    metrics$elite_configuration_rate <- sum(classification_summary$Count[classification_summary$Origin_Elite == "ELITE"]) / total_configurations
  } else {
    metrics$regular_configuration_rate <- NA
    metrics$elite_configuration_rate <- NA
  }

  # Compute structure metrics
  metrics$best_nodes <- sum(V(STN_i)$Quality == "BEST")
  metrics$average_degree <- mean(degree(STN_i))
  metrics$average_in_degree <- mean(degree(STN_i, mode = "in"))
  metrics$average_out_degree <- mean(degree(STN_i, mode = "out"))

  regular_nodes <- V(STN_i)[V(STN_i)$Quality == "REGULAR"]
  if (length(regular_nodes) > 0) {
    metrics$average_regular_in_degree  <- mean(degree(STN_i, v = regular_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_regular_out_degree <- mean(degree(STN_i, v = regular_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_regular_in_degree  <- NA
    metrics$average_regular_out_degree <- NA
  }

  elite_nodes <- V(STN_i)[V(STN_i)$Quality == "ELITE"]
  if (length(elite_nodes) > 0) {
    metrics$average_elite_in_degree  <- mean(degree(STN_i, v = elite_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_elite_out_degree <- mean(degree(STN_i, v = elite_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_elite_in_degree  <- NA
    metrics$average_elite_out_degree <- NA
  }

  best_ids <- which(V(STN_i)$Quality == "BEST")
  if (length(best_ids) > 0) {
    metrics$average_best_in_degree <- mean(degree(STN_i, v = best_ids, mode = "in"), na.rm = TRUE)
    metrics$average_best_out_degree <- mean(degree(STN_i, v = best_ids, mode = "out"), na.rm = TRUE)
    metrics$best_strength_in <- sum(strength(STN_i, vids = best_ids,  mode="in")) / number_of_runs
    metrics$start_best_nodes <- sum(V(STN_i)$Topology == "START" & V(STN_i)$Quality == "BEST")
    metrics$standard_best_nodes <- sum(V(STN_i)$Topology == "STANDARD" & V(STN_i)$Quality == "BEST")
    metrics$end_best_nodes <- sum(V(STN_i)$Topology == "END" & V(STN_i)$Quality == "BEST")
  } else {
    metrics$average_best_in_degree <- NA
    metrics$average_best_out_degree <- NA
    metrics$best_strength_in <- NA
    metrics$start_best_nodes <- NA
    metrics$standard_best_nodes <- NA
    metrics$end_best_nodes <- NA
  }

  start_ids <- which(V(STN_i)$Topology == "START")
  if (length(best_ids) > 0 & length(start_ids) > 0) {
    dist_matrix <- distances(STN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
    finite_distances <- dist_matrix[is.finite(dist_matrix)]
    metrics$average_path_length <- mean(finite_distances)
    metrics$paths <- length(finite_distances)
  } else {
    metrics$average_path_length <- NA
    metrics$paths <- NA
  }
  metrics$components <- components(STN_i)$no

  # Compute supervivence metrics
  run_averages <- sapply(configurations_survival_rates, function(run) mean(run, na.rm = TRUE))
  metrics$average_configurations_survival_rates <- mean(run_averages, na.rm = TRUE)

  # Compute percentage metrics for nodes
  if (metrics$nodes > 0) {
    metrics$best_nodes_rate <- metrics$best_nodes / metrics$nodes
    metrics$regular_nodes_rate <- metrics$regular_nodes / metrics$nodes
    metrics$elite_nodes_rate <- metrics$elite_nodes / metrics$nodes
    metrics$start_nodes_rate <- metrics$start_nodes / metrics$nodes
    metrics$standard_nodes_rate <- metrics$standard_nodes / metrics$nodes
    metrics$end_nodes_rate <- metrics$end_nodes / metrics$nodes
  } else {
    metrics$best_nodes_rate <- NA
    metrics$regular_nodes_rate <- NA
    metrics$elite_nodes_rate <- NA
    metrics$start_nodes_rate <- NA
    metrics$standard_nodes_rate <- NA
    metrics$end_nodes_rate <- NA
  }

  # Compute percentage metrics for edges
  if (metrics$edges > 0) {
    metrics$worsening_edges_rate <- metrics$worsening_edges / metrics$edges
    metrics$equal_edges_rate <- metrics$equal_edges / metrics$edges
    metrics$improving_edges_rate <- metrics$improving_edges / metrics$edges
  } else {
    metrics$worsening_edges_rate <- NA
    metrics$equal_edges_rate <- NA
    metrics$improving_edges_rate <- NA
  }

  return(metrics)
}

#' Save STN-i metrics to a CSV file
#'
#' This function saves a list of STN-i metrics to a CSV file.
#'
#' @param stn_i_metrics A list of metrics extracted from STN-i results, typically returned by `get_stn_i_metrics()`.
#' @param output_file_path A string specifying the path to the output CSV file where the metrics will be saved.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_stn_i_metrics(stn_i_metrics, "path/to/stn_i_metrics.csv")
#' }
save_stn_i_metrics <- function(stn_i_metrics, output_file_path) {
  # Ensure the file name ends in .csv
  if (!grepl("\\.csv$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".csv")
  }

  # Convert the named list to a data frame: names as columns, values as first row
  metrics_df <- as.data.frame(stn_i_metrics, stringsAsFactors = FALSE)
  # Ensure it's a single row
  if (is.list(metrics_df) && nrow(metrics_df) != 1) {
    metrics_df <- as.data.frame(t(unlist(stn_i_metrics)), stringsAsFactors = FALSE)
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

  message(paste("STN-i metrics saved to:", output_file_path))
}

#' Get metrics from a merged STN-i object
#'
#' This function computes various metrics from a merged STN-i graph object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#'
#' @return A list of metrics extracted from the merged STN-i object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @examples
#' \dontrun{
#' merged_stn_i_data <- get_merged_stn_i_data("path/to/merged_stn_i_file.RData")
#' metrics <- get_merged_stn_i_metrics(merged_stn_i_data)
#' }
get_merged_stn_i_metrics <- function(merged_stn_i_data) {

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
