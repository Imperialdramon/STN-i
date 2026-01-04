# nolint start

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
      node = list(regular = "black", elite = "orange", best = "red", shapes = "black"),
      edge = list(improving = "gray50",
      equal = rgb(0, 0, 250, max = 255, alpha = 180),
      worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "2" = list(
      node = list(regular = "lightblue", elite = "blue", best = "darkblue", shapes = "black"),
      edge = list(improving = "gray50",
      equal = rgb(0, 0, 250, max = 255, alpha = 180),
      worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "3" = list(
      node = list(regular = "lightgreen", elite = "green", best = "darkgreen", shapes = "black"),
      edge = list(improving = "gray50",
      equal = rgb(0, 0, 250, max = 255, alpha = 180),
      worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "4" = list(
      node = list(regular = "lightpink", elite = "pink", best = "darkred", shapes = "black"),
      edge = list(improving = "gray50",
      equal = rgb(0, 0, 250, max = 255, alpha = 180),
      worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "5" = list(
      node = list(regular = "lightgray", elite = "gray", best = "black", shapes = "black"),
      edge = list(improving = "gray50",
      equal = rgb(0, 0, 250, max = 255, alpha = 180),
      worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4, 5.")
  )

  return(palette_colors)
}

#' Get shape mappings for STN-i nodes
#'
#' This function returns a mapping of node types to shape types based on the specified shape option.
#'
#' @param shape A numeric value specifying the shape option (1, 2, or 3). Default is 1.
#'
#' @return A named list mapping node types to shape names.
#'
#' @examples
#' \dontrun{
#' shapes <- get_stn_i_shapes(1)
#' # Returns: list(Start = "square", End = "triangle", Standard = "diamond", ...)
#' }
get_stn_i_shapes <- function(shape = 1) {
  shapes <- switch(as.character(shape),
    "1" = list(
      "Start" = "square",
      "End" = "triangle",
      "Standard" = "diamond",
      "Start-End" = "cross",
      "Start-Standard" = "star",
      "Standard-End" = "ellipse",
      "Start-Standard-End" = "circle"
    ),
    "2" = list(
      "Start" = "triangle",
      "End" = "square",
      "Standard" = "diamond",
      "Start-End" = "ellipse",
      "Start-Standard" = "cross",
      "Standard-End" = "star",
      "Start-Standard-End" = "circle"
    ),
    "3" = list(
      "Start" = "diamond",
      "End" = "cross",
      "Standard" = "star",
      "Start-End" = "ellipse",
      "Start-Standard" = "circle",
      "Standard-End" = "square",
      "Start-Standard-End" = "triangle"
    ),
    stop("Invalid shape option. Choose from: 1, 2, 3.")
  )

  return(shapes)
}

#' Helper function: extract per-vertex values
#'
#' @param params Parameters function
#' @param name Attribute name
#' @param v Vertex indices
#' @param n Number of vertices
#' @return Vector of values
#' @keywords internal
get_vals <- function(params, name, v, n) {
  x <- params("vertex", name)
  if (length(x) == 1) rep(x, n) else x[v]
}

#' Generic polygon plotter for custom shapes
#'
#' @param coords Coordinates matrix
#' @param v Vertex indices
#' @param params Parameters function
#' @param fpoly Polygon generation function
#' @keywords internal
polygon_shape <- function(coords, v, params, fpoly) {
  n <- nrow(coords)
  cols  <- get_vals(params, "color", v, n)
  sizes <- get_vals(params, "size",  v, n)

  for (i in seq_len(n)) {
    poly <- fpoly(coords[i,1], coords[i,2], sizes[i])
    polygon(poly$x, poly$y, col = cols[i], border = NA)
  }
}

#' Generate circle shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_circle <- function(x, y, s) {
  r <- s * 0.5
  t <- seq(0, 2*pi, length.out = 60)
  list(
    x = x + r*cos(t),
    y = y + r*sin(t)
  )
}

#' Generate square shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_square <- function(x, y, s) {
  r <- s * 0.5
  k <- r / sqrt(2)   # para que quepa dentro del mismo radio
  list(
    x = x + c(-k, k, k, -k),
    y = y + c(k,  k, -k, -k)
  )
}

#' Generate triangle shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_triangle <- function(x, y, s) {
  r <- s * 0.5
  ang <- seq(pi/2, pi/2 + 2*pi, length.out = 4)[1:3]
  list(
    x = x + r * cos(ang),
    y = y + r * sin(ang)
  )
}

#' Generate ellipse shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_ellipse <- function(x, y, s) {
  r1 <- s * 0.5
  r2 <- r1 * 0.60
  t  <- seq(0, 2*pi, length.out=60)
  list(
    x = x + r1*cos(t),
    y = y + r2*sin(t)
  )
}

#' Generate cross shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_cross <- function(x, y, s) {
  r <- s * 0.5
  t <- r * 0.30
  list(
    x = x + c(
      -t,  t,   t,   r,   r,   t,   t,  -t,
      -t, -r,  -r,  -t
    ),
    y = y + c(
      r,  r,   t,   t,  -t,  -t,  -r,  -r,
      -t, -t,   t,   t
    )
  )
}

#' Generate star shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_star <- function(x, y, s) {
  r1 <- s * 0.5
  r2 <- r1 * 0.45
  k  <- 5
  t  <- seq(0, 2*pi, length.out = 2*k + 1)
  rad <- rep(c(r1, r2), k)[1:(2*k)]
  list(
    x = x + rad * cos(t),
    y = y + rad * sin(t)
  )
}

#' Generate diamond shape coordinates
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param s Size
#' @return List with x and y coordinates
#' @keywords internal
f_diamond <- function(x, y, s) {
  r <- s * 0.5
  list(
    x = x + c(0,  r,  0, -r),
    y = y + c(r,  0, -r,  0)
  )
}

#' Initialize custom shapes for STN-i plotting
#'
#' This function registers custom shape definitions with igraph for use in STN-i plots.
#' It should be called once before plotting to ensure all custom shapes are available.
#'
#' @return None. Registers shapes as a side effect.
#'
#' @examples
#' \dontrun{
#' init_stn_i_shapes()
#' }
init_stn_i_shapes <- function() {
  add_shape("circle",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_circle))

  add_shape("square",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_square))

  add_shape("triangle",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_triangle))

  add_shape("ellipse",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_ellipse))

  add_shape("cross",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_cross))

  add_shape("star",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_star))

  add_shape("diamond",
            clip = shapes("circle")$clip,
            plot = function(...) polygon_shape(..., fpoly = f_diamond))
}

#' Decorate nodes and edges an STN for visualising a single algorithm STN
#' 
#' This function decorates the nodes and edges of an STN graph object for visualization purposes.
#' 
#' @param STN_i Graph object
#' @param problem_type Boolean indicating minimisation or not
#' @param show_regular Boolean indicating whether to show regular nodes
#' @param show_start_regular Boolean indicating whether to show start regular nodes
#' @param show_single_nodes Boolean indicating whether to show single nodes
#' @param palette_colors List of colors for the different node and edge types
#' @param shape_option Numeric value (1-3) specifying the shape mapping option (default is 1)
#' @param size_type String indicating the size type for nodes (default is "equals")
#' 
#' @return Decorated STN-i graph object
#' 
#' @examples
#' \dontrun{
#' STN_i <- stn_i_decorate(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1), shape_option = 1)
#' }
stn_i_decorate <- function(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, show_single_nodes = TRUE, palette_colors, shape_option = 1, size_type = "equals") {

  # Filter vertices according to settings (before any decoration)
  to_remove <- c()
  if (!show_regular) {
    to_remove <- c(to_remove, V(STN_i)[IS_ELITE == FALSE]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(STN_i)[IS_ELITE == FALSE & STARTS > 0]$name)
  }
  
  # Remove duplicates before deletion
  to_remove <- unique(to_remove)
  
  # Delete the vertices
  if (length(to_remove) > 0) {
    STN_i <- delete_vertices(STN_i, to_remove)
  }
  
  # Remove single nodes (after other removals, to catch isolated nodes)
  if (!show_single_nodes && vcount(STN_i) > 0) {
    single_nodes <- V(STN_i)[degree(STN_i, v = V(STN_i), mode = "all") == 0]$name
    if (length(single_nodes) > 0) {
      STN_i <- delete_vertices(STN_i, single_nodes)
    }
  }

  # Assign edge colors based on type
  E(STN_i)$color[E(STN_i)$Type == "IMPROVING"] <- palette_colors$edge$improving
  E(STN_i)$color[E(STN_i)$Type == "WORSENING"] <- palette_colors$edge$worsening
  E(STN_i)$color[E(STN_i)$Type == "EQUAL"] <- palette_colors$edge$equal

  # Set edge width proportional to number of visits (weight)
  E(STN_i)$width <- E(STN_i)$weight

  # Assign node shapes based on combination of configuration topology (STARTS, STANDARDS, ENDS)
  # Note: Each node can be a combination of START, STANDARD, and END types
  # Note 2: Exists 7 possible combinations (2^3 - 1)
  shape_mapping <- get_stn_i_shapes(shape_option)
  node_shapes <- character(vcount(STN_i))
  for (i in 1:vcount(STN_i)) {
    has_start <- V(STN_i)$STARTS[i] > 0
    has_standard <- V(STN_i)$STANDARDS[i] > 0
    has_end <- V(STN_i)$ENDS[i] > 0
    if (has_start && !has_standard && !has_end) {
      node_shapes[i] <- shape_mapping[["Start"]]
    } else if (!has_start && !has_standard && has_end) {
      node_shapes[i] <- shape_mapping[["End"]]
    } else if (!has_start && has_standard && !has_end) {
      node_shapes[i] <- shape_mapping[["Standard"]]
    } else if (has_start && !has_standard && has_end) {
      node_shapes[i] <- shape_mapping[["Start-End"]]
    } else if (has_start && has_standard && !has_end) {
      node_shapes[i] <- shape_mapping[["Start-Standard"]]
    } else if (!has_start && has_standard && has_end) {
      node_shapes[i] <- shape_mapping[["Standard-End"]]
    } else {
      node_shapes[i] <- shape_mapping[["Start-Standard-End"]]
    }
  }
  V(STN_i)$shape <- node_shapes

  # Assign node colors based on IS_ELITE and IS_BEST flags
  V(STN_i)[V(STN_i)$IS_ELITE == FALSE]$color <- palette_colors$node$regular
  V(STN_i)[V(STN_i)$IS_ELITE == TRUE]$color <- palette_colors$node$elite
  V(STN_i)[V(STN_i)$IS_BEST == TRUE]$color <- palette_colors$node$best

  # Assign node sizes based on size_type
  base_size <- 5
  switch(size_type,
    "equals" = {
      # Set all node sizes to a constant value
      V(STN_i)$size <- base_size
    },
    "configurations" = {
      # Set node size proportional to CONFIGURATIONS count (how many times node appears)
      V(STN_i)$size <- V(STN_i)$CONFIGURATIONS + 1
    },
    "out_degree" = {
      # Set node size proportional to out-degree
      V(STN_i)$size <- degree(STN_i, v = V(STN_i), mode = "out") + 1
    },
    "in_degree" = {
      # Set node size proportional to in-degree
      V(STN_i)$size <- degree(STN_i, v = V(STN_i), mode = "in") + 1
    },
    "degree" = {
      # Set node size proportional to total degree
      V(STN_i)$size <- degree(STN_i, v = V(STN_i), mode = "all") + 1
    },
    "elite_out_degree" = {
      # Set node size based on elite status
      # Regular nodes get base_size, elite nodes get size based on out-degree to regular nodes
      V(STN_i)$size <- base_size
      
      # For elite nodes, count out-neighbors that are regular
      elite_indices <- which(V(STN_i)$IS_ELITE == TRUE)
      V(STN_i)$size[elite_indices] <- sapply(elite_indices, function(v_idx) {
        out_neighbors <- neighbors(STN_i, v_idx, mode = "out")
        sum(V(STN_i)[out_neighbors]$IS_ELITE == FALSE) + 1
      })
    },
    {
      warning(sprintf("Invalid size_type '%s'. Using default 'equals'.", size_type))
      V(STN_i)$size <- 5
    }
  )

  # Increase size of BEST node for visibility
  if (any(V(STN_i)$IS_BEST == TRUE)) {
    V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size <- V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size * 1.2
  }

  return(STN_i)
}

#' Function to create a plot for STN-i data
#'
#' This function processes the input STN-i file, decorates the STN object, and prepares layout data for plotting.
#'
#' @param stn_i_result A list containing the STN-i graph object and problem type.
#' @param show_regular Boolean indicating whether to show regular nodes in the plot (default is TRUE).
#' @param show_start_regular Boolean indicating whether to show start regular nodes in the plot (default is TRUE).
#' @param show_single_nodes Boolean indicating whether to show single nodes in the plot (default is TRUE).
#' @param palette_colors List of colors for the different node and edge types.
#' @param shape_option Numeric value (1-3) specifying the shape mapping option (default is 1).
#' @param size_type String indicating the size type for nodes (default is "equals").
#' @param zoom_quantile Numeric value between 0 and 1 to define a zoom level for the plot (default is NA, meaning no zoom).
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#' result <- stn_i_plot_create("path/to/stn_i_file.Rdata", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1), shape_option = 1, zoom_quantile = 0.5)
#' }
stn_i_plot_create <- function(stn_i_result, show_regular = TRUE, show_start_regular = TRUE, show_single_nodes = TRUE, palette_colors, shape_option = 1, size_type = "equals", zoom_quantile = NA) {

  # Obtain the STN object and problem type
  STN_i <- stn_i_result$STN_i
  problem_type <- stn_i_result$problem_type

  # Decorate the STN-i object
  STN_i <- stn_i_decorate(
    STN_i = STN_i,
    problem_type = problem_type,
    show_regular = show_regular,
    show_start_regular = show_start_regular,
    show_single_nodes = show_single_nodes,
    palette_colors = palette_colors,
    shape_option = shape_option,
    size_type = size_type
  )

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
  if (!grepl("\\.pdf$", output_file_path))
    output_file_path <- paste0(output_file_path, ".pdf")

  # Initialize custom shapes for plotting
  init_stn_i_shapes()

  pdf(output_file_path, width = 11, height = 8)

  # Allow drawing outside
  par(xpd = NA)

  # --- Node size computation ---
  maxns <- max(V(STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(STN_i)$size + 1
  } else {
    nsize <- nsizef * V(STN_i)$size
  }
  ewidth <- ewidthf * E(STN_i)$width

  # --- Title ---
  regular_count <- sum(V(STN_i)$IS_ELITE == FALSE)
  elite_count <- sum(V(STN_i)$IS_ELITE == TRUE)
  best_count <- sum(V(STN_i)$IS_BEST == TRUE)
  improving_count <- sum(E(STN_i)$Type == "IMPROVING")
  equal_count <- sum(E(STN_i)$Type == "EQUAL")
  worsening_count <- sum(E(STN_i)$Type == "WORSENING")
  ttl <- paste(
    layout_data$title,
    ", Comp:", components(STN_i)$no,
    "\nNodes:", vcount(STN_i),
    ", Regular:", regular_count,
    ", Elite:", elite_count,
    ", Best:", best_count,
    "\nEdges:", ecount(STN_i),
    ", Improving:", improving_count,
    ", Equal:", equal_count,
    ", Worsening:", worsening_count
  )

  # --- Main graph ---
  plot(
    STN_i,
    layout = layout_data$coords,
    vertex.label = "",
    vertex.size  = nsize,
    edge.width   = ewidth,
    edge.arrow.size = asize,
    edge.curved  = ecurv,
    main = ttl
  )

  # --- Left legend construction ---

  # Counts for shape types
  start_shape_count <- sum(V(STN_i)$shape == "square", na.rm=TRUE)
  end_shape_count <- sum(V(STN_i)$shape == "triangle", na.rm=TRUE)
  standard_shape_count <- sum(V(STN_i)$shape == "diamond", na.rm=TRUE)
  start_end_shape_count <- sum(V(STN_i)$shape == "cross", na.rm=TRUE)
  start_standard_shape_count <- sum(V(STN_i)$shape == "star", na.rm=TRUE)
  standard_end_shape_count <- sum(V(STN_i)$shape == "ellipse", na.rm=TRUE)
  start_standard_end_shape_count <- sum(V(STN_i)$shape == "circle", na.rm=TRUE)

  # Define all possible shape types and their properties
  all_shape_types <- list(
    list(name = "Start", shape = "square", color = palette_colors$node$shapes),
    list(name = "End", shape = "triangle", color = palette_colors$node$shapes),
    list(name = "Standard", shape = "diamond", color = palette_colors$node$shapes),
    list(name = "Start-End", shape = "cross", color = palette_colors$node$shapes),
    list(name = "Start-Standard", shape = "star", color = palette_colors$node$shapes),
    list(name = "Standard-End", shape = "ellipse", color = palette_colors$node$shapes),
    list(name = "Start-Standard-End", shape = "circle", color = palette_colors$node$shapes)
  )
  
  # Detect which shape types actually exist in the graph
  shape_legend <- list()
  if (start_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[1]]
  }
  if (end_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[2]]
  }
  if (standard_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[3]]
  }
  if (start_end_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[4]]
  }
  if (start_standard_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[5]]
  }
  if (standard_end_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[6]]
  }
  if (start_standard_end_shape_count > 0) {
    shape_legend[[length(shape_legend) + 1]] <- all_shape_types[[7]]
  }

  # Define all possible node types
  all_node_types <- list(
    list(name = "Regular", shape = "circle", color = palette_colors$node$regular),
    list(name = "Elite", shape = "circle", color = palette_colors$node$elite),
    list(name = "Best", shape = "circle", color = palette_colors$node$best)
  )

  # Detect which node types actually exist
  node_legend <- list()
  if (regular_count > 0) {
    node_legend[[length(node_legend) + 1]] <- all_node_types[[1]]
  }
  if (elite_count > 0) {
    node_legend[[length(node_legend) + 1]] <- all_node_types[[2]]
  }
  if (best_count > 0) {
    node_legend[[length(node_legend) + 1]] <- all_node_types[[3]]
  }

  # Define all possible edge types
  all_edge_types <- list(
    list(name = "Improve", type = "IMPROVING", color = palette_colors$edge$improving),
    list(name = "Equal", type = "EQUAL", color = palette_colors$edge$equal),
    list(name = "Worse", type = "WORSENING", color = palette_colors$edge$worsening)
  )

  # Edge types - detect which actually exist
  edge_legend <- list()
  if (improving_count > 0) {
    edge_legend[[length(edge_legend) + 1]] <- all_edge_types[[1]]
  }
  if (equal_count > 0) {
    edge_legend[[length(edge_legend) + 1]] <- all_edge_types[[2]]
  }
  if (worsening_count > 0) {
    edge_legend[[length(edge_legend) + 1]] <- all_edge_types[[3]]
  }

  # Build combined legend with detected items
  total_legend_items <- length(shape_legend) + length(node_legend) + length(edge_legend)

  shape_fun <- list(
    square = f_square,
    triangle = f_triangle,
    diamond = f_diamond,
    cross = f_cross,
    star = f_star,
    ellipse = f_ellipse,
    circle = f_circle
  )

  # --- Legend coordinates ---
  xsh <- -1.30
  xtext <- -1.25
  y_start <- 0.95
  dy <- 0.07

  # --- Constant shape size ---
  shape_size <- 0.05
  
  # Current y position and item counter
  current_y <- y_start
  item_index <- 0

  # --- Draw shape types ---
  for (st in shape_legend) {
    fn <- shape_fun[[ st$shape ]]
    poly <- fn(xsh, current_y, shape_size)
    polygon(poly$x, poly$y, col = st$color, border = NA)
    text(xtext, current_y, st$name, adj = 0, cex = 0.9)
    item_index <- item_index + 1
    current_y <- y_start - item_index * dy
  }

  # --- Draw node types ---
  for (nt in node_legend) {
    fn <- shape_fun[[ nt$shape ]]
    poly <- fn(xsh, current_y, shape_size)
    polygon(poly$x, poly$y, col = nt$color, border = NA)
    text(xtext, current_y, nt$name, adj = 0, cex = 0.9)
    item_index <- item_index + 1
    current_y <- y_start - item_index * dy
  }

  # --- Draw edge types ---
  for (et in edge_legend) {
    segments(xsh - 0.02, current_y, xsh + 0.02, current_y,
            col = et$color, lwd = 2)
    text(xtext, current_y, et$name, adj = 0, cex = 0.9)
    item_index <- item_index + 1
    current_y <- y_start - item_index * dy
  }

  dev.off()
  message("STN-i plot saved to: ", output_file_path)
}

# nolint end
