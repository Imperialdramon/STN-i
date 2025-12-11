# nolint start

#########################################################################
# STN-i Plotting Script
# Author: Pablo Estobar
#
# Description:
# This script generates a visual representation of a Search Trajectory Network 
# for irace (STN-i) from a single STN-i file in .Rdata format.
#
# Usage:
# Rscript plot_STN-i.R --input=<input_file> --output=<output_folder> 
#                      [--output_file=<output_file_name>] 
#                      [--layout_type=<value>] 
#                      [--show_regular=<TRUE|FALSE>]
#                      [--show_start_regular=<TRUE|FALSE>]
#                      [--size_factor=<value>] 
#                      [--palette=<value>]
#                      [--zoom_quantile=<value>]
#                      [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input         : (Required) Path to the input file (.Rdata) containing the STN-i object.
# --output        : (Required) Path to the output folder where the plot PDF will be saved.
# --output_file   : (Optional) Name of the output PDF file. If not provided, defaults to the
#                   input file name (without extension) + ".pdf".
# --layout_type   : (Optional) Layout algorithm to position the nodes. Options:
#                     - "fr"        : Fruchterman-Reingold (default)
#                     - "kk"        : Kamada-Kawai
#                     - "circle"    : Circular layout
#                     - "grid"      : Grid layout
#                     - "sphere"    : Spherical layout
#                     - "random"    : Random layout
#                     - "drl"       : DrL (force-directed, scalable)
#                     - "graphopt"  : Force-directed using physics model
# --show_regular  : (Optional) Whether to include REGULAR nodes in the plot.
#                   TRUE or FALSE (default: TRUE).
# --show_start_regular : (Optional) Whether to include START REGULAR nodes in the plot.
#                   TRUE or FALSE (default: FALSE).
# --size_factor   : (Optional) Scaling factor for node sizes and edge widths (default: 1).
# --palette       : (Optional) Integer value (1–5) specifying a color palette for nodes and edges.
#                   Each palette alters the visual distinction of node types (default: 1).
# --zoom_quantile : (Optional) Numeric value between 0 and 1 to define a zoom level for the plot.
# --verbose      : (Optional) Whether to show detailed processing information (default: FALSE).
#
# Requirements:
# - R with the `igraph` package installed.
#
# Note:
# - The input file must exist and contain a valid STN-i object.
# - Plots are saved as PDF files using the specified or default name.
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(igraph)
library(optparse)

# ---------- Load utility functions ----------
source("R/utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the input file (.Rdata) containing the STN-i object"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the output folder where the plot PDF will be saved"),

  make_option(c("-f", "--output_file"),
              type="character",
              default=NULL,
              help="Name of the output PDF file [default= input_name.pdf]"),

  make_option(c("-l", "--layout_type"),
              type="character", 
              default="fr",
              help="Layout algorithm to position the nodes: fr, kk, circle, grid, sphere, random, drl, graphopt [default= %default]"),

  make_option(c("-r", "--show_regular"),
              type="logical",
              default=TRUE,
              help="Whether to include REGULAR nodes in the plot [default= %default]"),

  make_option(c("-s", "--show_start_regular"),
              type="logical",
              default=FALSE,
              help="Whether to include START REGULAR nodes in the plot [default= %default]"),

  make_option(c("-z", "--size_factor"),
              type="numeric",
              default=1,
              help="Scaling factor for node sizes and edge widths [default= %default]"),

  make_option(c("-p", "--palette"),
              type="integer",
              default=1,
              help="Color palette (1-5) for nodes and edges [default= %default]"),

  make_option(c("-q", "--zoom_quantile"),
              type="numeric",
              default=NA,
              help="Zoom level for the plot (0-1) [default= no zoom]"),

  make_option(c("-v", "--verbose"),
              type="logical",
              default=FALSE,
              help="Show detailed processing information [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$input)) {
  stop("Input file is required (-i/--input)")
}
if (is.null(opt$output)) {
  stop("Output folder is required (-o/--output)")
}

# Process paths
input_file <- normalizePath(opt$input, mustWork = TRUE)
output_folder <- normalizePath(opt$output, mustWork = FALSE)

# Input validation
if (!file.exists(input_file)) {
  stop(sprintf("Input file does not exist: %s", input_file))
}

# Check if input file is a valid STN-i file
if (!grepl("\\.Rdata$", input_file)) {
  stop("Input file must be a .Rdata file containing an STN-i object.")
}

# Create output directory if needed
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Process output filename
if (is.null(opt$output_file)) {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, ".pdf")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.pdf$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".pdf")
  }
}

# Validate layout type
valid_layouts <- c(
  "fr", "kk", "circle", "grid", "sphere",
  "random", "drl", "graphopt"
)
if (!opt$layout_type %in% valid_layouts) {
  stop(sprintf("Invalid layout_type. Choose from: %s", paste(valid_layouts, collapse = ", ")))
}

# Validate numeric parameters
if (!is.numeric(opt$size_factor) || opt$size_factor <= 0) {
  stop("size_factor must be a positive numeric value")
}

if (!opt$palette %in% 1:5) {
  stop("palette must be an integer between 1 and 5")
}

# Handle zoom_quantile - NA should be passed as is, or as numeric value
if (!is.na(opt$zoom_quantile)) {
  # If it's not NA, validate it's a number between 0 and 1 (exclusive)
  if (is.character(opt$zoom_quantile) && opt$zoom_quantile == "NA") {
    # String "NA" - convert to actual NA
    opt$zoom_quantile <- NA
  } else if (!is.numeric(opt$zoom_quantile)) {
    # Try to convert to numeric
    opt$zoom_quantile <- as.numeric(opt$zoom_quantile)
    if (is.na(opt$zoom_quantile)) {
      stop("zoom_quantile must be NA or a numeric value between 0 and 1 (exclusive)")
    }
  }
  
  # If it's a number, validate the range
  if (!is.na(opt$zoom_quantile) && (opt$zoom_quantile <= 0 || opt$zoom_quantile >= 1)) {
    stop("zoom_quantile must be between 0 and 1 (exclusive)")
  }
}

# ---------- Process the input file ----------

if (opt$verbose) cat("Processing STN-i data...\n")

# Obtain the palette colors
palette_colors <- get_stn_i_palette_colors(opt$palette)

# Load the STN-i object decorated
STN_i <- stn_i_plot_create(
  input_file = input_file,
  show_regular = opt$show_regular,
  show_start_regular = opt$show_start_regular,
  palette_colors = palette_colors,
  zoom_quantile = opt$zoom_quantile
)

if (opt$verbose) cat(sprintf("Generating layout with %s algorithm...\n", opt$layout_type))

# Obtain layout data
layout_data <- get_layout_data(STN_i, opt$layout_type)

# ---------- Save result ----------

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

if (opt$verbose) cat(sprintf("Saving plot to %s...\n", output_file_path))

# Save the STN-i plot as a PDF
save_stn_i_plot(
  output_file_path = output_file_path,
  STN_i = STN_i,
  layout_data = layout_data,
  palette_colors = palette_colors,
  nsizef = opt$size_factor,
  ewidthf = opt$size_factor,
  asize = 0.3,
  ecurv = 0.3
)

if (opt$verbose) cat("Done.\n")

#  ---------- Functions ----------

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
    to_remove <- c(to_remove, V(STN_i)[IS_ELITE == FALSE]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(STN_i)[IS_ELITE == FALSE & STARTS > 0]$name)
  }

  # Remove duplicates before deletion
  #to_remove <- V(STN_i)[IS_ELITE == FALSE]$name
  to_remove <- unique(to_remove)
  STN_i <- delete_vertices(STN_i, to_remove)

  # Assign edge colors based on type
  E(STN_i)$color[E(STN_i)$Type == "IMPROVING"] <- palette_colors$edge$improving
  E(STN_i)$color[E(STN_i)$Type == "WORSENING"] <- palette_colors$edge$worsening
  E(STN_i)$color[E(STN_i)$Type == "EQUAL"]     <- palette_colors$edge$equal

  # Set edge width proportional to number of visits (weight)
  E(STN_i)$width <- E(STN_i)$weight

  # Assign node shapes based on combination of configuration topology (STARTS, STANDARDS, ENDS)
  node_shapes <- character(vcount(STN_i))
  for (i in 1:vcount(STN_i)) {
    has_start <- V(STN_i)$STARTS[i] > 0
    has_standard <- V(STN_i)$STANDARDS[i] > 0
    has_end <- V(STN_i)$ENDS[i] > 0
    if (has_start && !has_standard && !has_end) {
      # Start only → square
      node_shapes[i] <- "square"
    } else if (!has_start && !has_standard && has_end) {
      # End only → triangle
      node_shapes[i] <- "triangle"
    } else if (!has_start && has_standard && !has_end) {
      # Standard only → diamond
      node_shapes[i] <- "diamond"
    } else if (has_start && !has_standard && has_end) {
      # Start + End (no Standard) → cross
      node_shapes[i] <- "cross"
    } else if (has_start && has_standard && !has_end) {
      # Start + Standard (no End) → star
      node_shapes[i] <- "star"
    } else if (!has_start && has_standard && has_end) {
      # Standard + End (no Start) → ellipse
      node_shapes[i] <- "ellipse"
    } else {
      # Start + Standard + End → circle
      node_shapes[i] <- "circle"
    }
  }
  V(STN_i)$shape <- node_shapes

  # Assign node colors based on IS_ELITE and IS_BEST flags
  V(STN_i)[V(STN_i)$IS_ELITE == FALSE]$color <- palette_colors$node$regular
  V(STN_i)[V(STN_i)$IS_ELITE == TRUE]$color <- palette_colors$node$elite
  V(STN_i)[V(STN_i)$IS_BEST == TRUE]$color <- palette_colors$node$best

  # Set node size proportional to CONFIGURATIONS count (how many times node appears)
  # V(STN_i)$size <- V(STN_i)$CONFIGURATIONS + 1
  V(STN_i)$size <- 5

  # Slightly increase the size of BEST nodes for visibility
  #V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size <- V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size + 0.5
  V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size <- V(STN_i)[V(STN_i)$IS_BEST == TRUE]$size + 2

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
#' result <- stn_i_plot_create("path/to/stn_i_file.Rdata", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1), zoom_quantile = 0.5)
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
save_stn_i_plot <- function(output_file_path, STN_i, layout_data,
                            palette_colors, nsizef = 1, ewidthf = 0.5,
                            asize = 0.3, ecurv = 0.3) {

  if (!grepl("\\.pdf$", output_file_path))
    output_file_path <- paste0(output_file_path, ".pdf")

  # Helper: extract per-vertex values
  get_vals <- function(params, name, v, n) {
    x <- params("vertex", name)
    if (length(x) == 1) rep(x, n) else x[v]
  }

  # Generic polygon plotter
  polygon_shape <- function(coords, v, params, fpoly) {
    n <- nrow(coords)
    cols  <- get_vals(params, "color", v, n)
    sizes <- get_vals(params, "size",  v, n)

    for (i in seq_len(n)) {
      poly <- fpoly(coords[i,1], coords[i,2], sizes[i])
      polygon(poly$x, poly$y, col = cols[i], border = NA)
    }
  }

  # Circle
  f_circle <- function(x, y, s) {
    r <- s * 0.5
    t <- seq(0, 2*pi, length.out = 60)
    list(
      x = x + r*cos(t),
      y = y + r*sin(t)
    )
  }

  # Square
  f_square <- function(x, y, s) {
    r <- s * 0.5
    k <- r / sqrt(2)   # para que quepa dentro del mismo radio
    list(
      x = x + c(-k, k, k, -k),
      y = y + c(k,  k, -k, -k)
    )
  }

  # Triangle
  f_triangle <- function(x, y, s) {
    r <- s * 0.5
    ang <- seq(pi/2, pi/2 + 2*pi, length.out = 4)[1:3]
    list(
      x = x + r * cos(ang),
      y = y + r * sin(ang)
    )
  }

  # Ellipse
  f_ellipse <- function(x, y, s) {
    r1 <- s * 0.5
    r2 <- r1 * 0.60
    t  <- seq(0, 2*pi, length.out=60)
    list(
      x = x + r1*cos(t),
      y = y + r2*sin(t)
    )
  }

  # Cross
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

  # Star
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

  # Diamond
  f_diamond <- function(x, y, s) {
    r <- s * 0.5
    list(
      x = x + c(0,  r,  0, -r),
      y = y + c(r,  0, -r,  0)
    )
  }

  # --------------------------------------------------------------------
  # Register all shapes
  # --------------------------------------------------------------------
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

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
