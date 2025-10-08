# nolint start

#########################################################################
# STN-i Plotting Script
# Author: Pablo Estobar
#
# Description:
# This script generates a visual representation of a Search Trajectory Network 
# for irace (STN-i) from a single STN-i file in .RData format.
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
#
# Arguments:
# --input         : (Required) Path to the input file (.RData) containing the STN-i object.
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

# ---------- Load the required packages ----------
library(igraph)

# ---------- Load utility functions ----------
source("R/utils.R")

# ---------- Parse command line arguments ----------
args <- commandArgs(trailingOnly = TRUE)
params <- parse_arguments(args)

# ---------- Validate required arguments ----------
required_args <- c("input", "output")
for (param_name in required_args) {
  if (is.null(params[[param_name]])) {
    stop(paste("Missing required argument: --", param_name, sep = ""), call. = FALSE)
  }
}

# ---------- Assign and normalize paths ----------
input_file <- normalizePath(params$input, mustWork = TRUE)
output_folder <- normalizePath(params$output, mustWork = TRUE)

# Check if input file exists
if (!file.exists(input_file)) {
  stop(paste("Input file does not exist:", input_file), call. = FALSE)
}

# Check if input file is a valid STN-i file
if (!grepl("\\.RData$", input_file)) {
  stop("Input file must be a .RData file containing an STN-i object.", call. = FALSE)
}

# Check if the output folder exists, if not create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------- Optional parameters ----------

if (!is.null(params$output_file)) {
  output_file_name <- params$output_file
} else {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, ".pdf")
}
layout_type <- ifelse(!is.null(params$layout_type), params$layout_type, "fr")
show_regular <- ifelse(!is.null(params$show_regular), as.logical(params$show_regular), TRUE)
show_start_regular <- ifelse(!is.null(params$show_start_regular), as.logical(params$show_start_regular), FALSE)
size_factor <- ifelse(!is.null(params$size_factor), as.numeric(params$size_factor), 1)
palette     <- ifelse(!is.null(params$palette), as.integer(params$palette), 1)
zoom_quantile <- ifelse(!is.null(params$zoom_quantile), as.numeric(params$zoom_quantile), NA)

# Check if the output file name has a valid extension
if (!grepl("\\.pdf$", output_file_name)) {
  output_file_name <- paste0(output_file_name, ".pdf")
}

# Check if layout is valid
if (!layout_type %in% c(
  "fr", "kk", "circle", "grid", "sphere",
  "random", "star", "tree", "reingold", "mds",
  "drl", "lgl", "graphopt", "sugiyama", "dh"
)) {
  stop("Invalid layout_type. Choose from: fr, kk, circle, grid, sphere, random, star, tree, reingold, mds, drl, lgl, graphopt, sugiyama, dh.", call. = FALSE)
}

# Check if show_regular is a boolean
if (!is.logical(show_regular)) {
  stop("show_regular must be a boolean value (TRUE or FALSE).", call. = FALSE)
}

# Check if size_factor is numeric
if (!is.numeric(size_factor)) {
  stop("size_factor must be a numeric value.", call. = FALSE)
}

# Check if palette is valid
if (!palette %in% c(1, 2, 3, 4, 5)) {
  stop("Invalid palette. Choose from: 1, 2, 3, 4, 5.", call. = FALSE)
}

# Check if zoom_quantile is numeric and within the valid range
if (!is.na(zoom_quantile) && (zoom_quantile < 0 || zoom_quantile > 1)) {
  stop("zoom_quantile must be a numeric value between 0 and 1 (exclusive).", call. = FALSE)
}

# ---------- Process the input file ----------

# Obtain the palette colors
palette_colors <- get_stn_i_palette_colors(palette)

# Load the STN-i object decorated
STN_i <- stn_i_plot_create(input_file, show_regular, show_start_regular, palette_colors, zoom_quantile)

# ---------- Save result ----------

# Obtain layout data
layout_data <- get_layout_data(STN_i, layout_type)

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

# Save the STN-i plot as a PDF
save_stn_i_plot(output_file_path, STN_i, layout_data, palette_colors, nsizef = size_factor, ewidthf = size_factor, asize = 0.3, ecurv = 0.3)

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
