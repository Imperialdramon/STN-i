# nolint start

#########################################################################
# Merged STN-i Plotting Script
# Author: Pablo Estobar
#
# Description:
# This script generates a visual representation of a merged Search Trajectory 
# Network for irace (STN-i), built from multiple STN-i executions.
# It decorates the network with node shapes, colors, and categories,
# and saves the plot as a PDF file, including a legend.
#
# Usage:
# Rscript plot_merged_STN-i.R --input=<input_file> --output=<output_folder> 
#                              [--output_file=<output_file_name>] /
#                              [--layout_type=<value>] /
#                              [--show_shared_regular=<TRUE|FALSE>] /
#                              [--show_shared_mixed=<TRUE|FALSE>] /
#                              [--show_regular=<TRUE|FALSE>] /
#                              [--show_start_regular=<TRUE|FALSE>] /
#                              [--size_factor=<value>] /
#                              [--palette=<value>] /
#                              [--zoom_quantile=<value>] /
#                              [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input         : (Required) Path to the input file (.Rdata) containing the merged STN-i object.
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
# --show_shared_regular : (Optional) Whether to include shared regular nodes in the plot.
#                   TRUE or FALSE (default: TRUE).
# --show_shared_mixed : (Optional) Whether to include shared mixed nodes in the plot.
#                   TRUE or FALSE (default: TRUE).
# --show_regular  : (Optional) Whether to include regular (non-elite) nodes in the plot.
#                   TRUE or FALSE (default: TRUE).
# --show_start_regular : (Optional) Whether to include start regular nodes in the plot.
#                   TRUE or FALSE (default: FALSE).
# --size_factor   : (Optional) Scaling factor for node sizes and edge widths (default: 1).
# --palette       : (Optional) Integer value (1–5) specifying a color palette for nodes.
#                   Each palette alters the visual distinction of node types (default: 1).
# --zoom_quantile : (Optional) Numeric value between 0 and 1 to define a zoom level for the plot.
# --verbose      : (Optional) Whether to show detailed processing information (default: FALSE).
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - optparse
# - A merged STN-i object created using `merge_stns_i_data()`.
#
# Note:
# - The input file must exist and contain a valid merged STN-i object.
# - The PDF will include a legend showing node types and their visual representation.
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
source("R/Functions/general_utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the input file (.Rdata) containing merged STN-i object"),

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
              help="Layout algorithm to position the nodes [default= %default]"),

  make_option(c("-r", "--show_regular"),
              type="logical",
              default=TRUE,
              help="Whether to include regular nodes in the plot [default= %default]"),

  make_option(c("-s", "--show_start_regular"),
              type="logical",
              default=FALSE,
              help="Whether to include start regular nodes in the plot [default= %default]"),

  make_option(c("-g", "--show_shared_regular"),
              type="logical",
              default=TRUE,
              help="Whether to include shared regular nodes in the plot [default= %default]"),

  make_option(c("-m", "--show_shared_mixed"),
              type="logical",
              default=TRUE,
              help="Whether to include shared mixed nodes in the plot [default= %default]"),

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

# Process paths and validate input file
input_file <- normalizePath(opt$input, mustWork = TRUE)

# Validate input file extension
if (!grepl("\\.Rdata$", input_file)) {
  stop("Input file must be a .Rdata file containing an STN-i object")
}

# Normalize output path
output_folder <- normalizePath(opt$output, mustWork = FALSE)

# Process paths
input_file <- normalizePath(opt$input, mustWork = TRUE)
output_folder <- normalizePath(opt$output, mustWork = FALSE)

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

if (opt$verbose) cat(sprintf("Processing merged STN-i data from %s...\n", input_file))

# Validate inputs
# Check if layout type is valid
valid_layouts <- c(
  "fr", "kk", "circle", "grid", "sphere",
  "random", "star", "tree", "reingold", "mds",
  "drl", "lgl", "graphopt", "sugiyama", "dh"
)
if (!opt$layout_type %in% valid_layouts) {
  stop(sprintf("Invalid layout_type. Choose from: %s", paste(valid_layouts, collapse = ", ")))
}

# Check if boolean parameters are logical
for (param in c("show_regular", "show_start_regular", "show_shared_regular", "show_shared_mixed")) {
  if (!is.logical(opt[[param]])) {
    stop(sprintf("%s must be a logical value (TRUE or FALSE)", param))
  }
}

# Check if size_factor is numeric and positive
if (!is.numeric(opt$size_factor) || opt$size_factor <= 0) {
  stop("size_factor must be a positive numeric value")
}

# Check if palette is valid
if (!opt$palette %in% 1:5) {
  stop("palette must be an integer between 1 and 5")
}

# Check if zoom_quantile is numeric and within valid range
if (!is.na(opt$zoom_quantile) && (opt$zoom_quantile <= 0 || opt$zoom_quantile >= 1)) {
  stop("zoom_quantile must be between 0 and 1 (exclusive)")
}

# ---------- Process the input file ----------

if (opt$verbose) cat(sprintf("Generating layout with %s algorithm...\n", opt$layout_type))

# Obtain the palette colors
palette_colors <- get_merged_stn_i_palette_colors(opt$palette)

# Load the merged STN-i data
merged_stn_i_data <- get_merged_stn_i_data(input_file)

# Load the STN-i object decorated
merged_STN_i <- merged_stn_i_plot_create(
  merged_stn_i_data, 
  opt$show_shared_regular, 
  opt$show_shared_mixed, 
  opt$show_regular, 
  opt$show_start_regular, 
  palette_colors, 
  opt$zoom_quantile
)

# ---------- Save result ----------

# Obtain layout data
layout_data <- get_layout_data(merged_STN_i, opt$layout_type)

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

if (opt$verbose) cat(sprintf("Saving plot to %s...\n", output_file_path))

# Save the STN-i plot as a PDF
save_merged_stn_i_plot(
  output_file_path, 
  merged_STN_i, 
  merged_stn_i_data$network_names, 
  layout_data, 
  palette_colors, 
  nsizef = opt$size_factor, 
  ewidthf = opt$size_factor, 
  asize = 0.3, 
  ecurv = 0.3
)

if (opt$verbose) cat("Done.\n")

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
