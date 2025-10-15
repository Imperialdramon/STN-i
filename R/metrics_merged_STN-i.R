# nolint start

#########################################################################
# STN-i Metrics Calculation Script
# Author: Pablo Estobar
#
# Description:
# This script processes a Search Trajectory Network (STN-i) object
# and computes various metrics such as the number of nodes, edges,
# best nodes, end nodes, connected components, strength of best nodes,
# average path length, and number of paths to best optima.
#
# Usage:
# Rscript metrics_merged_STN-i.R --input=<input_file> --output=<output_folder> /
#                                [--output_file=<output_file_name>] /
#                                [--verbose=<TRUE|FALSE>]
# Arguments:
# --input                : (Required) Path to the input file (.RData) containing STN-i data.
# --output               : (Required) Path to the output folder where the resulting metrics for STN-i object will be saved.
# --output_file          : (Optional)  Name of the output file (default: input file name without extension + "_metrics.csv").
# --verbose              : (Optional) Whether to show detailed processing information (default: FALSE).
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - optparse
#
# Notes:
# - The input file must exist and be formatted as a .RData file containing STN-i data.
# - The output will be saved as a CSV file containing the STN-i metrics.
# - Designed for execution from command line using named arguments.
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
              help="Path to the input file (.RData) containing merged STN-i data"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the output folder where the resulting metrics for merged STN-i object will be saved"),

  make_option(c("-f", "--output_file"),
              type="character",
              default=NULL,
              help="Name of the output file [default= input_name_metrics.csv]"),

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
if (!grepl("\\.RData$", input_file)) {
  stop("Input file must be a .RData file containing a merged STN-i object")
}

# Normalize output path
output_folder <- normalizePath(opt$output, mustWork = FALSE)

# Create output directory if needed
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Process output filename
if (is.null(opt$output_file)) {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, "_metrics.csv")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.csv$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".csv")
  }
}

if (opt$verbose) cat(sprintf("Loading merged STN-i data from %s...\n", input_file))

# ---------- Obtain the merged STN-i metrics ----------

# Load the merged STN-i object from the input file
merged_stn_i_result <- get_merged_stn_i_data(input_file)

# Obtain the metrics from the merged STN-i result
merged_stn_i_metrics <- get_merged_stn_i_metrics(merged_stn_i_result)

# ---------- Save result ----------

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

# Save the merged STN-i result to the specified output file
if (opt$verbose) cat(sprintf("Saving merged metrics to %s...\n", output_file_path))
save_merged_stn_i_metrics(merged_stn_i_metrics, output_file_path)

if (opt$verbose) cat("Done.\n")

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
