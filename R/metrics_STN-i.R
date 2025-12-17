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
# Rscript generate_STN-i_data.R --input=<input_file> --output=<output_folder> /
#                                [--output_file=<output_file_name>] /
#                                [--verbose=<TRUE|FALSE>]
# Arguments:
# --input                : (Required) Path to the input file (.Rdata) containing STN-i data.
# --output               : (Required) Path to the output folder where the resulting metrics for STN-i object will be saved.
# --output_file          : (Optional)  Name of the output file (default: input file name without extension + "_metrics.csv").
# --verbose              : (Optional) Whether to show detailed processing information (default: FALSE).
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#
# Notes:
# - The input file must exist and be formatted as a .Rdata file containing STN-i data.
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
source("R/Functions/network_utils.R")
source("R/Functions/metrics_utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the input file (.Rdata) containing STN-i data"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the output folder where the resulting metrics for STN-i object will be saved"),

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

# Process paths
input_file <- normalizePath(opt$input, mustWork = TRUE)
output_folder <- normalizePath(opt$output, mustWork = FALSE)

# Check if input file exists
if (!file.exists(input_file)) {
  stop(paste("Input file does not exist:", input_file), call. = FALSE)
}

# Check if the output folder exists, if not create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Create output directory if needed
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Process output filename
if (is.null(opt$output_file)) {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, "_metrics.csv")
  output_file_nodes <- paste0(input_basename, "_metrics_nodes.csv")
  output_file_elite_nodes <- paste0(input_basename, "_metrics_elite_nodes.csv")
  output_file_configs <- paste0(input_basename, "_metrics_configurations.csv")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.csv$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".csv")
  }
  output_file_nodes <- sub("\\.csv$", "_nodes.csv", output_file_name)
  output_file_elite_nodes <- sub("\\.csv$", "_elite_nodes.csv", output_file_name)
  output_file_configs <- sub("\\.csv$", "_configurations.csv", output_file_name)
}

if (opt$verbose) cat(sprintf("Loading STN-i data from %s...\n", input_file))

# ---------- Obtain the STN-i metrics ----------

# Load the STN-i object from the input file
stn_i_result <- get_stn_i_data(input_file)

# Obtain the three types of metrics
stn_i_metrics_nodes <- get_stn_i_metrics_nodes(stn_i_result)
stn_i_metrics_elite_nodes <- get_stn_i_metrics_elite_nodes(stn_i_result)
stn_i_metrics_configurations <- get_stn_i_metrics_configurations(stn_i_result)

# ---------- Save results ----------

# Construct the full paths for the output files
output_file_path_nodes <- file.path(output_folder, output_file_nodes)
output_file_path_elite_nodes <- file.path(output_folder, output_file_elite_nodes)
output_file_path_configs <- file.path(output_folder, output_file_configs)

if (opt$verbose) cat(sprintf("Saving nodes metrics to %s...\n", output_file_path_nodes))
save_stn_i_metrics(stn_i_metrics_nodes, output_file_path_nodes)

if (opt$verbose) cat(sprintf("Saving elite nodes metrics to %s...\n", output_file_path_elite_nodes))
save_stn_i_metrics(stn_i_metrics_elite_nodes, output_file_path_elite_nodes)

if (opt$verbose) cat(sprintf("Saving configurations metrics to %s...\n", output_file_path_configs))
save_stn_i_metrics(stn_i_metrics_configurations, output_file_path_configs)

if (opt$verbose) cat("Done.\n")

# ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
