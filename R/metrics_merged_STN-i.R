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
# Arguments:
# --input                : (Required) Path to the input file (.RData) containing STN-i data.
#
# --output               : (Required) Path to the output folder where the resulting metrics for STN-i object will be saved.
#
# --output_file          : (Optional)  Name of the output file (default: input file name without extension + "_metrics.csv").
#
# Requirements:
# - R with the following packages installed:
#     - igraph
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

# ---------- Load the required packages ----------
library(igraph)

# ---------- Load utility functions ----------
source("R/utils.R")

# ---------- Parse command line arguments ----------
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

# Check if the output folder exists, if not create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------- Optional parameters ----------
if (!is.null(params$output_file)) {
  output_file_name <- params$output_file
} else {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, "_metrics.csv")
}

# Check if output file name has a valid extension
if (!grepl("\\.csv$", output_file_name)) {
  output_file_name <- paste0(output_file_name, ".csv")
}

# ---------- Obtain the merged STN-i metrics ----------

# Load the merged STN-i object from the input file
merged_stn_i_result <- get_merged_stn_i_data(input_file)

# Obtain the metrics from the merged STN-i result
merged_stn_i_metrics <- get_merged_stn_i_metrics(merged_stn_i_result)

# ---------- Save result ----------

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

# Save the merged STN-i result to the specified output file
save_merged_stn_i_metrics(merged_stn_i_metrics, output_file_path)

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
