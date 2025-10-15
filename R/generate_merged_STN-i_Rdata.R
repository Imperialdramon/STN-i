# nolint start

#########################################################################
# STN-i Merge Script
# Author: Pablo Estobar
#
# Description:
# This script merges multiple Search Trajectory Networks for irace (STN-i) 
# from independent irace executions into a single unified network.
#
# Usage:
# Rscript generate_merged_STN-i_data.R --input=<input_folder> --output=<output_folder> 
#                                      [--output_file=<output_file_name>] /
#                                      [--criteria=<value>] /
#                                      [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input         : (Required) Path to the folder containing 2 or more .RData files, 
#                   each with an STN-i object named 'STN-i'.
# --output        : (Required) Path to the folder where the merged file will be saved.
# --output_file   : (Optional) Name for the output file. If not provided, defaults to 
#                   "<input_folder_name>_merged_stn_i.RData".
# --criteria      : (Optional) Criteria for merging shared nodes. Options:
#                     - "mean"   : Use the mean value for merging attributes (default).
#                     - "min"    : Use the minimum value.
#                     - "max"    : Use the maximum value.
#                     - "median" : Use the median value.
#                     - "mode"   : Use the mode value.
# --verbose      : (Optional) Whether to show detailed processing information (default: FALSE).
#
# Behavior:
# - Loads each `.RData` file and extracts the `STN-i` graph object.
# - Assigns algorithm identifiers based on filenames (prefix before first underscore).
# - Merges the networks using igraph::graph.union().
# - Consolidates vertex attributes (Fitness, Type, Quality, etc.) and 
#   assigns a unified classification (`Category`) to shared nodes:
#     - shared-regular
#     - shared-elite
#     - shared-mixed
#     - algorithm-elite
#     - algorithm-regular
# - Consolidates edge weights and algorithms.
# - Saves the merged network as "<input_folder_name>_merged_stn_i.RData".
# - The merged object is saved as `merged_STN_i` along with metadata (nruns, algn, bmin, best).
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - dplyr
#     - tidyr
#     - optparse
#
# Notes:
# - The input folder must contain 2 or more `.RData` files, each with a valid STN-i object.
# - The merging logic and attribute handling are implemented in utils.R.
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Error: The dplyr package is not installed. Please install it with 'install.packages(\"dplyr\")'", call. = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Error: The tidyr package is not installed. Please install it with 'install.packages(\"tidyr\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(igraph)
library(dplyr)
library(tidyr)
library(optparse)

# ---------- Load utility functions ----------
source("R/utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the folder containing 2 or more .RData files with STN-i objects"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the folder where the merged file will be saved"),

  make_option(c("-f", "--output_file"),
              type="character",
              default=NULL,
              help="Name for the output file [default= input_folder_name_merged_stn_i.RData]"),

  make_option(c("-c", "--criteria"),
              type="character",
              default="mean",
              help="Criteria for merging shared nodes: mean, min, max, median, mode [default= %default]"),

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
  stop("Input folder is required (-i/--input)")
}
if (is.null(opt$output)) {
  stop("Output folder is required (-o/--output)")
}

# Process paths and validate input folder
input_folder <- normalizePath(opt$input, mustWork = TRUE)

# Validate input folder exists and contains RData files
if (!dir.exists(input_folder)) {
  stop(sprintf("Input folder does not exist: %s", input_folder))
}

# Check for RData files in input folder
rdata_files <- list.files(input_folder, pattern = "\\.RData$", full.names = TRUE)
if (length(rdata_files) < 2) {
  stop(sprintf("Input folder must contain at least 2 .RData files, found %d", length(rdata_files)))
}

# Normalize output path
output_folder <- normalizePath(opt$output, mustWork = FALSE)

# Create output directory if needed
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Process and validate criteria
valid_criteria <- c("mean", "min", "max", "median", "mode")
if (!opt$criteria %in% valid_criteria) {
  stop(sprintf("Invalid criteria. Choose from: %s", paste(valid_criteria, collapse = ", ")))
}

# Process output filename
if (is.null(opt$output_file)) {
  input_basename <- tools::file_path_sans_ext(basename(input_folder))
  output_file_name <- paste0(input_basename, "_merged_stn_i.RData")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.RData$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".RData")
  }
}

if (opt$verbose) {
  cat(sprintf("Processing STN-i files from %s...\n", input_folder))
  cat(sprintf("Found %d .RData files\n", length(rdata_files)))
  cat(sprintf("Using '%s' criteria for merging\n", opt$criteria))
}

# ---------- Process input files ----------

# Get the STNs-i data from the input folder
if (opt$verbose) cat("Loading STN-i files...\n")
stns_i_data <- get_stns_i_data(input_folder)

# Merge the STN-i data based on the specified criteria
if (opt$verbose) cat(sprintf("Merging STN-i data using '%s' criteria...\n", opt$criteria))
merged_stn_i_result <- merge_stns_i_data(
  stns_i_data = stns_i_data,
  criteria = opt$criteria,
  verbose = opt$verbose
)

# Construct the full output file path
output_file_path <- file.path(output_folder, output_file_name)

# Save the merged STN-i object
if (opt$verbose) cat(sprintf("Saving merged STN-i to %s...\n", output_file_path))
save_merged_stn_i_data(
  merged_stn_i_data = merged_stn_i_result,
  output_file_path = output_file_path,
  verbose = opt$verbose
)

if (opt$verbose) cat("Done.\n")

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end