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
#                                      [--output_file=<output_file_name>] 
#                                      [--criteria=<value>]
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

# ---------- Load the required packages ----------
library(igraph)
library(dplyr)
library(tidyr)

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
input_folder <- normalizePath(params$input, mustWork = TRUE)
output_folder <- normalizePath(params$output, mustWork = TRUE)

# Check if input file exists
if (!dir.exists(input_folder)) {
  stop(paste("Input forlder does not exist:", input_folder), call. = FALSE)
}

# Check if the output folder exists, if not create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------- Optional parameters ----------
if (!is.null(params$output_file)) {
  output_file_name <- params$output_file
} else {
  input_basename <- tools::file_path_sans_ext(basename(input_folder))
  output_file_name <- paste0(input_basename, "_merged_stn_i.RData")
}
criteria <- ifelse(!is.null(params$criteria), params$criteria, "mean")

# ---------- Process input files ----------

# Get the STNs-i data from the input folder
stns_i_data <- get_stns_i_data(input_folder)

# Merge the STN-i data based on the specified criteria
merged_stn_i_result <- merge_stns_i_data(stns_i_data, criteria)

# ---------- Save output file ----------

# Check if output file name has a valid extension
if (!grepl("\\.RData$", output_file_name)) {
  output_file_name <- paste0(output_file_name, ".RData")
}

# Construct the full output file path
output_file_path <- file.path(output_folder, output_file_name)

# Save the merged STN-i object
save_merged_stn_i_data(merged_stn_i_result, output_file_path)

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end