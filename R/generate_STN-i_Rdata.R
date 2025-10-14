# nolint start

#########################################################################
# STN-i File Processing Script
# Author: Pablo Estobar
#
# Description:
# This script processes STN-i trace files from independent irace executions
# and builds the corresponding Search Trajectory Network (STN-i).
# It allows configuration for minimization or maximization problems,
# supports optional specification of a best-known solution and number
# of runs to consider, and saves the resulting STN-i as a .RData file.
#
# Usage:
# Rscript generate_STN-i_Rdata.R --input=<input_file> --output=<output_folder> /
#                                [--output_file=<output_file_name>] /
#                                [--problem_type=<min|max>] /
#                                [--best_known_solution=<numeric_value>] /
#                                [--number_of_runs=<integer_value>] /
#                                [--network_name=<name>]
#
# Arguments:
# --input                : (Required) Path to the input file (.csv) containing STN-i trace data
#                          from irace executions. Must include columns:
#                          RUN_ID, PATH, ITERATION_1, SOLUTION_1, QUALITY_1, 
#                          ORIGIN_TYPE_1, TYPE_1, ORIGIN_IS_ELITE_1, IS_ELITE_1,
#                          ITERATION_2, SOLUTION_2, QUALITY_2, ORIGIN_TYPE_2, 
#                          TYPE_2, ORIGIN_IS_ELITE_2, IS_ELITE_2
#
# --output               : (Required) Path to the output folder where the resulting
#                          STN-i object (.RData) will be saved.
#
# --output_file          : (Optional) Name of the output file (default: input file name
#                          without extension + "_stn_i.RData").
#
# --problem_type         : (Optional) Optimization objective. Use:
#                            - "min" for minimization (default)
#                            - "max" for maximization
#
# --best_known_solution  : (Optional) Numeric value representing the best-known solution.
#                          If not provided, the best value is computed from the trace.
#
# --number_of_runs       : (Optional) Integer specifying how many independent runs
#                          to include from the trace data. If not set, uses the maximum run found.
#
# --network_name         : (Optional) Name for the network. If not provided, uses the input file name.
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - plyr
#     - dplyr
#     - optparse
#
# Notes:
# - Input files must be in CSV format with standard comma separator
# - All required columns must be present in the input file
# - SOLUTION columns contain the location codes that identify configurations
# - QUALITY columns contain the performance metric values (lower is better for min problems)
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("plyr", quietly = TRUE)) {
  stop("Error: The plyr package is not installed. Please install it with 'install.packages(\"plyr\")'", call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Error: The dplyr package is not installed. Please install it with 'install.packages(\"dplyr\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(igraph)
library(plyr)
library(dplyr)
library(optparse)

# ---------- Load utility functions ----------
source("R/utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the input file (.csv) containing STN-i trace data"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the output folder where the resulting STN-i object (.RData) will be saved"),

  make_option(c("-f", "--output_file"),
              type="character",
              default=NULL,
              help="Name of the output file [default= input_name_stn_i.RData]"),

  make_option(c("-p", "--problem_type"),
              type="character", 
              default="min",
              help="Optimization objective ('min' or 'max') [default= %default]"),

  make_option(c("-b", "--best_known_solution"),
              type="numeric",
              default=NA,
              help="Best known solution value [default= calculated from data]"),

  make_option(c("-r", "--number_of_runs"),
              type="integer",
              default=NA,
              help="Number of runs to process [default= all runs]"),

  make_option(c("-n", "--network_name"),
              type="character",
              default=NULL,
              help="Name for the network [default= input file name]"),

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

# Process input file
input_file <- normalizePath(opt$input, mustWork = TRUE)
if (!file.exists(input_file)) {
  stop(sprintf("Input file does not exist: %s", input_file))
}

# Check if input file is CSV
if (!grepl("\\.csv$", input_file, ignore.case = TRUE)) {
  stop("Input file must be a CSV file.")
}

# Process output directory
output_folder <- normalizePath(opt$output, mustWork = FALSE)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Process output filename
if (is.null(opt$output_file)) {
  input_basename <- tools::file_path_sans_ext(basename(input_file))
  output_file_name <- paste0(input_basename, "_stn_i.RData")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.RData$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".RData")
  }
}

# Validate problem type
if (!opt$problem_type %in% c("min", "max")) {
  stop("Problem type must be 'min' or 'max'")
}

# Process network name
network_name <- if (!is.null(opt$network_name)) {
  opt$network_name
} else {
  tools::file_path_sans_ext(basename(input_file))
}

# ---------- Process the STN-i file ----------

if (opt$verbose) cat("Creating STN-i object...\n")

# Create the STN-i object from the input file and parameters
stn_i_result <- stn_i_create(
  input_file = input_file, 
  problem_type = opt$problem_type, 
  best_known_solution = opt$best_known_solution, 
  number_of_runs = opt$number_of_runs, 
  network_name = network_name
)

# ---------- Save result ----------

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

if (opt$verbose) cat(sprintf("Saving STN-i to %s...\n", output_file_path))

# Save the STN-i result to the specified output file
save_stn_i_data(stn_i_result = stn_i_result, output_file_path = output_file_path)

if (opt$verbose) cat("Done.\n")

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
