# nolint start

#########################################################################
# STN-i File Processing Script
# Author: Pablo Estobar
#
# Description:
# This script processes trace files from independent irace executions
# and builds the corresponding Search Trajectory Network (STN-i).
# It allows configuration for minimization or maximization problems,
# supports optional specification of a best-known solution and number
# of runs to consider, and saves the resulting STN-i as a .RData file.
#
# Usage:
# Rscript generate_STN-i_data.R --input=<input_file> --output=<output_folder> /
#                                [--output_file=<output_file_name>] /
#                                [--problem_type=<min|max>] /
#                                [--best_known_solution=<numeric_value>] /
#                                [--number_of_runs=<integer_value>] /
#                                [--separator=<char>] /
#                                [--network_name=<name>]
#
# Arguments:
# --input                : (Required) Path to the input file (.txt) containing trace data
#                          from irace executions.
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
# --separator            : (Optional) Separator used in the trace file (e.g., "," or "\t").
#                          Default is "" which uses any whitespace.
#
# --network_name         : (Optional) Name for the network. If not provided, uses the input file name.
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - plyr
#     - dplyr
#
# Notes:
# - The input file must exist and be formatted as a trace file from irace.
# - The output will be saved as an RData file containing the STN-i structure.
# - Designed for execution from command line using named arguments.
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

# ---------- Load the required packages ----------
library(igraph)
library(plyr)
library(dplyr)

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
  output_file_name <- paste0(input_basename, "_stn_i.RData")
}
problem_type <- ifelse(!is.null(params$problem_type), params$problem_type, "min")
best_known_solution <- ifelse(!is.null(params$best_known_solution), as.numeric(params$best_known_solution), NA)
number_of_runs <- ifelse(!is.null(params$number_of_runs), as.integer(params$number_of_runs), NA)
separator <- ifelse(!is.null(params$separator), params$separator, "")
if (!is.null(params$network_name)) {
  network_name <- as.character(params$network_name)
} else {
  network_name <- tools::file_path_sans_ext(basename(input_file))
}

# Check if output file name has a valid extension
if (!grepl("\\.RData$", output_file_name)) {
  output_file_name <- paste0(output_file_name, ".RData")
}

# Check if problem_type is valid
if (!problem_type %in% c("min", "max")) {
  stop("Invalid problem_type Options: 'min' for minimization or 'max' for maximization.", call. = FALSE)
}

# Check if best_known_solution and number_of_runs are numeric
if (!is.na(best_known_solution) && !is.numeric(best_known_solution)) {
  stop("best_known_solution must be a numeric value.", call. = FALSE)
}

# Check if number_of_runs is an integer
if (!is.na(number_of_runs) && !is.numeric(number_of_runs)) {
  stop("number_of_runs must be an integer value.", call. = FALSE)
}

# ---------- Process the STN-i file ----------

# Create the STN-i object from the input file and parameters
stn_i_result <- stn_i_create(input_file, problem_type, best_known_solution, number_of_runs, separator, network_name)

# ---------- Save result ----------

# Construct the full path for the output file
output_file_path <- file.path(output_folder, output_file_name)

# Save the STN-i result to the specified output file
save_stn_i_data(stn_i_result = stn_i_result, output_file_path = output_file_path)

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
