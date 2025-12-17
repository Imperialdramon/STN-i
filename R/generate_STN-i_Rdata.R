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
# of runs to consider, and saves the resulting STN-i as a .Rdata file.
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
#                          STN-i object (.Rdata) will be saved.
#
# --output_file          : (Optional) Name of the output file (default: input file name
#                          without extension + "_stn_i.Rdata").
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
source("R/Functions/network_utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Path to the input file (.csv) containing STN-i trace data"),

  make_option(c("-o", "--output"),
              type="character",
              help="Path to the output folder where the resulting STN-i object (.Rdata) will be saved"),

  make_option(c("-f", "--output_file"),
              type="character",
              default=NULL,
              help="Name of the output file [default= input_name_stn_i.Rdata]"),

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
  output_file_name <- paste0(input_basename, "_stn_i.Rdata")
} else {
  output_file_name <- opt$output_file
  if (!grepl("\\.Rdata$", output_file_name)) {
    output_file_name <- paste0(output_file_name, ".Rdata")
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

# ---------- Functions ----------

#' Create a STN-i from a trace file
#' 
#' This function reads a trace file and creates a STN-i (Solution Trace Network) graph.
#' 
#' @param input_file A string specifying the path to the input trace file.
#' @param problem_type A string specifying the type of problem, either "min" for minimization or "max" for maximization. Default is "min".
#' @param best_known_solution A numeric value representing the best known solution for the problem. Default is NA, which means it will be calculated from the data.
#' 
#' @param number_of_runs An integer specifying the number of runs to consider from the trace file. Default is NA, which means it will use the maximum run number found in the data.
#' @param separator A string specifying the separator used in the input file. Default is "".
#' @param network_name A string specifying the name of the STN-i network. Default is "STN_i".
#' 
#' @return A list containing the STN-i graph, problem type, best known solution, number of runs, and summaries of elite and type origins.
#' 
#' @examples
#' \dontrun{
#' stn_i_result <- stn_i_create("path/to/trace_file.txt", problem_type = "min", best_known_solution = 0.5, network_name = "My_STN_i")
#' }
stn_i_create <- function(input_file, problem_type = "min", best_known_solution = NA, number_of_runs = NA, network_name = "STN_i") {
  # Load the input file data with updated column structure
  trace_all <- read.csv2(input_file, header = TRUE, stringsAsFactors = FALSE)

  # Verify expected columns are present (new format with counters)
  expected_columns <- c(
    "RUN_ID", "PATH",
    "ITERATION_1", "SOLUTION_1", "QUALITY_1",
    "CONFIG_COUNT_1", "ELITE_COUNT_1", "REGULAR_COUNT_1",
    "START_COUNT_1", "STANDARD_COUNT_1", "END_COUNT_1",
    "ELITE_START_COUNT_1", "ELITE_STANDARD_COUNT_1", "ELITE_END_COUNT_1",
    "REGULAR_START_COUNT_1", "REGULAR_STANDARD_COUNT_1", "REGULAR_END_COUNT_1",
    "ITERATION_2", "SOLUTION_2", "QUALITY_2",
    "CONFIG_COUNT_2", "ELITE_COUNT_2", "REGULAR_COUNT_2",
    "START_COUNT_2", "STANDARD_COUNT_2", "END_COUNT_2",
    "ELITE_START_COUNT_2", "ELITE_STANDARD_COUNT_2", "ELITE_END_COUNT_2",
    "REGULAR_START_COUNT_2", "REGULAR_STANDARD_COUNT_2", "REGULAR_END_COUNT_2"
  )
  
  missing_columns <- setdiff(expected_columns, names(trace_all))
  if (length(missing_columns) > 0) {
    stop("Missing required columns in input file: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  
  # Convert numeric columns to numeric type (they might be read as characters)
  numeric_columns <- c(
    "RUN_ID", "ITERATION_1", "QUALITY_1",
    "CONFIG_COUNT_1", "ELITE_COUNT_1", "REGULAR_COUNT_1",
    "START_COUNT_1", "STANDARD_COUNT_1", "END_COUNT_1",
    "ELITE_START_COUNT_1", "ELITE_STANDARD_COUNT_1", "ELITE_END_COUNT_1",
    "REGULAR_START_COUNT_1", "REGULAR_STANDARD_COUNT_1", "REGULAR_END_COUNT_1",
    "ITERATION_2", "QUALITY_2",
    "CONFIG_COUNT_2", "ELITE_COUNT_2", "REGULAR_COUNT_2",
    "START_COUNT_2", "STANDARD_COUNT_2", "END_COUNT_2",
    "ELITE_START_COUNT_2", "ELITE_STANDARD_COUNT_2", "ELITE_END_COUNT_2",
    "REGULAR_START_COUNT_2", "REGULAR_STANDARD_COUNT_2", "REGULAR_END_COUNT_2"
  )
  
  for (col in numeric_columns) {
    trace_all[[col]] <- as.numeric(trace_all[[col]])
  }
  
  # Convert PATH to logical
  trace_all$PATH <- as.logical(trace_all$PATH)

  # Check if the number of runs is specified, if not, use the maximum run number in the data
  if (is.na(number_of_runs)) {
    number_of_runs <- max(trace_all$RUN_ID, na.rm = TRUE)
  } else {
    if (number_of_runs < 1) {
      stop("number_of_runs must be a positive integer.", call. = FALSE)
    }
  }

  # Filter the trace data to include only the specified number of runs
  trace_all <- trace_all[trace_all$RUN_ID <= number_of_runs,]

  # Initialize maps to store nodes and edges
  nodes_map <- list()
  edges_map <- list()

  # Process all runs
  for (i in 1:number_of_runs) {
    # Take by run
    trace <- trace_all[trace_all$RUN_ID == i, ]

    for (j in 1:nrow(trace)) {
      # Add only the second node (destination) to avoid over-representation
      node_key <- trace$SOLUTION_2[j]
      
      # Only add node if not already present
      if (is.null(nodes_map[[node_key]])) {
        nodes_map[[node_key]] <- data.frame(
          Node = trace$SOLUTION_2[j],
          FITNESS = trace$QUALITY_2[j],
          CONFIGURATIONS = trace$CONFIG_COUNT_2[j],
          ELITES = trace$ELITE_COUNT_2[j],
          REGULARS = trace$REGULAR_COUNT_2[j],
          IS_ELITE = ifelse(trace$ELITE_COUNT_2[j] > 0, TRUE, FALSE),
          STARTS = trace$START_COUNT_2[j],
          STANDARDS = trace$STANDARD_COUNT_2[j],
          ENDS = trace$END_COUNT_2[j],
          TOPOLOGY = if (trace$START_COUNT_2[j] > 0) {
            'START'
          } else if (trace$END_COUNT_2[j] > 0) {
            'END'
          } else {
            'STANDARD'
          },
          ELITE_STARTS = trace$ELITE_START_COUNT_2[j],
          ELITE_STANDARDS = trace$ELITE_STANDARD_COUNT_2[j],
          ELITE_ENDS = trace$ELITE_END_COUNT_2[j],
          REGULAR_STARTS = trace$REGULAR_START_COUNT_2[j],
          REGULAR_STANDARDS = trace$REGULAR_STANDARD_COUNT_2[j],
          REGULAR_ENDS = trace$REGULAR_END_COUNT_2[j],
          stringsAsFactors = FALSE
        )
      }
      
      # Add edges (only paths, no self-loops)
      if (trace$PATH[j] == TRUE) {
        edge_key <- paste(trace$SOLUTION_1[j], trace$SOLUTION_2[j], sep = "|")
        if (is.null(edges_map[[edge_key]])) {
          edges_map[[edge_key]] <- list(
            Start = trace$SOLUTION_1[j],
            End = trace$SOLUTION_2[j],
            weight = 1
          )
        } else {
          edges_map[[edge_key]]$weight <- edges_map[[edge_key]]$weight + 1
        }
      }
    }
  }

  # Convert maps to data frames
  nodes_aggregated <- do.call(rbind, nodes_map)
  rownames(nodes_aggregated) <- NULL
  
  edges_aggregated <- do.call(rbind, lapply(edges_map, function(x) {
    data.frame(Start = x$Start, End = x$End, weight = x$weight, stringsAsFactors = FALSE)
  }))
  rownames(edges_aggregated) <- NULL

  # Create STN_i graph and remove self loops
  STN_i <- graph_from_data_frame(d = edges_aggregated, directed = TRUE, vertices = nodes_aggregated)
  STN_i <- simplify(STN_i, remove.multiple = FALSE, remove.loops = TRUE)

  # Obtain the fitness values from graph
  fitness_vals <- V(STN_i)$FITNESS

  # Determine BEST nodes and best known solution if not provided
  V(STN_i)$IS_BEST <- FALSE
  if (problem_type == "min") {
    if (!is.na(best_known_solution)) {
      best_known_solution <- as.numeric(best_known_solution)
    } else {
      best_known_solution <- min(fitness_vals, na.rm = TRUE)
    }
    V(STN_i)$IS_BEST <- fitness_vals <= best_known_solution
  } else {
    if (!is.na(best_known_solution)) {
      best_known_solution <- as.numeric(best_known_solution)
    } else {
      best_known_solution <- max(fitness_vals, na.rm = TRUE)
    }
    V(STN_i)$IS_BEST <- fitness_vals >= best_known_solution
  }

  # Classify edges as IMPROVING, WORSENING, or EQUAL
  fitness_from <- fitness_vals[as.numeric(head_of(STN_i, E(STN_i)))]
  fitness_to <- fitness_vals[as.numeric(tail_of(STN_i, E(STN_i)))]
  
  # Initialize edge types - use a character vector approach
  edge_types <- rep("EQUAL", ecount(STN_i))
  
  if (ecount(STN_i) > 0) {
    if (problem_type == "min") {
      # For minimization: improving means fitness_to < fitness_from
      edge_types[fitness_to < fitness_from] <- "IMPROVING"
      edge_types[fitness_to > fitness_from] <- "WORSENING"
    } else {
      # For maximization: improving means fitness_to > fitness_from
      edge_types[fitness_to > fitness_from] <- "IMPROVING"
      edge_types[fitness_to < fitness_from] <- "WORSENING"
    }
  }
  
  # Assign all edge types at once
  E(STN_i)$Type <- edge_types

  # Return all data without save
  return(list(
    STN_i = STN_i,
    network_name = network_name,
    number_of_runs = number_of_runs,
    problem_type = problem_type,
    best_known_solution = best_known_solution
  ))
}

#' Save the STN_i-i data to a R file
#'
#' This function saves the STN_i-i data to a specified output folder with a given file name.
#'
#' @param stn_i_result The result of the STN_i-i analysis, typically an object containing the STN_i graph and related data.
#' @param output_file_path A string specifying the path to the output folder where the RData file will be saved.
#' @param verbose A logical value indicating whether to print detailed information during the saving process. Default is FALSE.
#'
#' @return None. The function writes the file to disk.
#'
#' @examples
#' \dontrun{
#' save_stn_i_data(stn_i_result, "output/", "custom_name.Rdata")
#' }
save_stn_i_data <- function(stn_i_result, output_file_path, verbose = FALSE) {

  # Save the STN-i result as an RData file
  save(stn_i_result, file = output_file_path)

  if (verbose) {
    cat("General STN-i summary:\n")
    cat("Number of Runs:", stn_i_result$number_of_runs, "\n")
    cat("Problem Type:", stn_i_result$problem_type, "\n")
    cat("Best Known Solution:", stn_i_result$best_known_solution, "\n")
    cat("Number of Nodes:", vcount(stn_i_result$STN_i), "\n")
    cat("Number of Edges:", ecount(stn_i_result$STN_i), "\n")
    cat("Number of Best Nodes:", sum(V(stn_i_result$STN_i)$BEST), "\n")
    
    # Print statistics about configurations
    cat("\nConfiguration Statistics:\n")
    cat("Total Configurations:", sum(V(stn_i_result$STN_i)$CONFIGURATIONS), "\n")
    cat("Total Elite Configurations:", sum(V(stn_i_result$STN_i)$ELITES), "\n")
    cat("Total Regular Configurations:", sum(V(stn_i_result$STN_i)$REGULARS), "\n")
    
    cat("\nTopology Statistics (normalized):\n")
    cat("Start Configurations:", sum(V(stn_i_result$STN_i)$STARTS), "\n")
    cat("Standard Configurations:", sum(V(stn_i_result$STN_i)$STANDARDS), "\n")
    cat("End Configurations:", sum(V(stn_i_result$STN_i)$ENDS), "\n")

    # Print a message indicating where the file was saved
    message(paste("STN-i data saved to:", output_file_path))
  }
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
save_stn_i_data(
  stn_i_result = stn_i_result,
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
