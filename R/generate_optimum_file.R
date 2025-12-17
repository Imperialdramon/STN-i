# nolint start
#########################################################################
# Optimum File Generation Script
#
# Description:
# This script processes multiple Rdata files containing irace results
# and generates a single CSV file listing the best quality values
# for each instance across all files.
#
# Usage:
# Rscript generate_optimum_file.R --input=<input_directory> --output=<output_directory> 
#                                [--name=<output_file_name>] 
#                                [--best=<min|max>]
#
# Arguments:
# --input         : (Optional) Path to a single input directory containing Rdata files.
# --directories   : (Optional) Comma-separated paths to multiple input directories containing Rdata files.
# --output        : (Required) Path to the output directory where the optimum CSV file will be saved.
# --name          : (Optional) Name of the output CSV file (default: "Optimum.csv").
# --best          : (Optional) Criteria for selecting the best value ('min' or 'max', default: "min").
#
# Requirements:
# - R with the following packages installed:
#     - tools
#     - irace
#     - utils
#     - optparse
# Notes:
# - At least one input directory must be provided.
# - The output will be saved as a CSV file containing the best values for each instance.
# - Designed for execution from command line using named arguments.
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("tools", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("irace", quietly = TRUE)) {
  stop("Error: The irace package is not installed. Please install it with 'install.packages(\"irace\")'", call. = FALSE)
}
if (!requireNamespace("utils", quietly = TRUE)) {
  stop("Error: The utils package is not installed. Please install it with 'install.packages(\"utils\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(tools)
library(irace)
library(utils)
library(optparse)

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Directorio de entrada individual con los archivos .Rdata"),
              
  make_option(c("-d", "--directories"),
              type="character",
              help="Directorios de entrada separados por comas con los archivos .Rdata"),
              
  make_option(c("-o", "--output"),
              type="character",
              help="Directorio de salida para el archivo de óptimos"),
              
  make_option(c("-n", "--name"),
              type="character",
              default="Optimum.csv",
              help="Nombre del archivo de salida [default= %default]"),
              
  make_option(c("-b", "--best"),
              type="character", 
              default="min",
              help="Criterio para seleccionar el mejor valor ('min' o 'max') [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$input) && is.null(opt$directories)) {
  stop("Se requiere al menos un directorio de entrada (-i/--input o -d/--directories)")
}
if (is.null(opt$output)) {
  stop("El directorio de salida es requerido (-o/--output)")
}

# Process input directories
input_dirs <- c()

# Process single input directory if provided
if (!is.null(opt$input)) {
  input_dir <- normalizePath(opt$input)
  if (!dir.exists(input_dir)) {
    stop(sprintf("El directorio de entrada no existe: %s", input_dir))
  }
  input_dirs <- c(input_dirs, input_dir)
}

# Process multiple directories if provided
if (!is.null(opt$directories)) {
  # Split directories by comma and trim whitespace
  multiple_dirs <- strsplit(opt$directories, ",")[[1]]
  multiple_dirs <- trimws(multiple_dirs)
  
  for (dir in multiple_dirs) {
    dir_path <- normalizePath(dir)
    if (!dir.exists(dir_path)) {
      stop(sprintf("El directorio de entrada no existe: %s", dir_path))
    }
    input_dirs <- c(input_dirs, dir_path)
  }
}

# Create output directory if it doesn't exist
output_dir <- normalizePath(opt$output, mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Validate if best criteria is either 'min' or 'max'
if (!(opt$best %in% c("min", "max"))) {
  stop("El criterio 'best' debe ser 'min' o 'max'")
}

# Function to process Rdata files and find best values
process_optimum_files <- function(input_dir, best_criteria = "min") {
  # List all Rdata files
  files <- list.files(input_dir, pattern = "\\.Rdata$", full.names = TRUE, recursive = TRUE)
  
  # Initialize storage for instance results
  instance_results <- list()
  
  # Process each Rdata file
  for (file in files) {
    cat("Procesando archivo:", basename(file), "\n")
    
    tryCatch({
      # Load irace results with suppressed output
      irace_results <- suppressMessages(suppressWarnings(read_logfile(file)))
      
      if (!is.null(irace_results$experiments) && !is.null(irace_results$scenario$instances)) {
        # Get instance names
        instances <- basename(irace_results$scenario$instances)
        experiment_ids <- as.integer(rownames(irace_results$experiments))
        
        # Process each instance
        for (i in seq_along(experiment_ids)) {
          exp_id <- experiment_ids[i]
          instance_name <- instances[irace_results$state$instances_log[exp_id]$instanceID]
          qualities <- as.numeric(irace_results$experiments[i, ])
          
          # Remove NA values
          qualities <- qualities[!is.na(qualities)]
          
          if (length(qualities) > 0) {
            # Get best quality based on criteria
            best_quality <- if (best_criteria == "min") {
              min(qualities)
            } else {
              max(qualities)
            }
            
            # Update instance results if better quality found
            if (is.null(instance_results[[instance_name]]) || 
                (best_criteria == "min" && best_quality < instance_results[[instance_name]]) ||
                (best_criteria == "max" && best_quality > instance_results[[instance_name]])) {
              instance_results[[instance_name]] <- best_quality
            }
          }
        }
      }
    }, error = function(e) {
      cat("Error procesando archivo", basename(file), ":", conditionMessage(e), "\n")
    })
  }
  
  # Convert results to data frame
  results_df <- data.frame(
    INSTANCE = names(instance_results),
    BEST = unlist(instance_results),
    stringsAsFactors = FALSE
  )
  
  return(results_df)
}

# Process files and generate optimum file
cat("Procesando archivos Rdata para encontrar valores óptimos...\n")
instance_results <- list()

# Process each input directory
for (dir in input_dirs) {
  cat(sprintf("Procesando directorio: %s\n", dir))
  optimum_df <- process_optimum_files(dir, opt$best)
  
  # Update instance results with better values
  for (i in seq_len(nrow(optimum_df))) {
    instance_name <- optimum_df$INSTANCE[i]
    new_best <- optimum_df$BEST[i]
    
    if (is.null(instance_results[[instance_name]]) ||
        (opt$best == "min" && new_best < instance_results[[instance_name]]) ||
        (opt$best == "max" && new_best > instance_results[[instance_name]])) {
      instance_results[[instance_name]] <- new_best
    }
  }
}

# Convert final results to data frame
optimum_df <- data.frame(
  INSTANCE = names(instance_results),
  BEST = unlist(instance_results),
  stringsAsFactors = FALSE
)

# Save results
output_file <- file.path(output_dir, opt$name)
write.table(optimum_df, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
cat(sprintf("Archivo de óptimos guardado en: %s\n", output_file))

# ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
