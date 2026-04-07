# nolint start

#########################################################################
# Elite Optimum File Generation Script
# Author: Pablo Estobar
#
# Description:
# This script processes multiple elite testing .Rdata files and generates
# a CSV file listing the best quality values for each instance across
# all elite configurations.
#
# Usage:
# Rscript generate_elite_optimum_file.R --input=<input_directory>
#                                       --output=<output_directory>
#                                       [--name=<output_file_name>]
#                                       [--best=<min|max>]
#
# Arguments:
# --input   : (Required) Directory containing elite testing .Rdata files.
# --output  : (Required) Path to the output directory where the optimum CSV will be saved.
# --name    : (Optional) Name of the output CSV file (default: "Optimum_Elite.csv").
# --best    : (Optional) Criteria for selecting best value ('min' or 'max', default: "min").
#
# Requirements:
# - R with optparse, tools, and irace packages installed.
#
# Notes:
# - Input .Rdata files must contain iraceResults with testing data.
# - Output CSV contains columns: INSTANCE, BEST
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("tools", quietly = TRUE)) {
  stop("Error: The tools package is not installed. Please install it with 'install.packages(\"tools\")'", call. = FALSE)
}
if (!requireNamespace("irace", quietly = TRUE)) {
  stop("Error: The irace package is not installed. Please install it with 'install.packages(\"irace\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load required packages ----------
library(tools)
library(irace)
library(optparse)

# ---------- Define command line options ----------
option_list <- list(
  make_option(c("-i", "--input"),
              type="character",
              help="Directory containing elite testing .Rdata files"),

  make_option(c("-o", "--output"),
              type="character",
              help="Output directory for optimum file"),

  make_option(c("-n", "--name"),
              type="character",
              default="Optimum_Elite.csv",
              help="Name of the output CSV file [default= %default]"),

  make_option(c("-b", "--best"),
              type="character",
              default="min",
              help="Criteria for selecting best value ('min' or 'max') [default= %default]")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# ---------- Validate required arguments ----------
if (is.null(opt$input)) {
  stop("Input directory is required (-i/--input)")
}
if (is.null(opt$output)) {
  stop("Output directory is required (-o/--output)")
}
if (!(opt$best %in% c("min", "max"))) {
  stop("best must be 'min' or 'max'")
}

# ---------- Process paths ----------
input_dir <- normalizePath(opt$input, mustWork = TRUE)
output_dir <- normalizePath(opt$output, mustWork = FALSE)
output_file <- file.path(output_dir, opt$name)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

testing_files <- list.files(input_dir, pattern = "\\.Rdata$", full.names = TRUE)

if (length(testing_files) == 0) {
  stop(sprintf("No .Rdata files found in %s", input_dir))
}

instance_optimums <- list()

for (file_path in testing_files) {
  tryCatch({
    load(file_path)
    
    if (is.null(iraceResults$testing)) {
      next
    }
    
    experiments <- iraceResults$testing$experiments
    test_instances <- iraceResults$scenario$testInstances
    instance_names <- basename(test_instances)
    
    for (j in seq_len(nrow(experiments))) {
      inst_name <- instance_names[j]
      row_values <- experiments[j, ]
      valid_values <- row_values[!is.na(row_values)]
      
      if (length(valid_values) > 0) {
        best_val <- if (opt$best == "min") {
          min(valid_values)
        } else {
          max(valid_values)
        }
        
        if (is.null(instance_optimums[[inst_name]])) {
          instance_optimums[[inst_name]] <- best_val
        } else {
          if (opt$best == "min") {
            instance_optimums[[inst_name]] <- min(instance_optimums[[inst_name]], best_val)
          } else {
            instance_optimums[[inst_name]] <- max(instance_optimums[[inst_name]], best_val)
          }
        }
      }
    }
  }, error = function(e) {
    # Skip files with errors
  })
}

sorted_instances <- sort(names(instance_optimums))

optimum_df <- data.frame(
  INSTANCE = sorted_instances,
  BEST = as.numeric(instance_optimums[sorted_instances]),
  stringsAsFactors = FALSE
)

write.csv(optimum_df, file = output_file, row.names = FALSE, quote = TRUE)

rm(list = ls())
gc()
quit(save = "no")

# nolint end
