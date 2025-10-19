# nolint start

#########################################################################
# Get Elite Configurations Script
# Author: Pablo Estobar
#
# Description:
# This script processes multiple irace .Rdata files and extracts all elite
# configurations, creating two output files:
# 1. A tab-separated .txt file containing unique elite configurations
# 2. A CSV file mapping runs to configurations
#
# Usage:
# Rscript get_elites.R --directories=<dir1,dir2,...> --output=<output_dir> /
#                      --name=<output_name> /
#                      --parameters=<parameters_file> /
#                      [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --directories        : (Required) Comma-separated list of directories containing .Rdata files
# --output            : (Required) Directory where output files will be saved
# --name             : (Required) Base name for output files
# --verbose          : (Optional) Whether to show detailed processing information (default: FALSE)
#
# Requirements:
# - R with the following packages installed:
#     - irace
#     - optparse
#
# Notes:
# - Input directories should contain valid irace .Rdata files
# - Output files will be named <name>_configs.txt and <name>_mapping.csv
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("irace", quietly = TRUE)) {
    stop("Error: The irace package is not installed. Please install it with 'install.packages(\"irace\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load required packages ----------
library(irace)
library(optparse)

# ---------- Load utility functions ----------
source("R/utils.R")

# Define command line options
option_list <- list(
    make_option(c("-d", "--directories"),
                type = "character",
                help = "Comma-separated list of directories containing .Rdata files"),
    
    make_option(c("-o", "--output"),
                type = "character",
                help = "Directory where output files will be saved"),
    
    make_option(c("-n", "--name"),
                type = "character",
                help = "Base name for output files"),
    
    make_option(c("-p", "--parameters"),
                type = "character",
                help = "Path to parameters CSV file"),
    
    make_option(c("-v", "--verbose"),
                type = "logical",
                default = FALSE,
                help = "Show detailed processing information [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$directories)) {
    stop("Directories list is required (-d/--directories)")
}
if (is.null(opt$output)) {
    stop("Output directory is required (-o/--output)")
}
if (is.null(opt$name)) {
    stop("Output name is required (-n/--name)")
}
if (is.null(opt$parameters)) {
    stop("Parameters file is required (-p/--parameters)")
}

# Process paths
directories <- strsplit(opt$directories, ",")[[1]]
directories <- normalizePath(directories, mustWork = TRUE)
output_dir <- normalizePath(opt$output, mustWork = FALSE)
parameters_file <- normalizePath(opt$parameters, mustWork = TRUE)

# Read parameters
parameters <- read.csv(parameters_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")
parameters$NAME <- trimws(parameters$NAME)
parameters$TYPE <- trimws(parameters$TYPE)

# Create output directory if needed
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# Define output file paths
configs_file <- file.path(output_dir, paste0(opt$name, "_configs.txt"))
mapping_file <- file.path(output_dir, paste0(opt$name, "_mapping.csv"))

if (opt$verbose) cat("Processing directories...\n")

# Store all run data
runs_data <- list()

# Process each directory
for (dir in directories) {
    if (opt$verbose) cat(sprintf("Reading from %s...\n", dir))
    
    # Get all .Rdata files
    rdata_files <- list.files(dir, pattern = "\\.Rdata$", full.names = TRUE)
    
    for (file in rdata_files) {
        if (opt$verbose) cat(sprintf("  Loading %s...\n", basename(file)))
        
        # Load the .Rdata file
        load(file)
        
        # Store the run data with the filename as its name
        runs_data[[basename(file)]] <- iraceResults
    }
}

if (opt$verbose) cat("Processing elite configurations...\n")

# Process elite configurations
result <- process_elite_configurations(
    runs_data,
    parameters
)

if (opt$verbose) cat("Writing output files...\n")

# Write configurations file
write.table(result$unique_elites, 
            file = configs_file,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

# Write mapping file
write.table(result$runs_mapping,
            file = mapping_file,
            sep = ";",
            row.names = FALSE,
            quote = FALSE)

if (opt$verbose) {
    cat(sprintf("Found %d unique elite configurations across %d runs\n",
                nrow(result$unique_elites),
                nrow(result$runs_mapping)))
    cat(sprintf("Results saved to:\n  %s\n  %s\n",
                configs_file,
                mapping_file))
}

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end