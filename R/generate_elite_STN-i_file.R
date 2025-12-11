# nolint start

#########################################################################
# Elite STN-i File Generation Script
# Author: Pablo Estobar
#
# Description:
# This script processes elite configurations from irace testing results and
# generates STN-i files for each scenario. It creates configurations, trajectories,
# instances, and results files based on testing data from elite configurations.
#
# Usage:
# Rscript generate_elite_STN-i_file.R --elites_dir=<elites_dir> --directories=<dir1,dir2,...> /
#                                     --output=<output_dir> --parameters=<params_file> /
#                                     [--best=<min|max>] [--na_ranking=<TRUE|FALSE>] /
#                                     [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --elites_dir     : (Required) Directory containing elite data (irace.Rdata and mapping)
# --directories    : (Required) Comma-separated list of scenario directories with .Rdata files
# --output         : (Required) Output directory for elite STN-i files
# --parameters     : (Required) CSV file with parameters definition
# --best           : (Optional) Criterion for best value ('min' or 'max', default: 'min')
# --na_ranking     : (Optional) Consider NA as worst value in rankings (default: FALSE)
# --verbose        : (Optional) Show detailed processing information (default: FALSE)
#
# Requirements:
# - R with the following packages installed:
#     - irace
#     - optparse
#
# Notes:
# - The elites_dir must contain irace.Rdata with testing results
# - Each scenario directory should contain .Rdata files with irace results
# - Output will follow the structure: output_dir/ScenarioName/Results/RunID/...
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

# TODO: Utilizar múltiples archivos .Rdata por cada ejecución de testing elite
# TODO: Construir nuevos archivos basados en los archivos de configurations y trayectories, aprovechando el mapping se configuraciones, que permitirá idenfificar el escenario y semilla al que va cada configuración elite.
# TODO: Construir archivo de instances de élite basado en los experimentos de testing, considerando el optimum como el mejor valor entre todas las configuraciones elite.
# TODO: Traer todas las funciones a este archivo.

# Define command line options
option_list <- list(
    make_option(c("-e", "--elites_dir"),
                type = "character",
                help = "Directory containing elite data (irace.Rdata and mapping)"),
    
    make_option(c("-d", "--directories"),
                type = "character",
                help = "Comma-separated list of scenario directories with .Rdata files"),
    
    make_option(c("-o", "--output"),
                type = "character",
                help = "Output directory for elite STN-i files"),
    
    make_option(c("-p", "--parameters"),
                type = "character",
                help = "CSV file with parameters definition"),
    
    make_option(c("-b", "--best"),
                type = "character",
                default = "min",
                help = "Criterion for best value ('min' or 'max') [default= %default]"),
    
    make_option(c("-n", "--na_ranking"),
                type = "logical",
                default = FALSE,
                help = "Consider NA as worst value in rankings [default= %default]"),
    
    make_option(c("-v", "--verbose"),
                type = "logical",
                default = FALSE,
                help = "Show detailed processing information [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$elites_dir)) {
    stop("Elites directory is required (-e/--elites_dir)")
}
if (is.null(opt$directories)) {
    stop("Directories list is required (-d/--directories)")
}
if (is.null(opt$output)) {
    stop("Output directory is required (-o/--output)")
}
if (is.null(opt$parameters)) {
    stop("Parameters file is required (-p/--parameters)")
}

# Validate best criteria
if (!(opt$best %in% c("min", "max"))) {
    stop("The 'best' criterion must be 'min' or 'max'")
}

# Process paths
elites_dir <- normalizePath(opt$elites_dir, mustWork = TRUE)
directories <- strsplit(opt$directories, ",")[[1]]
directories <- normalizePath(directories, mustWork = TRUE)
output_dir <- normalizePath(opt$output, mustWork = FALSE)
parameters_file <- normalizePath(opt$parameters, mustWork = TRUE)

# Read parameters
parameters <- read.csv(parameters_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")
parameters$NAME <- trimws(parameters$NAME)
parameters$TYPE <- trimws(parameters$TYPE)

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (opt$verbose) cat("Loading elite testing data...\n")

# Load elite testing data
irace_file <- file.path(elites_dir, "irace.Rdata")
if (!file.exists(irace_file)) {
    stop(sprintf("irace.Rdata not found in %s", elites_dir))
}
load(irace_file)

# Get testing results
if (is.null(iraceResults$testing)) {
    stop("No testing results found in irace.Rdata")
}

testing_experiments <- iraceResults$testing$experiments
testing_instances <- iraceResults$scenario$testInstances
testing_seeds <- iraceResults$testing$seeds

if (opt$verbose) {
    cat(sprintf("  Found %d testing configurations\n", ncol(testing_experiments)))
    cat(sprintf("  Found %d testing instances\n", nrow(testing_experiments)))
}

# Create instances dataframe for testing
instances_df <- data.frame(
    EXPERIMENT_ID = seq_len(nrow(testing_experiments)),
    INSTANCE_ID = seq_len(nrow(testing_experiments)),
    SEED = testing_seeds,  # Seeds from testing phase
    NAME = basename(testing_instances),
    OPTIMUM = NA,  # Will be calculated from testing data
    stringsAsFactors = FALSE
)

# Calculate optimum for each instance (best value across all configurations)
for (i in seq_len(nrow(testing_experiments))) {
    row_values <- testing_experiments[i, ]
    if (opt$best == "min") {
        instances_df$OPTIMUM[i] <- min(row_values, na.rm = TRUE)
    } else {
        instances_df$OPTIMUM[i] <- max(row_values, na.rm = TRUE)
    }
}

if (opt$verbose) cat("Processing scenarios...\n")

# Process each scenario directory
for (scenario_dir in directories) {
    # Get the scenario name from the parent directory (e.g., BH, BH-90, BL, BL-45)
    scenario_name <- basename(dirname(scenario_dir))
    
    if (opt$verbose) cat(sprintf("\nProcessing scenario: %s\n", scenario_name))
    
    # Get all .Rdata files in the scenario
    rdata_files <- list.files(scenario_dir, pattern = "\\.Rdata$", full.names = TRUE)
    
    if (length(rdata_files) == 0) {
        if (opt$verbose) cat(sprintf("  No .Rdata files found in %s, skipping...\n", scenario_dir))
        next
    }
    
    # Process each .Rdata file (each run)
    for (rdata_file in rdata_files) {
        run_name <- tools::file_path_sans_ext(basename(rdata_file))
        
        if (opt$verbose) cat(sprintf("  Processing run: %s\n", run_name))
        
        # Load the run data
        load(rdata_file)
        
        # Get elite IDs from this run
        elite_ids <- c()
        if (!is.null(iraceResults$allElites)) {
            for (iter in seq_along(iraceResults$allElites)) {
                elite_ids <- c(elite_ids, iraceResults$allElites[[iter]])
            }
            elite_ids <- unique(elite_ids)
        }
        
        if (length(elite_ids) == 0) {
            if (opt$verbose) cat("    No elite configurations found, skipping...\n")
            next
        }
        
        if (opt$verbose) cat(sprintf("    Found %d elite configurations\n", length(elite_ids)))
        
        # Create output directory for this run
        run_output_dir <- file.path(output_dir, scenario_name, "Results", run_name)
        dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
        
        # Filter testing experiments to only include elite configurations
        elite_config_names <- as.character(elite_ids)
        available_configs <- intersect(elite_config_names, colnames(testing_experiments))
        
        if (length(available_configs) == 0) {
            if (opt$verbose) cat("    No elite configurations found in testing data, skipping...\n")
            next
        }
        
        elite_experiments <- testing_experiments[, available_configs, drop = FALSE]
        
        if (opt$verbose) cat(sprintf("    Processing %d elite configurations with testing data\n", ncol(elite_experiments)))
        
        # Create configurations file
        configs_file <- file.path(run_output_dir, "configurations.csv")
        configs <- create_elite_configurations_file(
            iraceResults$allConfigurations,
            elite_ids,
            parameters,
            configs_file
        )
        
        # Create instances file
        instances_file <- file.path(run_output_dir, "instances.csv")
        write.table(instances_df, file = instances_file, sep = ";",
                   row.names = FALSE, quote = FALSE)
        
        # Create results file
        results_file <- file.path(run_output_dir, "results.csv")
        results <- create_results_file(
            elite_experiments,
            instances_df,
            results_file,
            opt$best,
            opt$na_ranking
        )
        
        # Create trajectories file (only elite paths)
        trajectories_file <- file.path(run_output_dir, "trajectories.csv")
        trajectories <- create_elite_trajectories_file(
            iraceResults,
            elite_ids,
            trajectories_file
        )
        
        if (opt$verbose) {
            cat(sprintf("    Created files in %s\n", run_output_dir))
            cat(sprintf("      - configurations.csv (%d configs)\n", nrow(configs)))
            cat(sprintf("      - instances.csv (%d instances)\n", nrow(instances_df)))
            cat(sprintf("      - results.csv (%d configs)\n", nrow(results)))
            cat(sprintf("      - trajectories.csv (%d paths)\n", nrow(trajectories)))
        }
    }
}

if (opt$verbose) cat("\nProcessing completed successfully.\n")

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end