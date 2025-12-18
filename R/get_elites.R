# nolint start

#########################################################################
# Get Elite Configurations Script
# Author: Pablo Estobar
#
# Description:
# This script processes configuration CSV files from multiple scenarios
# and extracts all elite configurations, creating two output files:
# 1. A tab-separated .txt file containing unique elite configurations
# 2. A CSV file mapping scenarios to elite configurations with ESCENARIO info
#
# Usage:
# Rscript get_elites.R --directories=<dir1,dir2,...> --output=<output_dir> /
#                      --name=<output_name> /
#                      [--parameters=<parameters_file>] /
#                      [--best_elites=<TRUE|FALSE>] /
#                      [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --directories        : (Required) Comma-separated list of parent directories containing Results subdirectories
# --output            : (Required) Directory where output files will be saved
# --name             : (Required) Base name for output files
# --parameters        : (Optional) Path to Parameters.csv file. If not provided, will search in output or parent directory
# --best_elites       : (Optional) Only keep the best elite per run using IS_BEST column (default: FALSE)
# --verbose          : (Optional) Whether to show detailed processing information (default: FALSE)
#
# Requirements:
# - R with the following packages installed:
#     - optparse
#
# Notes:
# - Searches for configurations.csv files in Results/{SEED_ID}/ subdirectories
# - Extracts rows where IS_ELITE == TRUE
# - Output files will be named <name>_configs.txt and <name>_mapping.csv
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load required packages ----------
library(optparse)

# ---------- Load utility functions ----------
source("R/Functions/general_utils.R")

# Define command line options
option_list <- list(
    make_option(c("-d", "--directories"),
                type = "character",
                help = "Comma-separated list of parent directories containing Results subdirectories"),
    
    make_option(c("-o", "--output"),
                type = "character",
                help = "Directory where output files will be saved"),
    
    make_option(c("-n", "--name"),
                type = "character",
                help = "Base name for output files"),
    
    make_option(c("-p", "--parameters"),
                type = "character",
                default = NULL,
                help = "Path to Parameters.csv file [default: auto-detect]"),
    
    make_option(c("-b", "--best_elites"),
                type = "logical",
                default = FALSE,
                help = "Only keep the best elite per run using IS_BEST column [default= %default]"),
    
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

# Validate best_elites argument
if (!is.logical(opt$best_elites)) {
    stop("The 'best_elites' argument must be TRUE or FALSE")
}

# Process paths
directories <- strsplit(opt$directories, ",")[[1]]
directories <- normalizePath(directories, mustWork = TRUE)
output_dir <- normalizePath(opt$output, mustWork = FALSE)

# Create output directory if needed
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# Define output file paths
configs_file <- file.path(output_dir, paste0(opt$name, "_configs.txt"))
mapping_file <- file.path(output_dir, paste0(opt$name, "_mapping.csv"))

# Try to find parameters file
params_file <- NULL
if (!is.null(opt$parameters) && file.exists(opt$parameters)) {
    # Use provided parameters file if it exists
    params_file <- opt$parameters
} else {
    # Try to find in output directory
    candidate <- file.path(output_dir, "Parameters.csv")
    if (file.exists(candidate)) {
        params_file <- candidate
    } else {
        # Try parent directory
        candidate <- file.path(dirname(output_dir), "Parameters.csv")
        if (file.exists(candidate)) {
            params_file <- candidate
        }
    }
}

# Read parameters if available
parameters_info <- NULL
if (!is.null(params_file)) {
    if (opt$verbose) cat(sprintf("Reading parameters file from %s...\n", params_file))
    parameters_info <- read.csv(params_file, sep = ";", stringsAsFactors = FALSE)
    colnames(parameters_info) <- trimws(tolower(colnames(parameters_info)))
} else {
    if (opt$verbose) cat("Warning: Parameters file not found. Categorical types may not be properly formatted.\n")
}

# Function to format parameter values according to their type
format_parameter_value <- function(param_name, value, parameters_info) {
    if (is.null(parameters_info)) {
        return(value)
    }
    
    param_row <- parameters_info[tolower(parameters_info$name) == tolower(param_name), ]
    if (nrow(param_row) == 0) {
        return(value)
    }
    
    param_type <- param_row$type[1]
    
    # Handle NA values
    if (is.na(value)) {
        return("NA")
    }
    
    # Format categorical parameters with quotes
    if (param_type == "c") {
        return(paste0('"', as.character(value), '"'))
    }
    
    # Format numeric and integer parameters without quotes
    return(as.character(value))
}

if (opt$verbose) cat("Processing directories for configurations.csv files...\n")
all_elite_configs_list <- list()
scenario_mapping <- data.frame(
    ESCENARIO = character(),
    SEED_ID = character(),
    CONFIG_ID = integer(),
    stringsAsFactors = FALSE
)

# Process each parent directory
for (parent_dir in directories) {
    if (opt$verbose) cat(sprintf("Searching in %s...\n", parent_dir))
    
    # Extract ESCENARIO name from parent directory path
    # Path structure: .../Individuals/{ESCENARIO}/Results/{SEED_ID}/
    path_parts <- strsplit(parent_dir, "/")[[1]]
    ESCENARIO <- NA
    
    # Find "Individuals" in the path and get the next element as ESCENARIO
    for (i in seq_along(path_parts)) {
        if (path_parts[i] == "Individuals" && i < length(path_parts)) {
            ESCENARIO <- path_parts[i + 1]
            break
        }
    }
    
    if (is.na(ESCENARIO)) {
        if (opt$verbose) cat(sprintf("  Warning: Could not extract ESCENARIO name from %s\n", parent_dir))
        ESCENARIO <- "UNKNOWN"
    }
    
    # Check if parent_dir already ends with Results
    if (basename(parent_dir) == "Results") {
        results_dir <- parent_dir
    } else {
        results_dir <- file.path(parent_dir, "Results")
    }
    
    if (dir.exists(results_dir)) {
        # Get all scenario subdirectories
        scenario_dirs <- list.dirs(results_dir, recursive = FALSE, full.names = TRUE)
        
        for (scenario_dir in scenario_dirs) {
            SEED_ID <- basename(scenario_dir)
            config_file <- file.path(scenario_dir, "configurations.csv")
            
            if (file.exists(config_file)) {
                if (opt$verbose) cat(sprintf("  Reading %s...\n", config_file))
                
                # Read the configurations file
                configs <- read.csv(config_file, sep = ";", stringsAsFactors = FALSE)
                
                # Filter only elite configurations
                if ("IS_ELITE" %in% colnames(configs)) {
                    elite_configs <- configs[configs$IS_ELITE == TRUE, ]
                    
                    if (nrow(elite_configs) > 0) {
                        # If best_elites is TRUE, filter to only the best elite using IS_BEST column
                        if (opt$best_elites) {
                            if ("IS_BEST" %in% colnames(elite_configs)) {
                                best_configs <- elite_configs[elite_configs$IS_BEST == TRUE, ]
                                
                                if (nrow(best_configs) > 0) {
                                    elite_configs <- best_configs
                                    
                                    if (opt$verbose) {
                                        cat(sprintf("    Selected best elite: CONFIG_ID=%s (marked as IS_BEST=TRUE)\n",
                                                   best_configs$CONFIG_ID[1]))
                                    }
                                } else {
                                    if (opt$verbose) {
                                        cat("    Warning: No configuration marked as IS_BEST=TRUE, keeping all elites\n")
                                    }
                                }
                            } else {
                                if (opt$verbose) {
                                    cat("    Warning: IS_BEST column not found in configurations.csv, keeping all elites\n")
                                }
                            }
                        }
                        
                        # Store elite configurations with scenario context
                        for (i in 1:nrow(elite_configs)) {
                            CONFIG_ID <- elite_configs[i, "CONFIG_ID"]
                            config_key <- paste(ESCENARIO, SEED_ID, CONFIG_ID, sep = "_")
                            
                            # Store the full config row
                            all_elite_configs_list[[config_key]] <- list(
                                ESCENARIO = ESCENARIO,
                                SEED_ID = SEED_ID,
                                CONFIG_ID = CONFIG_ID,
                                config_row = elite_configs[i, ]
                            )
                            
                            # Add mapping entry
                            mapping_entry <- data.frame(
                                ESCENARIO = ESCENARIO,
                                SEED_ID = SEED_ID,
                                CONFIG_ID = CONFIG_ID,
                                stringsAsFactors = FALSE
                            )
                            scenario_mapping <- rbind(scenario_mapping, mapping_entry)
                        }
                        
                        if (opt$verbose) {
                            cat(sprintf("    Found %d elite configurations\n", nrow(elite_configs)))
                        }
                    }
                }
            }
        }
    }
}

if (opt$verbose) cat("Processing elite configurations and formatting parameters...\n")

# Extract parameter columns (exclude CONFIG_ID and IS_ELITE)
if (length(all_elite_configs_list) > 0) {
    # Get first config to determine parameter columns
    first_config <- all_elite_configs_list[[1]]$config_row
    param_cols <- setdiff(colnames(first_config), c("CONFIG_ID", "IS_ELITE", "IS_BEST"))
    
    # Build unique configurations with proper formatting
    unique_configs_list <- list()
    config_seen <- character()
    
    for (config_key in names(all_elite_configs_list)) {
        config_row <- all_elite_configs_list[[config_key]]$config_row
        
        # Extract parameter values and format them according to type
        formatted_row <- list()
        raw_values <- c()  # For uniqueness check
        
        for (param in param_cols) {
            raw_value <- config_row[[param]]
            raw_values <- c(raw_values, as.character(raw_value))
            
            # Format value according to parameter type
            formatted_value <- format_parameter_value(param, raw_value, parameters_info)
            formatted_row[[param]] <- formatted_value
        }
        
        # Create signature for uniqueness check (use raw values)
        config_sig <- paste(raw_values, collapse = "|")
        
        if (!(config_sig %in% config_seen)) {
            config_seen <- c(config_seen, config_sig)
            unique_configs_list[[length(unique_configs_list) + 1]] <- as.data.frame(formatted_row, stringsAsFactors = FALSE)
        }
    }
    
    # Combine into data frame
    if (length(unique_configs_list) > 0) {
        unique_configs <- do.call(rbind, unique_configs_list)
        rownames(unique_configs) <- NULL
    } else {
        unique_configs <- data.frame()
    }
} else {
    unique_configs <- data.frame()
}

if (opt$verbose) cat("Writing output files...\n")

# Write configurations file (tab-separated)
if (nrow(unique_configs) > 0) {
    # Create output content as matrix
    output_matrix <- rbind(colnames(unique_configs), unique_configs)
    
    # Convert to character matrix for writing
    output_char <- apply(output_matrix, 1, function(row) {
        paste(row, collapse = "\t")
    })
    
    # Write to file
    writeLines(output_char, configs_file)
}

# Write mapping file
write.table(scenario_mapping,
            file = mapping_file,
            sep = ";",
            row.names = FALSE,
            quote = FALSE)

if (opt$verbose) {
    cat(sprintf("Found %d unique elite configurations across %d scenario references\n",
                nrow(unique_configs),
                nrow(scenario_mapping)))
    cat(sprintf("Results saved to:\n  %s\n  %s\n",
                configs_file,
                mapping_file))
}

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end