# nolint start

#########################################################################
# File: generate_summarize_testing.R
# Author: Pablo Estobar
#
# Description:
# This script processes elite testing results and training data to generate
# base files for box plot analysis. It calculates normalized ranking gaps (MNRG)
# for configurations across all testing instances and creates summary files
# for visualization.
#
# Usage:
# Rscript generate_summarize_testing.R --testing_dir=<testing_dir> --scenarios_dir=<scenarios_dir> /
#                                         --output=<output_dir> --parameters=<params_file> /
#                                         [--problem_type=<min|max>] [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --testing_dir    : (Required) Directory containing elite testing .Rdata files
# --scenarios_dir  : (Required) Directory containing scenario subdirectories with training data
# --output         : (Required) Output directory for generated files
# --parameters     : (Required) CSV file with parameters definition
# --problem_type   : (Optional) Optimization objective ('min' or 'max', default: 'min')
# --verbose        : (Optional) Show detailed processing information (default: FALSE)
#
# Requirements:
# - R with the following packages installed:
#     - irace
#     - optparse
#
# Notes:
# - Testing directory should contain .Rdata files with elite testing results
# - Scenarios directory should have subdirectories with Data/ folders containing training .Rdata files
# - Output files: Optimum_Testing.csv, configurations_results.csv, and per-scenario run files
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

# ---------- Define command line options ----------
option_list <- list(
    make_option(c("-t", "--testing_dir"),
                type = "character",
                help = "Directory containing elite testing .Rdata files"),
    
    make_option(c("-s", "--scenarios_dir"),
                type = "character",
                help = "Directory containing scenario subdirectories with training data"),
    
    make_option(c("-o", "--output"),
                type = "character",
                help = "Output directory for generated files"),
    
    make_option(c("-p", "--parameters"),
                type = "character",
                help = "CSV file with parameters definition"),
    
    make_option(c("-m", "--problem_type"),
                type = "character",
                default = "min",
                help = "Optimization objective ('min' or 'max') [default= %default]"),
    
    make_option(c("-v", "--verbose"),
                type = "logical",
                default = FALSE,
                help = "Show detailed processing information [default= %default]")
)

# ---------- Parse command line arguments ----------
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# ---------- Validate required arguments ----------
if (is.null(opt$testing_dir)) {
    stop("Testing directory is required (-t/--testing_dir)")
}
if (is.null(opt$scenarios_dir)) {
    stop("Scenarios directory is required (-s/--scenarios_dir)")
}
if (is.null(opt$output)) {
    stop("Output directory is required (-o/--output)")
}
if (is.null(opt$parameters)) {
    stop("Parameters file is required (-p/--parameters)")
}

# Validate problem type
if (!(opt$problem_type %in% c("min", "max"))) {
    stop("The 'problem_type' must be 'min' or 'max'")
}

# ---------- Process paths ----------
testing_dir <- normalizePath(opt$testing_dir, mustWork = TRUE)
scenarios_dir <- normalizePath(opt$scenarios_dir, mustWork = TRUE)
output_dir <- normalizePath(opt$output, mustWork = FALSE)
parameters_file <- normalizePath(opt$parameters, mustWork = TRUE)

# Read parameters
parameters <- read.csv(parameters_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")
parameters$NAME <- trimws(parameters$NAME)
parameters$TYPE <- trimws(parameters$TYPE)

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (opt$verbose) cat("Starting box plot base files generation...\n")

# ---------- Step 1: Process testing data ----------
if (opt$verbose) cat("\n=== Processing testing data ===\n")

# Get all testing .Rdata files
testing_files <- list.files(testing_dir, pattern = "\\.Rdata$", full.names = TRUE)

if (length(testing_files) == 0) {
    stop(sprintf("No .Rdata files found in %s", testing_dir))
}

if (opt$verbose) cat(sprintf("Found %d testing files\n", length(testing_files)))

# Initialize data structures
all_matrices <- list()
all_instances <- c()
all_config_ids <- c()
instance_best <- list()

# Process each testing file
for (i in seq_along(testing_files)) {
    file_path <- testing_files[i]
    file_name <- basename(file_path)
    
    if (opt$verbose) cat(sprintf("  [%d/%d] Processing: %s\n", i, length(testing_files), file_name))
    
    # Load testing data
    load(file_path)
    
    if (is.null(iraceResults$testing)) {
        if (opt$verbose) cat("    Warning: No testing results found, skipping...\n")
        next
    }
    
    # Get testing experiments matrix
    experiments <- iraceResults$testing$experiments
    test_instances <- iraceResults$scenario$testInstances
    
    if (opt$verbose) {
        cat(sprintf("    Matrix dimensions: %d instances x %d configurations\n", 
                   nrow(experiments), ncol(experiments)))
    }
    
    # Get instance base names
    instance_names <- basename(test_instances)
    
    # Store all unique instances
    all_instances <- unique(c(all_instances, instance_names))
    
    # Store all unique configuration IDs
    all_config_ids <- unique(c(all_config_ids, colnames(experiments)))
    
    # Calculate best value for each instance
    for (j in seq_len(nrow(experiments))) {
        inst_name <- instance_names[j]
        row_values <- experiments[j, ]
        
        # Remove NA values for calculation
        valid_values <- row_values[!is.na(row_values)]
        
        if (length(valid_values) > 0) {
            if (opt$problem_type == "min") {
                best_val <- min(valid_values)
            } else {
                best_val <- max(valid_values)
            }
            
            # Update instance best (keep the best across all files)
            if (is.null(instance_best[[inst_name]])) {
                instance_best[[inst_name]] <- best_val
            } else {
                if (opt$problem_type == "min") {
                    instance_best[[inst_name]] <- min(instance_best[[inst_name]], best_val)
                } else {
                    instance_best[[inst_name]] <- max(instance_best[[inst_name]], best_val)
                }
            }
        }
    }
    
    # Store matrix with metadata
    all_matrices[[file_name]] <- list(
        matrix = experiments,
        instances = instance_names,
        config_ids = colnames(experiments)
    )
}

if (opt$verbose) {
    cat(sprintf("\nTotal unique instances: %d\n", length(all_instances)))
    cat(sprintf("Total unique configurations: %d\n", length(all_config_ids)))
}

# ---------- Step 2: Standardize matrices ----------
if (opt$verbose) cat("\n=== Standardizing matrices ===\n")

# Sort instances and configurations for consistent ordering
sorted_instances <- sort(all_instances)
sorted_config_ids <- sort(all_config_ids)

# Create standardized matrices
standardized_matrices <- list()

for (mat_name in names(all_matrices)) {
    mat_data <- all_matrices[[mat_name]]
    orig_matrix <- mat_data$matrix
    orig_instances <- mat_data$instances
    orig_configs <- mat_data$config_ids
    
    # Create new matrix with standard dimensions
    std_matrix <- matrix(NA, nrow = length(sorted_instances), ncol = length(sorted_config_ids))
    rownames(std_matrix) <- sorted_instances
    colnames(std_matrix) <- sorted_config_ids
    
    # Fill in values from original matrix
    for (i in seq_len(nrow(orig_matrix))) {
        inst_name <- orig_instances[i]
        std_row_idx <- which(sorted_instances == inst_name)
        
        for (j in seq_len(ncol(orig_matrix))) {
            config_id <- orig_configs[j]
            std_col_idx <- which(sorted_config_ids == config_id)
            
            std_matrix[std_row_idx, std_col_idx] <- orig_matrix[i, j]
        }
    }
    
    standardized_matrices[[mat_name]] <- std_matrix
}

if (opt$verbose) {
    cat(sprintf("Standardized matrix dimensions: %d instances x %d configurations\n",
               length(sorted_instances), length(sorted_config_ids)))
}

# ---------- Step 3: Calculate GAP, Ranking, and Normalization ----------
if (opt$verbose) cat("\n=== Calculating metrics ===\n")

processed_matrices <- list()

for (mat_name in names(standardized_matrices)) {
    if (opt$verbose) cat(sprintf("  Processing: %s\n", mat_name))
    
    std_matrix <- standardized_matrices[[mat_name]]
    
    # Step 3a: Calculate GAP for each cell
    gap_matrix <- matrix(NA, nrow = nrow(std_matrix), ncol = ncol(std_matrix))
    rownames(gap_matrix) <- rownames(std_matrix)
    colnames(gap_matrix) <- colnames(std_matrix)
    
    for (i in seq_len(nrow(std_matrix))) {
        inst_name <- rownames(std_matrix)[i]
        best_val <- instance_best[[inst_name]]
        
        for (j in seq_len(ncol(std_matrix))) {
            val <- std_matrix[i, j]
            
            if (!is.na(val) && !is.na(best_val)) {
                gap_matrix[i, j] <- abs(val - best_val)
            }
        }
    }
    
    # Step 3b: Calculate ranking for each row (lower GAP = better = lower rank)
    rank_matrix <- matrix(NA, nrow = nrow(gap_matrix), ncol = ncol(gap_matrix))
    rownames(rank_matrix) <- rownames(gap_matrix)
    colnames(rank_matrix) <- colnames(gap_matrix)
    
    for (i in seq_len(nrow(gap_matrix))) {
        row_vals <- gap_matrix[i, ]
        valid_idx <- !is.na(row_vals)
        
        if (any(valid_idx)) {
            # Rank: lower GAP gets lower rank (better)
            ranks <- rank(row_vals[valid_idx], ties.method = "average")
            rank_matrix[i, valid_idx] <- ranks
        }
    }
    
    # Step 3c: Normalize rankings to [0, 1] for each row
    norm_matrix <- matrix(NA, nrow = nrow(rank_matrix), ncol = ncol(rank_matrix))
    rownames(norm_matrix) <- rownames(rank_matrix)
    colnames(norm_matrix) <- colnames(rank_matrix)
    
    for (i in seq_len(nrow(rank_matrix))) {
        row_vals <- rank_matrix[i, ]
        valid_idx <- !is.na(row_vals)
        
        if (any(valid_idx)) {
            valid_vals <- row_vals[valid_idx]
            min_val <- min(valid_vals)
            max_val <- max(valid_vals)
            
            if (max_val > min_val) {
                # Normalize to [0, 1]
                normalized <- (valid_vals - min_val) / (max_val - min_val)
                norm_matrix[i, valid_idx] <- normalized
            } else {
                # All values are the same
                norm_matrix[i, valid_idx] <- 0
            }
        }
    }
    
    # Step 3d: Calculate mean per column (vertical mean per matrix)
    col_means <- colMeans(norm_matrix, na.rm = TRUE)
    
    processed_matrices[[mat_name]] <- list(
        gap = gap_matrix,
        rank = rank_matrix,
        normalized = norm_matrix,
        col_means = col_means
    )
}

# ---------- Step 4: Calculate mean of means across matrices ----------
if (opt$verbose) cat("\n=== Calculating mean of means ===\n")

# Create matrix to store column means from all matrices
means_matrix <- matrix(NA, nrow = length(processed_matrices), ncol = length(sorted_config_ids))
colnames(means_matrix) <- sorted_config_ids

for (i in seq_along(processed_matrices)) {
    mat_name <- names(processed_matrices)[i]
    col_means <- processed_matrices[[mat_name]]$col_means
    means_matrix[i, ] <- col_means
}

# Calculate mean of means for each configuration
config_mnrg <- colMeans(means_matrix, na.rm = TRUE)

if (opt$verbose) {
    cat(sprintf("Calculated MNRG for %d configurations\n", length(config_mnrg)))
    cat(sprintf("MNRG range: [%.4f, %.4f]\n", min(config_mnrg, na.rm = TRUE), max(config_mnrg, na.rm = TRUE)))
}

# ---------- Step 5: Create configuration dictionary ----------
if (opt$verbose) cat("\n=== Creating configuration dictionary ===\n")

# Load one testing file to get configuration details
load(testing_files[1])
all_configs <- iraceResults$allConfigurations

# Get parameter names
param_names <- intersect(parameters$NAME, colnames(all_configs))
param_types <- setNames(parameters$TYPE, parameters$NAME)

# Create configuration data frame
config_data <- data.frame(CONFIG_ID = sorted_config_ids, stringsAsFactors = FALSE)

# Add parameter values
for (pname in param_names) {
    config_data[[pname]] <- sapply(sorted_config_ids, function(id) {
        config_row <- all_configs[all_configs$.ID. == as.integer(id), , drop = FALSE]
        
        if (nrow(config_row) == 0 || is.na(config_row[[pname]])) {
            return(NA)
        }
        
        value <- config_row[[pname]]
        type <- param_types[[pname]]
        
        if (type %in% c("c", "o", "cat", "ord")) {
            return(as.character(value))
        } else if (type %in% c("i", "int", "i,log")) {
            return(as.character(as.integer(value)))
        } else if (type %in% c("r", "real", "r,log")) {
            return(as.character(as.numeric(value)))
        } else {
            return(as.character(value))
        }
    })
}

# Add MNRG values
config_data$MNRG <- config_mnrg[sorted_config_ids]

# ---------- Step 6: Process training data by scenario ----------
if (opt$verbose) cat("\n=== Processing training data by scenario ===\n")

# Get scenario directories
scenario_dirs <- list.dirs(scenarios_dir, recursive = FALSE, full.names = TRUE)

for (scenario_path in scenario_dirs) {
    scenario_name <- basename(scenario_path)
    data_dir <- file.path(scenario_path, "Data")
    
    if (!dir.exists(data_dir)) {
        if (opt$verbose) cat(sprintf("  Skipping %s (no Data directory)\n", scenario_name))
        next
    }
    
    if (opt$verbose) cat(sprintf("\n  Processing scenario: %s\n", scenario_name))
    
    # Get all .Rdata files in Data directory
    training_files <- list.files(data_dir, pattern = "\\.Rdata$", full.names = TRUE)
    
    if (length(training_files) == 0) {
        if (opt$verbose) cat("    No training files found, skipping...\n")
        next
    }
    
    if (opt$verbose) cat(sprintf("    Found %d training files\n", length(training_files)))
    
    # Create results data frame
    scenario_results <- data.frame(
        RUN_ID = character(),
        CONFIG_ID = character(),
        MNRG = numeric(),
        stringsAsFactors = FALSE
    )
    
    # Process each training file
    for (training_file in training_files) {
        run_name <- tools::file_path_sans_ext(basename(training_file))
        
        if (opt$verbose) cat(sprintf("      Processing run: %s\n", run_name))
        
        # Load training data
        load(training_file)
        
        # Get the best elite from the last iteration
        if (is.null(iraceResults$allElites) || length(iraceResults$allElites) == 0) {
            if (opt$verbose) cat("        Warning: No elites found, skipping...\n")
            next
        }
        
        last_iter <- length(iraceResults$allElites)
        best_elite_id <- iraceResults$allElites[[last_iter]][1]
        
        if (opt$verbose) cat(sprintf("        Best elite ID: %s\n", best_elite_id))
        
        # Get configuration values
        elite_config <- iraceResults$allConfigurations[iraceResults$allConfigurations$.ID. == best_elite_id, , drop = FALSE]
        
        # Create parameter signature to match with config_data
        param_signature <- sapply(param_names, function(pname) {
            if (nrow(elite_config) == 0 || is.na(elite_config[[pname]])) {
                return(NA)
            }
            
            value <- elite_config[[pname]]
            type <- param_types[[pname]]
            
            if (type %in% c("c", "o", "cat", "ord")) {
                return(as.character(value))
            } else if (type %in% c("i", "int", "i,log")) {
                return(as.character(as.integer(value)))
            } else if (type %in% c("r", "real", "r,log")) {
                return(as.character(as.numeric(value)))
            } else {
                return(as.character(value))
            }
        })
        
        # Find matching configuration in config_data
        matching_config_id <- NA
        for (k in seq_len(nrow(config_data))) {
            match_found <- TRUE
            for (pname in param_names) {
                if (!identical(config_data[[pname]][k], param_signature[[pname]])) {
                    match_found <- FALSE
                    break
                }
            }
            if (match_found) {
                matching_config_id <- config_data$CONFIG_ID[k]
                break
            }
        }
        
        if (is.na(matching_config_id)) {
            if (opt$verbose) cat("        Warning: Configuration not found in testing data, skipping...\n")
            next
        }
        
        # Get MNRG value
        mnrg_value <- config_data$MNRG[config_data$CONFIG_ID == matching_config_id]
        
        # Add to results
        scenario_results <- rbind(scenario_results, data.frame(
            RUN_ID = run_name,
            CONFIG_ID = matching_config_id,
            MNRG = mnrg_value,
            stringsAsFactors = FALSE
        ))
    }
    
    # Save scenario results
    scenario_output_file <- file.path(output_dir, paste0(scenario_name, ".csv"))
    write.table(scenario_results, file = scenario_output_file, sep = ";",
               row.names = FALSE, quote = FALSE)
    
    if (opt$verbose) {
        cat(sprintf("    Saved %d records to: %s\n", nrow(scenario_results), 
                   basename(scenario_output_file)))
    }
}

# ---------- Step 7: Save instance best values ----------
if (opt$verbose) cat("\n=== Saving instance best values ===\n")

instances_df <- data.frame(
    INSTANCE = names(instance_best),
    BEST = unlist(instance_best),
    stringsAsFactors = FALSE
)

instances_output_file <- file.path(output_dir, "Optimum_Testing.csv")
write.table(instances_df, file = instances_output_file, sep = ";",
           row.names = FALSE, quote = FALSE)

if (opt$verbose) {
    cat(sprintf("Saved %d instances to: %s\n", nrow(instances_df), 
               basename(instances_output_file)))
}

# ---------- Step 8: Save configuration quality data ----------
if (opt$verbose) cat("\n=== Saving configuration quality data ===\n")

configs_output_file <- file.path(output_dir, "configurations_results.csv")
write.table(config_data, file = configs_output_file, sep = ";",
           row.names = FALSE, quote = FALSE)

if (opt$verbose) {
    cat(sprintf("Saved %d configurations to: %s\n", nrow(config_data), 
               basename(configs_output_file)))
}

# ---------- Summary ----------
if (opt$verbose) {
    cat("\n=== Processing completed successfully ===\n")
    cat(sprintf("Output directory: %s\n", output_dir))
    cat(sprintf("Files generated:\n"))
    cat(sprintf("  - Optimum_Testing.csv (%d instances)\n", nrow(instances_df)))
    cat(sprintf("  - configurations_results.csv (%d configurations)\n", nrow(config_data)))
    
    scenario_files <- list.files(output_dir, pattern = "_runs\\.csv$")
    if (length(scenario_files) > 0) {
        cat(sprintf("  - %d scenario run files\n", length(scenario_files)))
        for (sf in scenario_files) {
            cat(sprintf("    * %s\n", sf))
        }
    }
}

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end

