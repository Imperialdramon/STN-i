# nolint start

#########################################################################
# Elite Configuration Processing Utilities
# Author: Pablo Estobar
#
# Description:
# This file contains utility functions for processing elite configurations
# from multiple testing .Rdata files. Includes MNRG calculation, configuration
# tracking, and trajectory reconstruction.
#########################################################################

#' Calculate MNRG (Mean Normalized Ranking Gap) for elite configurations
#'
#' Processes N testing matrices to compute MNRG values for each configuration.
#' For each matrix:
#'   1. Calculate GAP: absolute difference from best known value per instance
#'   2. Calculate Ranking: horizontal ranking per instance (lower GAP = better rank)
#'   3. Normalize: horizontal normalization to [0,1] range per instance  
#'   4. Mean per configuration: vertical mean across instances
#' Then apply aggregation criteria across all matrices.
#'
#' @param matrices_data List of matrices, each with instances (rows) and configurations (cols)
#' @param instance_best List named by instance with best known values
#' @param aggregation_criteria Aggregation method: "mean", "median", "min", "max" (default: "mean")
#' @param problem_type "min" or "max" optimization sense
#' @param verbose Logical to print debug information
#'
#' @return List containing:
#'   - mnrg_values: Named vector of MNRG values per configuration
#'   - gap_matrices: List of gap matrices per file
#'   - rank_matrices: List of rank matrices per file
#'   - norm_matrices: List of normalized matrices per file
#'   - col_means: Matrix of column means per file
#'
#' @export
calculate_elite_mnrg <- function(matrices_data, instance_best, aggregation_criteria = "mean",
                                problem_type = "min", verbose = FALSE) {
  
  if (verbose) cat("\n=== Calculating MNRG for Elite Configurations ===\n")
  
  # Validate aggregation criteria
  if (!(aggregation_criteria %in% c("mean", "median", "min", "max"))) {
    stop("Aggregation criteria must be one of: 'mean', 'median', 'min', 'max'")
  }
  
  processed_matrices <- list()
  all_configs <- c()
  
  # Process each matrix (each .Rdata file)
  for (mat_idx in seq_along(matrices_data)) {
    mat_info <- matrices_data[[mat_idx]]
    current_matrix <- mat_info$matrix
    mat_name <- mat_info$name
    
    if (verbose) cat(sprintf("Processing matrix %d/%d: %s\n", mat_idx, length(matrices_data), mat_name))
    
    # Store all unique configurations
    all_configs <- unique(c(all_configs, colnames(current_matrix)))
    
    # --- Step 1: Calculate GAP for each cell ---
    gap_matrix <- matrix(NA, nrow = nrow(current_matrix), ncol = ncol(current_matrix))
    rownames(gap_matrix) <- rownames(current_matrix)
    colnames(gap_matrix) <- colnames(current_matrix)
    
    for (i in seq_len(nrow(current_matrix))) {
      inst_name <- rownames(current_matrix)[i]
      best_val <- instance_best[[inst_name]]
      
      for (j in seq_len(ncol(current_matrix))) {
        val <- current_matrix[i, j]
        
        if (!is.na(val) && !is.na(best_val)) {
          gap_matrix[i, j] <- abs(val - best_val)
        }
      }
    }
    
    # --- Step 2: Calculate ranking for each row (horizontal ranking) ---
    # Lower GAP = better = lower rank number
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
    
    # --- Step 3: Normalize rankings to [0, 1] for each row (horizontal normalization) ---
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
    
    # --- Step 4: Calculate mean per column (vertical mean per matrix) ---
    col_means <- colMeans(norm_matrix, na.rm = TRUE)
    
    processed_matrices[[mat_name]] <- list(
      gap = gap_matrix,
      rank = rank_matrix,
      normalized = norm_matrix,
      col_means = col_means
    )
  }
  
  # --- Step 5: Apply aggregation criteria across matrices ---
  # Create matrix to store column means from all matrices
  means_matrix <- matrix(NA, nrow = length(processed_matrices), ncol = length(all_configs))
  colnames(means_matrix) <- sort(all_configs)
  
  for (i in seq_along(processed_matrices)) {
    mat_data <- processed_matrices[[i]]
    col_means <- mat_data$col_means
    
    # Place means in correct columns
    for (config_id in names(col_means)) {
      col_idx <- which(colnames(means_matrix) == config_id)
      if (length(col_idx) > 0) {
        means_matrix[i, col_idx] <- col_means[[config_id]]
      }
    }
  }
  
  # Calculate MNRG according to aggregation criteria
  mnrg_values <- switch(aggregation_criteria,
    "mean" = colMeans(means_matrix, na.rm = TRUE),
    "median" = apply(means_matrix, 2, median, na.rm = TRUE),
    "min" = apply(means_matrix, 2, min, na.rm = TRUE),
    "max" = apply(means_matrix, 2, max, na.rm = TRUE)
  )
  
  if (verbose) {
    cat(sprintf("Calculated MNRG for %d configurations\n", length(mnrg_values)))
    cat(sprintf("MNRG range: [%.4f, %.4f]\n", min(mnrg_values, na.rm = TRUE), max(mnrg_values, na.rm = TRUE)))
  }
  
  return(list(
    mnrg_values = mnrg_values,
    all_configs = all_configs,
    processed_matrices = processed_matrices,
    means_matrix = means_matrix
  ))
}


#' Load and process elite configuration .Rdata files
#'
#' Loads N testing .Rdata files and extracts testing matrices with elite configurations.
#' Returns standardized matrices aligned by instances and configuration IDs.
#'
#' @param elites_dir Directory containing All_Elites .Rdata files
#' @param verbose Logical to print debug information
#'
#' @return List containing:
#'   - matrices_list: List of matrices with name, matrix, instances, configs fields
#'   - instance_best: Named list of best values per instance
#'   - problem_type: Optimization sense from first file
#'
#' @export
load_elite_testing_data <- function(elites_dir, verbose = FALSE) {
  
  if (verbose) cat(sprintf("Loading elite testing data from: %s\n", elites_dir))
  
  # Get all testing .Rdata files
  testing_files <- list.files(elites_dir, pattern = "\\.Rdata$", full.names = TRUE)
  
  if (length(testing_files) == 0) {
    stop(sprintf("No .Rdata files found in %s", elites_dir))
  }
  
  if (verbose) cat(sprintf("Found %d testing files\n", length(testing_files)))
  
  matrices_list <- list()
  all_instances <- c()
  all_config_ids <- c()
  instance_best <- list()
  problem_type <- "min"
  
  # Process each testing file
  for (i in seq_along(testing_files)) {
    file_path <- testing_files[i]
    file_name <- basename(file_path)
    
    if (verbose) cat(sprintf("  [%d/%d] Processing: %s\n", i, length(testing_files), file_name))
    
    # Load testing data
    load(file_path)
    
    if (is.null(iraceResults$testing)) {
      if (verbose) cat("    Warning: No testing results found, skipping...\n")
      next
    }
    
    # Get testing experiments matrix and metadata
    experiments <- iraceResults$testing$experiments
    test_instances <- iraceResults$scenario$testInstances
    sense <- iraceResults$scenario$sense
    # Ensure problem_type is a single value, default to "min" if not available
    if (is.na(sense) || is.null(sense) || length(sense) == 0) {
      problem_type <- "min"
    } else {
      problem_type <- as.character(sense)[1]
    }
    
    if (verbose) {
      cat(sprintf("    Matrix dimensions: %d instances x %d configurations\n", 
                 nrow(experiments), ncol(experiments)))
      cat(sprintf("    Problem type: %s\n", problem_type))
    }
    
    # Get instance base names
    instance_names <- basename(test_instances)
    
    # Store all unique instances and configurations
    all_instances <- unique(c(all_instances, instance_names))
    all_config_ids <- unique(c(all_config_ids, colnames(experiments)))
    
    # Calculate best value for each instance
    for (j in seq_len(nrow(experiments))) {
      inst_name <- instance_names[j]
      row_values <- experiments[j, ]
      
      # Remove NA values for calculation
      valid_values <- row_values[!is.na(row_values)]
      
      if (length(valid_values) > 0) {
        best_val <- if (problem_type == "min") min(valid_values) else max(valid_values)
        
        # Update instance best (keep the best across all files)
        if (is.null(instance_best[[inst_name]])) {
          instance_best[[inst_name]] <- best_val
        } else {
          if (problem_type == "min") {
            instance_best[[inst_name]] <- min(instance_best[[inst_name]], best_val)
          } else {
            instance_best[[inst_name]] <- max(instance_best[[inst_name]], best_val)
          }
        }
      }
    }
    
    matrices_list[[file_name]] <- list(
      name = file_name,
      matrix = experiments,
      instances = instance_names,
      configs = colnames(experiments)
    )
  }
  
  if (verbose) {
    cat(sprintf("\nTotal unique instances: %d\n", length(all_instances)))
    cat(sprintf("Total unique configurations: %d\n", length(all_config_ids)))
  }
  
  # Standardize matrices to common instance/config ordering
  sorted_instances <- sort(all_instances)
  sorted_config_ids <- sort(all_config_ids)
  
  standardized_matrices <- list()
  
  for (i in seq_along(matrices_list)) {
    mat_info <- matrices_list[[i]]
    orig_matrix <- mat_info$matrix
    orig_instances <- mat_info$instances
    orig_configs <- mat_info$configs
    
    # Create new matrix with standard dimensions
    std_matrix <- matrix(NA, nrow = length(sorted_instances), ncol = length(sorted_config_ids))
    rownames(std_matrix) <- sorted_instances
    colnames(std_matrix) <- sorted_config_ids
    
    # Fill in values from original matrix
    for (row_idx in seq_len(nrow(orig_matrix))) {
      inst_name <- orig_instances[row_idx]
      std_row_idx <- which(sorted_instances == inst_name)
      
      for (col_idx in seq_len(ncol(orig_matrix))) {
        config_id <- orig_configs[col_idx]
        std_col_idx <- which(sorted_config_ids == config_id)
        
        std_matrix[std_row_idx, std_col_idx] <- orig_matrix[row_idx, col_idx]
      }
    }
    
    standardized_matrices[[paste0("std_", i)]] <- list(
      name = mat_info$name,
      matrix = std_matrix,
      instances = sorted_instances,
      configs = sorted_config_ids
    )
  }
  
  return(list(
    matrices_list = standardized_matrices,
    instance_best = instance_best,
    problem_type = problem_type,
    all_instances = sorted_instances,
    all_config_ids = sorted_config_ids
  ))
}


#' Create elite configurations mapping from parameters
#'
#' Links elite configurations with their parameter values by reading from
#' the .Rdata files and matching configuration IDs.
#'
#' @param iraceResults IRace results object loaded from .Rdata file
#' @param elite_config_ids Vector of elite configuration IDs
#' @param parameters Parameters data frame with NAME and TYPE columns
#' @param verbose Logical to print debug information
#'
#' @return Data frame with CONFIG_ID and parameter columns
#'
#' @export
create_elite_configurations_mapping <- function(iraceResults, elite_config_ids, parameters, verbose = FALSE) {
  
  if (verbose) cat("Creating elite configurations mapping...\n")
  
  all_configs <- iraceResults$allConfigurations
  param_names <- intersect(parameters$NAME, colnames(all_configs))
  param_types <- setNames(parameters$TYPE, parameters$NAME)
  
  config_list <- lapply(elite_config_ids, function(id) {
    param_row <- all_configs[all_configs$.ID. == as.integer(id), , drop = FALSE]
    param_values <- sapply(param_names, function(pname) {
      if (nrow(param_row) == 0 || !(pname %in% colnames(param_row)) || is.na(param_row[[pname]])) {
        return(NA)
      }
      value <- param_row[[pname]]
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
    c(CONFIG_ID = as.character(id), param_values)
  })
  
  config_df <- as.data.frame(do.call(rbind, config_list), stringsAsFactors = FALSE)
  
  if (verbose) cat(sprintf("Created mapping for %d configurations\n", nrow(config_df)))
  
  return(config_df)
}


#' Create optimum file from testing instances
#'
#' Generates a CSV file containing the optimum value for each testing instance.
#' The optimum is extracted as the best value found in the testing matrices.
#'
#' @param instance_best Named list with best values per instance
#' @param output_file Path where to save the optimum CSV file
#' @param verbose Logical to print debug information
#'
#' @return Data frame with INSTANCE and BEST columns
#'
#' @export
create_elite_optimum_file <- function(instance_best, output_file, verbose = FALSE) {
  
  if (verbose) cat(sprintf("Creating optimum file: %s\n", output_file))
  
  optimum_df <- data.frame(
    INSTANCE = names(instance_best),
    BEST = as.numeric(instance_best),
    stringsAsFactors = FALSE
  )
  
  # Sort by instance name for consistency
  optimum_df <- optimum_df[order(optimum_df$INSTANCE), ]
  
  write.csv(optimum_df, file = output_file, row.names = FALSE, quote = TRUE)
  
  if (verbose) cat(sprintf("Optimum file created with %d instances\n", nrow(optimum_df)))
  
  return(optimum_df)
}


#' Reconstruct elite configuration trajectories from scenario data
#'
#' Searches through scenario Results directories to find which trajectories
#' contain each elite configuration. Matches elite configs with trajectory data
#' using parameter comparison.
#'
#' @param elite_configs_mapping Data frame from create_elite_configurations_mapping
#' @param scenarios_dir Parent directory containing scenario subdirectories
#' @param parameters Parameters data frame
#' @param verbose Logical to print debug information
#'
#' @return List of data frames, one per scenario, with trajectory information
#'
#' @export
reconstruct_elite_trajectories <- function(elite_configs_mapping, scenarios_dir, parameters, verbose = FALSE) {
  
  if (verbose) cat(sprintf("Reconstructing elite trajectories from: %s\n", scenarios_dir))
  
  param_names <- intersect(parameters$NAME, colnames(elite_configs_mapping))
  scenario_dirs <- list.dirs(scenarios_dir, recursive = FALSE, full.names = TRUE)
  
  trajectories_per_scenario <- list()
  total_found <- 0
  
  for (scenario_path in scenario_dirs) {
    scenario_name <- basename(scenario_path)
    results_dir <- file.path(scenario_path, "Results")
    
    if (!dir.exists(results_dir)) {
      if (verbose) cat(sprintf("  Warning: Results dir not found for %s\n", scenario_name))
      next
    }
    
    if (verbose) cat(sprintf("\n  Processing scenario: %s\n", scenario_name))
    
    # Get all run subdirectories
    run_dirs <- list.dirs(results_dir, recursive = FALSE, full.names = TRUE)
    
    scenario_trajectories <- list()
    
    for (run_path in run_dirs) {
      run_name <- basename(run_path)
      trajectories_file <- file.path(run_path, "trajectories.csv")
      configs_file <- file.path(run_path, "configurations.csv")
      
      if (!file.exists(trajectories_file) || !file.exists(configs_file)) {
        next
      }
      
      # Load trajectory and configuration data
      trajectories_df <- read.csv(trajectories_file, sep = ";", stringsAsFactors = FALSE)
      configs_df <- read.csv(configs_file, sep = ";", stringsAsFactors = FALSE)
      
      # Find matching elite configurations
      for (elite_idx in seq_len(nrow(elite_configs_mapping))) {
        elite_config <- elite_configs_mapping[elite_idx, ]
        elite_id <- elite_config$CONFIG_ID
        
        # Try to find matching configuration in this run
        matching_config <- NULL
        for (config_idx in seq_len(nrow(configs_df))) {
          config_row <- configs_df[config_idx, ]
          
          # Compare parameter values
          params_match <- TRUE
          for (param in param_names) {
            if (param %in% colnames(elite_config) && param %in% colnames(config_row)) {
              elite_val <- elite_config[[param]]
              config_val <- config_row[[param]]
              
              # If both are NA, consider them matching
              # If one is NA and the other isn't, no match
              # If neither is NA, compare their string representations
              if (is.na(elite_val) && is.na(config_val)) {
                # Both NA - matching parameter
                next
              } else if (is.na(elite_val) || is.na(config_val)) {
                # One is NA - no match
                params_match <- FALSE
                break
              } else if (as.character(elite_val) != as.character(config_val)) {
                # Neither is NA - compare values
                params_match <- FALSE
                break
              }
            }
          }
          
          if (params_match) {
            matching_config <- config_row$CONFIG_ID
            break
          }
        }
        
        # If found, extract trajectories containing this config
        if (!is.null(matching_config)) {
          elite_trajs <- trajectories_df[
            trajectories_df$ORIGIN_CONFIG_ID == matching_config |
            trajectories_df$DESTINY_CONFIG_ID == matching_config,
          ]
          
          if (nrow(elite_trajs) > 0) {
            scenario_trajectories[[sprintf("%s_%s_%s", scenario_name, run_name, elite_id)]] <- list(
              elite_config_id = elite_id,
              scenario = scenario_name,
              run = run_name,
              trajectories = elite_trajs,
              configurations = configs_df
            )
            total_found <- total_found + 1
          }
        }
      }
    }
    
    if (length(scenario_trajectories) > 0) {
      trajectories_per_scenario[[scenario_name]] <- scenario_trajectories
    }
  }
  
  if (verbose) cat(sprintf("\nFound %d elite configuration trajectories\n", total_found))
  
  return(trajectories_per_scenario)
}

# nolint end
