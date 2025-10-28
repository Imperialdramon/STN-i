# nolint start

#' Process Rdata files and create Results structure
#' 
#' This function processes all .Rdata files in the specified input directory,
#' creates a Results directory structure, and generates configurations, trajectories,
#' instances, and results files for each .Rdata file. It handles errors gracefully
#' and logs progress to the console.
#'
#' @param input_dir Directory containing .Rdata files
#' @param parameters_file Path to parameters CSV file
#' @param results_dir Directory for Results files
#' @param optimum_file Optional path to instance optimum values file
#' @param best_criteria Criteria for best value selection ('min' or 'max')
#' @param is_na_ranking Whether to consider NA values in rankings
#' @param verbose Whether to show detailed processing information
#' 
#' @return None
#' 
#' @export
process_rdata_files <- function(input_dir, parameters_file, results_dir, optimum_file = NULL, best_criteria = "min", is_na_ranking = FALSE, verbose = FALSE) {
  # List all Rdata files
  files <- list.files(input_dir, pattern = "\\.Rdata$", full.names = TRUE)
  
  # Create Results directory if it doesn't exist
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Read parameters
  parameters <- read.csv(parameters_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  parameters$NAME <- trimws(parameters$NAME)
  parameters$TYPE <- trimws(parameters$TYPE)
  
  # Initialize results storage
  all_results <- list()
  
    # Process each Rdata file
  for (file in files) {
    if (verbose) cat("Procesando archivo:", basename(file), "\n")
    seed_dir <- file.path(results_dir, tools::file_path_sans_ext(basename(file)))
    dir.create(seed_dir, showWarnings = FALSE, recursive = TRUE)
    
    tryCatch({
      # Load irace results with suppressed output
      irace_results <- suppressMessages(suppressWarnings(read_logfile(file)))      # Create configurations file
      if (verbose) cat("  Creando archivo configurations.csv...\n")
      configs_file <- file.path(seed_dir, "configurations.csv")
      configs <- create_configurations_file(
        irace_results$allConfigurations,
        irace_results$allElites,
        length(irace_results$allElites) - 1,
        parameters,
        configs_file
      )
      
      # Create trajectories file
      if (verbose) cat("  Creando archivo trajectories.csv...\n")
      trajectories_file <- file.path(seed_dir, "trajectories.csv")
      trajectories <- create_trajectories_file(
        irace_results,
        irace_results$raceData,
        irace_results$allElites,
        length(irace_results$allElites) - 1,
        trajectories_file
      )
      
      # Create instances file
      if (verbose) cat("  Creando archivo instances.csv...\n")
      instances_file <- file.path(seed_dir, "instances.csv")
      instances_df <- create_instances_file(
        irace_results,
        irace_results$experiments,
        optimum_file,
        instances_file
      )
      
      # Create results file
      if (verbose) cat("  Creando archivo results.csv...\n")
      results_file <- file.path(seed_dir, "results.csv")
      results <- create_results_file(
        irace_results$experiments,
        instances_df,
        results_file,
        best_criteria,
        is_na_ranking
      )
      
      # Store results
      all_results[[basename(file)]] <- list(
        configurations = configs,
        experiments = results,
        trajectories = trajectories,
        instances = instances_df
      )
    }, error = function(e) {
      cat("Error procesando archivo", basename(file), ":", conditionMessage(e), "\n")
      all_results[[basename(file)]] <- list(
        configurations = data.frame(),
        experiments = data.frame(),
        trajectories = data.frame(),
        instances = data.frame()
      )
    })
  }
  #return(all_results)
}

#' Create configurations file
#' 
#' This function creates a configurations file that lists all configurations evaluated
#' in the irace process, indicating whether each configuration was elite in any iteration.
#' It includes parameter values for each configuration based on the provided parameters data frame.
#' 
#' @param allConfigurations The allConfigurations data frame from iraceResults
#' @param allElites The allElites list from iraceResults
#' @param iterations Number of iterations
#' @param parameters The parameters data frame
#' @param output_file Path to save the configurations CSV file
#' 
#' @return Data frame with configurations
#' 
#' @export
create_configurations_file <- function(allConfigurations, allElites, iterations, parameters, output_file) {
  config_ids <- allConfigurations$.ID.
  is_elite <- sapply(config_ids, function(id) {
    any(sapply(allElites[1:iterations], function(elite_ids) id %in% elite_ids))
  })
  
  param_names <- intersect(parameters$NAME, colnames(allConfigurations))
  param_types <- setNames(parameters$TYPE, parameters$NAME)
  
  config_list <- lapply(seq_along(config_ids), function(i) {
    id <- config_ids[i]
    param_row <- allConfigurations[allConfigurations$.ID. == as.integer(id), , drop = FALSE]
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
    c(
      CONFIG_ID = as.character(id),
      IS_ELITE = as.character(is_elite[i]),
      param_values
    )
  })
  
  config_df <- as.data.frame(do.call(rbind, config_list), stringsAsFactors = FALSE)
  write.table(config_df, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
  return(config_df)
}

#' Get location code for a configuration based on parameter values
#' 
#' This function generates a unique location code for a configuration by concatenating
#' the parameter names and their corresponding values. This code is used to identify
#' configurations with identical parameter values across different runs.
#' 
#' @param iraceResults The irace results from read_logfile
#' @param raceData The raceData list from iraceResults
#' @param allElites The allElites list from iraceResults
#' @param iterations Number of iterations
#' @param output_file Path to save the configurations CSV file
#' 
#' @return Data frame with configurations
#' 
#' @export
create_trajectories_file <- function(iraceResults, raceData, allElites, iterations, output_file) {
  # Initialize data frame for trajectories
  trajectories <- data.frame(
    PATH = logical(),
    ORIGIN_ITER = integer(),
    ORIGIN_CONFIG_ID = character(),
    DESTINY_ITER = integer(),
    DESTINY_CONFIG_ID = character(),
    stringsAsFactors = FALSE
  )

  # Process each iteration
  for (iter in 1:iterations) {
    configs <- raceData[[iter]]
    if (!is.null(configs) && nrow(configs) > 0) {
      # For each configuration in current iteration
      for (i in 1:nrow(configs)) {
        config <- configs[i, ]
        config_id <- as.character(config$.ID.)
        
        if (iter == 1) {
          # First iteration configurations connect to themselves with PATH = FALSE
          trajectories <- rbind(trajectories, data.frame(
            PATH = FALSE,
            ORIGIN_ITER = 1,
            ORIGIN_CONFIG_ID = config_id,
            DESTINY_ITER = 1,
            DESTINY_CONFIG_ID = config_id,
            stringsAsFactors = FALSE
          ))
        } else {
          parent_id <- as.character(config$.PARENT.)
          # Check if it was elite in previous iteration
          is_elite <- config_id %in% allElites[[iter-1]]
          
          # If it has a parent in previous iteration, create connection
          if (!is.na(parent_id) && parent_id %in% rownames(raceData[[iter-1]])) {
            trajectories <- rbind(trajectories, data.frame(
              PATH = TRUE,
              ORIGIN_ITER = iter - 1,
              ORIGIN_CONFIG_ID = parent_id,
              DESTINY_ITER = iter,
              DESTINY_CONFIG_ID = config_id,
              stringsAsFactors = FALSE
            ))
          } else if (is_elite) {
            # If elite and no connection with previous iteration, connect to itself
            trajectories <- rbind(trajectories, data.frame(
              PATH = TRUE,
              ORIGIN_ITER = iter - 1,
              ORIGIN_CONFIG_ID = config_id,
              DESTINY_ITER = iter,
              DESTINY_CONFIG_ID = config_id,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Save file with semicolon separator
  write.table(trajectories, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
  return(trajectories)
}

#' Create trajectories file
#' 
#' This function creates a trajectories file that captures the evolution of configurations
#' across iterations in the irace process. It records the paths taken by configurations,
#' including whether they were elite configurations, and connects them based on their
#' parent-child relationships.
#' 
#' @param iraceResults The irace results from read_logfile
#' @param experiments The experiments matrix from irace
#' @param optimum_file Optional path to instance optimum values file
#' @param output_file Path to save the trajectories CSV file
#' 
#' @return Data frame with trajectories
#'
#' @export
create_instances_file <- function(iraceResults, experiments, optimum_file, output_file) {
  instances_full <- iraceResults$scenario$instances
  experiment_ids <- as.integer(rownames(experiments))

  if (is.null(instances_full) || length(instances_full) == 0 || is.null(experiment_ids) || length(experiment_ids) == 0) {
    cat("  Advertencia: No hay datos de instancias\n")
    results_df <- data.frame(
      EXPERIMENT_ID = integer(),
      INSTANCE_ID = integer(),
      SEED = integer(),
      NAME = character(),
      OPTIMUM = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    tryCatch({
      # Create data frame with experiments that are actually in experiments
      instances_df <- data.frame(
        EXPERIMENT_ID = experiment_ids,
        INSTANCE_ID = sapply(experiment_ids, function(id) iraceResults$state$instances_log[id]$instanceID),
        SEED = sapply(experiment_ids, function(id) iraceResults$state$instances_log[id]$seed),
        NAME = sapply(experiment_ids, function(id) basename(instances_full[iraceResults$state$instances_log[id]$instanceID])),
        OPTIMUM = NA,
        stringsAsFactors = FALSE
      )

      # Add optimal values if optimum_file is provided
      if (!is.null(optimum_file) && file.exists(optimum_file)) {
        optimum_data <- read.csv(optimum_file, sep = ";", stringsAsFactors = FALSE)
        for (i in seq_len(nrow(instances_df))) {
          instance_name <- instances_df$NAME[i]
          opt_row <- optimum_data[optimum_data$INSTANCE == instance_name, ]
          instances_df$OPTIMUM[i] <- if (nrow(opt_row) > 0 && !is.na(opt_row$BEST)) opt_row$BEST else NA
        }
      }

      # Save file with semicolon separator
      write.table(instances_df, file = output_file, sep = ";", 
                  row.names = FALSE, quote = FALSE)
                  
      # Return the data frame
      return(instances_df)
    }, error = function(e) {
      cat("  Error procesando instancias:", conditionMessage(e), "\n")
      instances_df <- data.frame(
        EXPERIMENT_ID = integer(),
        INSTANCE_ID = integer(),
        SEED = integer(),
        NAME = character(),
        OPTIMUM = numeric(),
        stringsAsFactors = FALSE
      )
      write.table(instances_df, file = output_file, sep = ";", 
                  row.names = FALSE, quote = FALSE)
      return(instances_df)
    })
  }

  # Save file
  write.table(instances_df, file = output_file, sep = ";", 
              row.names = FALSE, quote = FALSE)

  # Return the data frame
  return(instances_df)
}

#' Create results file
#' 
#' This function processes the experiments matrix and computes various statistics
#' for each configuration, including best and mean quality, rankings, normalized rankings,
#' GAPs, and their respective rankings. It handles NA values according to the specified criteria.
#' 
#' @param experiments Matrix of experiment results (rows: instances, columns: configurations)
#' @param instances_df Data frame of instances with optimal values
#' @param output_file Path to save the results CSV file
#' @param best_criteria Criteria for best value selection ('min' or 'max')
#' @param is_na_ranking Whether to consider NA values in rankings
#' 
#' @return Data frame with computed results
#' 
#' @export
create_results_file <- function(experiments, instances_df, output_file, best_criteria = "min", is_na_ranking = FALSE) {
  if (is.null(experiments) || ncol(experiments) == 0) {
    cat("  Advertencia: No hay datos de experimentos\n")
    results_df <- data.frame(
      CONFIG_ID = character(),
      INSTANCES = integer(),
      B = numeric(), BR = integer(), BNR = numeric(),
      M = numeric(), MR = integer(), MNR = numeric(),
      BG = numeric(), BRG = integer(), BNRG = numeric(),
      MG = numeric(), MRG = integer(), MNRG = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    tryCatch({
      # Get configuration IDs and initialize results dataframe
      config_ids <- colnames(experiments)
      results_df <- data.frame(CONFIG_ID = config_ids, stringsAsFactors = FALSE)

      # Best quality per configuration (based on best_criteria)
      results_df$B <- if (best_criteria == "min") {
        apply(experiments, 2, function(x) min(x, na.rm = TRUE))
      } else {
        apply(experiments, 2, function(x) max(x, na.rm = TRUE))
      }
      # Mean quality per configuration
      results_df$M <- apply(experiments, 2, function(x) mean(x, na.rm = TRUE))
      results_df$INSTANCES <- apply(experiments, 2, function(x) sum(!is.na(x)))
      
      # Calculate quality rankings by instance and their aggregations
      instance_rankings <- matrix(NA, nrow = nrow(experiments), ncol = ncol(experiments))
      colnames(instance_rankings) <- colnames(experiments)
      rownames(instance_rankings) <- rownames(experiments)
      for (i in seq_len(nrow(experiments))) {
        row <- experiments[i,]
        valid_values <- !is.na(row)
        if (sum(valid_values) > 0) {
          if (is_na_ranking) {
            # Count non-NA values for worst possible rank
            valid_count <- sum(valid_values)
            # Get ranks for non-NA values
            ranks <- rep(NA, length(row))
            ranks[valid_values] <- if (best_criteria == "min") {
              as.integer(rank(row[valid_values], ties.method = "min"))
            } else {
              as.integer(rank(row[valid_values], ties.method = "max"))
            }
            # Assign worst rank + 1 to NA values
            ranks[!valid_values] <- valid_count + 1
            instance_rankings[i,] <- ranks
          } else {
            # Normal ranking ignoring NA
            ranks <- rep(NA, length(row))
            ranks[valid_values] <- if (best_criteria == "min") {
              as.integer(rank(row[valid_values], ties.method = "min"))
            } else {
              as.integer(rank(row[valid_values], ties.method = "max"))
            }
            instance_rankings[i,] <- ranks
          }
        }
      }
      
      # BR: Best ranking per configuration (based on best_criteria)
      results_df$BR <- if (best_criteria == "min") {
        apply(instance_rankings, 2, min, na.rm = TRUE)
      } else {
        apply(instance_rankings, 2, max, na.rm = TRUE)
      }
      # MR: Mean ranking per configuration
      results_df$MR <- apply(instance_rankings, 2, mean, na.rm = TRUE)
      
      # Calculate normalized rankings by instance
      instance_rankings_norm <- matrix(NA, nrow = nrow(instance_rankings), ncol = ncol(instance_rankings))
      colnames(instance_rankings_norm) <- colnames(instance_rankings)
      rownames(instance_rankings_norm) <- rownames(instance_rankings)
      for (i in seq_len(nrow(instance_rankings))) {
        row <- instance_rankings[i,]
        valid_ranks <- !is.na(row)
        if (sum(valid_ranks) > 1) {
          norm_values <- rep(NA, length(row))
          norm_values[valid_ranks] <- (row[valid_ranks] - 1) / (sum(valid_ranks) - 1)
          instance_rankings_norm[i,] <- norm_values
        } else {
          instance_rankings_norm[i,] <- rep(NA, length(row))
        }
      }

      # BNR: Best normalized ranking per configuration (based on best_criteria)
      results_df$BNR <- if (best_criteria == "min") {
        apply(instance_rankings_norm, 2, min, na.rm = TRUE)
      } else {
        apply(instance_rankings_norm, 2, max, na.rm = TRUE)
      }
      # MNR: Mean normalized ranking per configuration
      results_df$MNR <- apply(instance_rankings_norm, 2, mean, na.rm = TRUE)
      
      # Calculate GAPs matrix
      gaps_matrix <- matrix(NA, nrow = nrow(experiments), ncol = ncol(experiments))
      colnames(gaps_matrix) <- colnames(experiments)
      rownames(gaps_matrix) <- rownames(experiments)
      for (i in seq_len(nrow(experiments))) {
        experiment_id <- as.integer(rownames(experiments)[i])
        opt_value <- if(!is.null(instances_df)) {
          instances_df$OPTIMUM[instances_df$EXPERIMENT_ID == experiment_id]
        } else {
          NA
        }
        best_quality <- if (!is.null(opt_value) && !is.na(opt_value)) {
          opt_value
        } else {
          if (best_criteria == "min") {
            min(experiments[i,], na.rm = TRUE)
          } else {
            max(experiments[i,], na.rm = TRUE)
          }
        }
        
        # Calculate GAPs for each configuration in this experiment
        if (!is.na(best_quality) && best_quality != 0) {
          row_values <- experiments[i,]
          gaps_matrix[i,] <- ifelse(!is.na(row_values),
                                  100 * abs(row_values - best_quality) / best_quality,
                                  NA)
        }
      }
      
      # BG: Best GAP per configuration
      results_df$BG <- apply(gaps_matrix, 2, min, na.rm = TRUE)
      # MG: Mean GAP per configuration
      results_df$MG <- apply(gaps_matrix, 2, mean, na.rm = TRUE)
      
      # Calculate GAP rankings by instance
      gap_rankings <- matrix(NA, nrow = nrow(gaps_matrix), ncol = ncol(gaps_matrix))
      colnames(gap_rankings) <- colnames(gaps_matrix)
      rownames(gap_rankings) <- rownames(gaps_matrix)
      
      # Calculate GAP rankings row by row (by instance)
      for (i in seq_len(nrow(gaps_matrix))) {
        row <- gaps_matrix[i,]
        valid_values <- !is.na(row)
        
        if (sum(valid_values) > 0) {  # Solo procesar si hay valores válidos
          if (is_na_ranking) {
            # Count non-NA values for worst possible rank
            valid_count <- sum(valid_values)
            # Get ranks for non-NA values
            ranks <- rep(NA, length(row))
            # Siempre usamos min para gaps porque menor gap es mejor
            ranks[valid_values] <- as.integer(rank(row[valid_values], ties.method = "min"))
            # Assign worst rank + 1 to NA values
            ranks[!valid_values] <- valid_count + 1
            gap_rankings[i,] <- ranks
          } else {
            # Normal ranking ignoring NA
            ranks <- rep(NA, length(row))
            ranks[valid_values] <- as.integer(rank(row[valid_values], ties.method = "min"))
            gap_rankings[i,] <- ranks
          }
        }
      }
      
      # BRG: Best GAP ranking per configuration
      results_df$BRG <- apply(gap_rankings, 2, min, na.rm = TRUE)
      # MRG: Mean GAP ranking per configuration
      results_df$MRG <- apply(gap_rankings, 2, mean, na.rm = TRUE)
      
      # Calculate normalized GAP rankings by instance
      gap_rankings_norm <- matrix(NA, nrow = nrow(gap_rankings), ncol = ncol(gap_rankings))
      colnames(gap_rankings_norm) <- colnames(gap_rankings)
      rownames(gap_rankings_norm) <- rownames(gap_rankings)
      for (i in seq_len(nrow(gap_rankings))) {
        row <- gap_rankings[i,]
        valid_ranks <- !is.na(row)
        if (sum(valid_ranks) > 1) {
          norm_values <- rep(NA, length(row))
          norm_values[valid_ranks] <- (row[valid_ranks] - 1) / (sum(valid_ranks) - 1)
          gap_rankings_norm[i,] <- norm_values
        } else {
          gap_rankings_norm[i,] <- rep(NA, length(row))
        }
      }
      
      # BNRG: Best normalized GAP ranking per configuration
      results_df$BNRG <- apply(gap_rankings_norm, 2, min, na.rm = TRUE)
      # MNRG: Mean normalized GAP ranking per configuration
      results_df$MNRG <- apply(gap_rankings_norm, 2, mean, na.rm = TRUE)
      
    }, error = function(e) {
      cat("  Error procesando resultados:", conditionMessage(e), "\n")
      results_df <- data.frame(
        CONFIG_ID = character(),
        INSTANCES = integer(),
        B = numeric(), BR = integer(), BNR = numeric(),
        M = numeric(), MR = integer(), MNR = numeric(),
        BG = numeric(), BRG = integer(), BNRG = numeric(),
        MG = numeric(), MRG = integer(), MNRG = numeric(),
        stringsAsFactors = FALSE
      )
    })
  }
  
  # Save file
  write.table(results_df, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
  return(results_df)
}

#' Generate STN-I structure from processed results
#' 
#' This function processes multiple results folders generated by different runs (seeds)
#' and aggregates configurations and trajectories into a single STN-I structure.
#' It saves the aggregated configurations and trajectories into specified output files.
#' The function allows selection of quality and representative criteria for configurations.
#' It also filters trajectory types based on a significance threshold.
#' The output files are saved in the specified output directory with a base name.
#' The function handles configurations with identical parameter values across different runs
#' by merging them and updating their associated information.
#' 
#' @param input_dir Directory containing processed results folders
#' @param parameters_file Path to parameters CSV file
#' @param output_dir Directory to save STN-I files
#' @param output_name Base name for output files
#' @param quality_criteria Criteria for quality selection for configurations ('mean' or 'best')
#' @param representative_criteria Criteria for representative configuration selection for locations ('min' or 'max')
#' @param significance Significance threshold for trajectory types
#' @param verbose Whether to show detailed processing information
#'
#' @return None
#' 
#' @export
generate_stn_i <- function(input_dir, parameters_file, output_dir, output_name, quality_criteria = "mean", representative_criteria = "min", significance = 2, verbose = FALSE) {
  # Load the parameters file
  parameters <- read_parameters_file(parameters_file = parameters_file)

  # List all results directories (one per seed/run)
  results_dirs <- list.dirs(path = input_dir, full.names = TRUE, recursive = FALSE)

  # Initialize counters and storage structures
  run_counter <- 1
  new_config_id <- 1
  configurations_list <- list()
  trajectories_list <- list()
  config_map <- list()  # Maps exact_config_id to index in configurations_list

  # Process each results folder (diferents runs by seeds)
  for (results_folder in results_dirs) {
    # Read the irace results files
    configurations_data <- read.csv(file.path(results_folder, "configurations.csv"), sep = ";")
    results_data <- read.csv(file.path(results_folder, "results.csv"), sep = ";")
    trajectories_data <- read.csv(file.path(results_folder, "trajectories.csv"), sep = ";")

    # Process the trajectories data on vectorized way 
    trajectories_batch <- data.frame(
      RUN_ID = run_counter,
      PATH = trajectories_data$PATH,
      ITERATION_1 = trajectories_data$ORIGIN_ITER,
      CONFIG_ID_1 = trajectories_data$ORIGIN_CONFIG_ID,
      NEW_CONFIG_ID_1 = NA,
      SOLUTION_1 = NA,
      QUALITY_1 = NA,
      CONFIG_COUNT_1 = NA,
      ELITE_COUNT_1 = NA,
      REGULAR_COUNT_1 = NA,
      START_COUNT_1 = NA,
      STANDARD_COUNT_1 = NA,
      END_COUNT_1 = NA,
      ELITE_START_COUNT_1 = NA,
      ELITE_STANDARD_COUNT_1 = NA,
      ELITE_END_COUNT_1 = NA,
      REGULAR_START_COUNT_1 = NA,
      REGULAR_STANDARD_COUNT_1 = NA,
      REGULAR_END_COUNT_1 = NA,
      ITERATION_2 = trajectories_data$DESTINY_ITER,
      CONFIG_ID_2 = trajectories_data$DESTINY_CONFIG_ID,
      NEW_CONFIG_ID_2 = NA,
      SOLUTION_2 = NA,
      QUALITY_2 = NA,
      CONFIG_COUNT_2 = NA,
      ELITE_COUNT_2 = NA,
      REGULAR_COUNT_2 = NA,
      START_COUNT_2 = NA,
      STANDARD_COUNT_2 = NA,
      END_COUNT_2 = NA,
      ELITE_START_COUNT_2 = NA,
      ELITE_STANDARD_COUNT_2 = NA,
      ELITE_END_COUNT_2 = NA,
      REGULAR_START_COUNT_2 = NA,
      REGULAR_STANDARD_COUNT_2 = NA,
      REGULAR_END_COUNT_2 = NA,
      stringsAsFactors = FALSE
    )
    trajectories_list[[length(trajectories_list) + 1]] <- trajectories_batch

    # Get parameter columns (same for all configurations)
    param_cols <- setdiff(names(configurations_data), c("CONFIG_ID", "IS_ELITE"))
    
    # Get all unique CONFIG_IDs that appear in trajectories (both ORIGIN and DESTINY)
    all_config_ids_in_trajectories <- unique(c(trajectories_data$ORIGIN_CONFIG_ID, trajectories_data$DESTINY_CONFIG_ID))
    
    # Process ALL configurations that appear in trajectories
    for (config_id in all_config_ids_in_trajectories) {
      # Find configuration in configurations_data
      config_idx <- which(configurations_data$CONFIG_ID == config_id)
      
      if (length(config_idx) == 0) {
        # Configuration not in configurations.csv, skip it
        # This should not happen if data is consistent
        cat("  Advertencia: CONFIG_ID", config_id, "no encontrado en configurations.csv para RUN_ID", run_counter, "\n")
        next
      }
      
      configuration <- configurations_data[config_idx[1], ]
      results <- results_data[results_data$CONFIG_ID == configuration$CONFIG_ID, ]

      # Get parameter values
      param_values <- configuration[param_cols]
      
      # Create exact configuration ID based on parameter values
      param_vec <- unlist(param_values)
      exact_config_id <- paste(names(param_vec), param_vec, sep = "=", collapse = "|")
      
      # Check if this exact configuration already exists
      existing_config_idx <- config_map[[exact_config_id]]

      # Find or create configuration entry
      if (is.null(existing_config_idx)) {
        # Add new configuration (first time we see this exact parameter combination)
        config_entry <- list(
          NEW_CONFIG_ID = new_config_id,
          IS_ELITE = as.character(configuration$IS_ELITE),
          RUN_IDS = list(run_counter),
          MNRG_VALUES = list(results$MNRG),
          ELITE_COUNT = 0,
          REGULAR_COUNT = 0,
          START_COUNT = 0,
          STANDARD_COUNT = 0,
          END_COUNT = 0,
          ELITE_START_COUNT = 0,
          ELITE_STANDARD_COUNT = 0,
          ELITE_END_COUNT = 0,
          REGULAR_START_COUNT = 0,
          REGULAR_STANDARD_COUNT = 0,
          REGULAR_END_COUNT = 0
        )
        # Add parameter values
        for (col_name in param_cols) {
          config_entry[[col_name]] <- configuration[[col_name]]
        }
        configurations_list[[length(configurations_list) + 1]] <- config_entry
        existing_config_idx <- length(configurations_list)
        config_map[[exact_config_id]] <- existing_config_idx
        new_config_id <- new_config_id + 1
      } else {
        # Update existing configuration (same parameter combination seen again)
        config_item <- configurations_list[[existing_config_idx]]
        config_item$RUN_IDS[[1]] <- c(config_item$RUN_IDS[[1]], run_counter)
        config_item$MNRG_VALUES[[1]] <- c(config_item$MNRG_VALUES[[1]], results$MNRG)

        # Update IS_ELITE if necessary
        if (configuration$IS_ELITE == "TRUE" && config_item$IS_ELITE == "FALSE") {
          config_item$IS_ELITE <- "TRUE"
        }
        configurations_list[[existing_config_idx]] <- config_item
      }

      # Get the NEW_CONFIG_ID for this configuration
      current_new_config_id <- configurations_list[[existing_config_idx]]$NEW_CONFIG_ID

      # Update trajectories with NEW_CONFIG_ID and calculate counters
      current_batch_idx <- length(trajectories_list)
      if (current_batch_idx > 0) {
        batch_data <- trajectories_list[[current_batch_idx]]
        config_id_matches_1 <- batch_data$CONFIG_ID_1 == configuration$CONFIG_ID
        config_id_matches_2 <- batch_data$CONFIG_ID_2 == configuration$CONFIG_ID
        
        batch_data$NEW_CONFIG_ID_1[config_id_matches_1] <- current_new_config_id
        batch_data$NEW_CONFIG_ID_2[config_id_matches_2] <- current_new_config_id
        
        trajectories_list[[current_batch_idx]] <- batch_data
        
        # Determine trajectory types and update counters for this configuration
        total_iterations <- max(batch_data$ITERATION_2)
        trajectory_types <- c()
        
        # For CONFIG_ID_1
        idx_1 <- which(batch_data$CONFIG_ID_1 == configuration$CONFIG_ID & batch_data$RUN_ID == run_counter)
        if (length(idx_1) > 0) {
          for (idx in idx_1) {
            if (batch_data$ITERATION_1[idx] == 1) {
              type <- "START"
            } else if (batch_data$ITERATION_1[idx] == total_iterations) {
              type <- "END"
            } else {
              type <- "STANDARD"
            }
            trajectory_types <- c(trajectory_types, type)
          }
        }
        
        # For CONFIG_ID_2
        idx_2 <- which(batch_data$CONFIG_ID_2 == configuration$CONFIG_ID & batch_data$RUN_ID == run_counter)
        if (length(idx_2) > 0) {
          for (idx in idx_2) {
            if (batch_data$ITERATION_2[idx] == 1) {
              type <- "START"
            } else if (batch_data$ITERATION_2[idx] == total_iterations) {
              type <- "END"
            } else {
              type <- "STANDARD"
            }
            trajectory_types <- c(trajectory_types, type)
          }
        }

        # Update counters based on trajectory types
        if (length(trajectory_types) > 0) {
          config_item <- configurations_list[[existing_config_idx]]
          is_elite <- configuration$IS_ELITE == "TRUE"
          unique_types <- unique(trajectory_types)
          num_types <- length(unique_types)
          points_per_type <- if (num_types > 0) 3 / num_types else 0
          
          # Update ELITE or REGULAR count
          if (is_elite) {
            config_item$ELITE_COUNT <- config_item$ELITE_COUNT + 1
          } else {
            config_item$REGULAR_COUNT <- config_item$REGULAR_COUNT + 1
          }
          
          # Update topology counts
          for (type in unique_types) {
            if (type == "START") {
              config_item$START_COUNT <- config_item$START_COUNT + points_per_type
              if (is_elite) {
                config_item$ELITE_START_COUNT <- config_item$ELITE_START_COUNT + points_per_type
              } else {
                config_item$REGULAR_START_COUNT <- config_item$REGULAR_START_COUNT + points_per_type
              }
            } else if (type == "END") {
              config_item$END_COUNT <- config_item$END_COUNT + points_per_type
              if (is_elite) {
                config_item$ELITE_END_COUNT <- config_item$ELITE_END_COUNT + points_per_type
              } else {
                config_item$REGULAR_END_COUNT <- config_item$REGULAR_END_COUNT + points_per_type
              }
            } else if (type == "STANDARD") {
              config_item$STANDARD_COUNT <- config_item$STANDARD_COUNT + points_per_type
              if (is_elite) {
                config_item$ELITE_STANDARD_COUNT <- config_item$ELITE_STANDARD_COUNT + points_per_type
              } else {
                config_item$REGULAR_STANDARD_COUNT <- config_item$REGULAR_STANDARD_COUNT + points_per_type
              }
            }
          }
          
          configurations_list[[existing_config_idx]] <- config_item
        }
      }
    }

    run_counter <- run_counter + 1
  }

  if (verbose) {
    cat("Procesamiento completado. Total configuraciones únicas:", length(configurations_list), "\n")
    cat("Convirtiendo listas a data.frames...\n")
  }

  # Transform configurations_list in configurations_df
  if (length(configurations_list) > 0) {
    configurations_df <- do.call(rbind, lapply(configurations_list, function(x) {
      x$RUN_IDS <- I(x$RUN_IDS)
      x$MNRG_VALUES <- I(x$MNRG_VALUES)
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  } else {
    configurations_df <- data.frame()
  }

  # Transform trajectories_list in trajectories_df
  if (length(trajectories_list) > 0) {
    trajectories_df <- do.call(rbind, trajectories_list)
  } else {
    trajectories_df <- data.frame()
  }

  if (verbose) {
    cat("Conversión completada. Configuraciones:", nrow(configurations_df), "Trayectorias:", nrow(trajectories_df), "\n")
    cat("Aplicando quality_criteria a cada configuración...\n")
  }

  # STEP 1: Apply quality_criteria to each configuration
  # This aggregates MNRG values across runs for each unique configuration
  configurations_with_quality <- list()
  
  for (i in seq_len(nrow(configurations_df))) {
    # Calculate final quality based on quality_criteria for this configuration on all runs
    config <- configurations_df[i, ]
    mnrg_values <- as.numeric(config$MNRG_VALUES[[1]])
    mnrg_values <- mnrg_values[!is.na(mnrg_values)]
    mnrg_values <- round(mnrg_values, significance)
    if (length(mnrg_values) == 0) {
      final_mnrg <- NA
    } else if (quality_criteria == "min") {
      final_mnrg <- min(mnrg_values)
    } else if (quality_criteria == "max") {
      final_mnrg <- max(mnrg_values)
    } else if (quality_criteria == "mean") {
      final_mnrg <- mean(mnrg_values)
    } else if (quality_criteria == "median") {
      final_mnrg <- median(mnrg_values)
    } else if (quality_criteria == "mode") {
      mode_value <- as.numeric(names(sort(table(mnrg_values), decreasing = TRUE)[1]))
      final_mnrg <- mode_value
    } else {
      stop("Error: quality_criteria debe ser 'min', 'max', 'mean', 'median' o 'mode'", call. = FALSE)
    }
    
    # NOW generate location code based on parameter file (LX)
    param_cols <- setdiff(names(config), c("NEW_CONFIG_ID", "IS_ELITE", "RUN_IDS", "MNRG_VALUES",
                                           "ELITE_COUNT", "REGULAR_COUNT", "START_COUNT", "STANDARD_COUNT", "END_COUNT",
                                           "ELITE_START_COUNT", "ELITE_STANDARD_COUNT", "ELITE_END_COUNT",
                                           "REGULAR_START_COUNT", "REGULAR_STANDARD_COUNT", "REGULAR_END_COUNT"))
    param_values <- config[param_cols]
    location_code <- get_location_code(param_values, parameters)
    
    # Count how many runs this configuration appeared in
    config_count <- length(config$RUN_IDS[[1]])
    
    # Store configuration with its quality, location code, and all counters
    config_with_quality <- list(
      NEW_CONFIG_ID = config$NEW_CONFIG_ID,
      LOCATION_CODE = location_code,
      IS_ELITE = config$IS_ELITE,
      FINAL_MNRG = final_mnrg,
      CONFIG_COUNT = config_count,
      ELITE_COUNT = config$ELITE_COUNT,
      REGULAR_COUNT = config$REGULAR_COUNT,
      START_COUNT = config$START_COUNT,
      STANDARD_COUNT = config$STANDARD_COUNT,
      END_COUNT = config$END_COUNT,
      ELITE_START_COUNT = config$ELITE_START_COUNT,
      ELITE_STANDARD_COUNT = config$ELITE_STANDARD_COUNT,
      ELITE_END_COUNT = config$ELITE_END_COUNT,
      REGULAR_START_COUNT = config$REGULAR_START_COUNT,
      REGULAR_STANDARD_COUNT = config$REGULAR_STANDARD_COUNT,
      REGULAR_END_COUNT = config$REGULAR_END_COUNT
    )
    
    configurations_with_quality[[i]] <- config_with_quality
  }

  if (verbose) {
    cat("Total configuraciones procesadas:", length(configurations_with_quality), "\n")
    cat("Agrupando configuraciones por locaciones...\n")
  }

  # STEP 2: Group configurations by location
  # Aggregate configurations that have the same LOCATION_CODE
  locations_df <- data.frame()
  location_map <- list()  # Maps LOCATION_CODE to index in locations_df
  
  for (config_with_quality in configurations_with_quality) {
    loc_code <- config_with_quality$LOCATION_CODE
    final_mnrg <- config_with_quality$FINAL_MNRG
    config_count <- config_with_quality$CONFIG_COUNT
    
    # Get counters directly from configuration (already calculated)
    elite_count <- config_with_quality$ELITE_COUNT
    regular_count <- config_with_quality$REGULAR_COUNT
    start_count <- config_with_quality$START_COUNT
    standard_count <- config_with_quality$STANDARD_COUNT
    end_count <- config_with_quality$END_COUNT
    elite_start_count <- config_with_quality$ELITE_START_COUNT
    elite_standard_count <- config_with_quality$ELITE_STANDARD_COUNT
    elite_end_count <- config_with_quality$ELITE_END_COUNT
    regular_start_count <- config_with_quality$REGULAR_START_COUNT
    regular_standard_count <- config_with_quality$REGULAR_STANDARD_COUNT
    regular_end_count <- config_with_quality$REGULAR_END_COUNT
    
    if (loc_code %in% names(location_map)) {
      # Location already exists, sum the counters
      loc_idx <- location_map[[loc_code]]
      
      locations_df$CONFIG_COUNT[loc_idx] <- locations_df$CONFIG_COUNT[loc_idx] + config_count
      locations_df$ELITE_COUNT[loc_idx] <- locations_df$ELITE_COUNT[loc_idx] + elite_count
      locations_df$REGULAR_COUNT[loc_idx] <- locations_df$REGULAR_COUNT[loc_idx] + regular_count
      locations_df$START_COUNT[loc_idx] <- locations_df$START_COUNT[loc_idx] + start_count
      locations_df$STANDARD_COUNT[loc_idx] <- locations_df$STANDARD_COUNT[loc_idx] + standard_count
      locations_df$END_COUNT[loc_idx] <- locations_df$END_COUNT[loc_idx] + end_count
      locations_df$ELITE_START_COUNT[loc_idx] <- locations_df$ELITE_START_COUNT[loc_idx] + elite_start_count
      locations_df$ELITE_STANDARD_COUNT[loc_idx] <- locations_df$ELITE_STANDARD_COUNT[loc_idx] + elite_standard_count
      locations_df$ELITE_END_COUNT[loc_idx] <- locations_df$ELITE_END_COUNT[loc_idx] + elite_end_count
      locations_df$REGULAR_START_COUNT[loc_idx] <- locations_df$REGULAR_START_COUNT[loc_idx] + regular_start_count
      locations_df$REGULAR_STANDARD_COUNT[loc_idx] <- locations_df$REGULAR_STANDARD_COUNT[loc_idx] + regular_standard_count
      locations_df$REGULAR_END_COUNT[loc_idx] <- locations_df$REGULAR_END_COUNT[loc_idx] + regular_end_count
      
      # Add this configuration's quality to the location's list
      locations_df$QUALITIES[[loc_idx]] <- c(locations_df$QUALITIES[[loc_idx]], final_mnrg)
    } else {
      # Create new location with counters from this configuration
      location_entry <- data.frame(
        LOCATION_CODE = loc_code,
        CONFIG_COUNT = config_count,
        ELITE_COUNT = elite_count,
        REGULAR_COUNT = regular_count,
        START_COUNT = start_count,
        STANDARD_COUNT = standard_count,
        END_COUNT = end_count,
        ELITE_START_COUNT = elite_start_count,
        ELITE_STANDARD_COUNT = elite_standard_count,
        ELITE_END_COUNT = elite_end_count,
        REGULAR_START_COUNT = regular_start_count,
        REGULAR_STANDARD_COUNT = regular_standard_count,
        REGULAR_END_COUNT = regular_end_count,
        QUALITIES = I(list(final_mnrg)),
        FINAL_QUALITY = NA,
        stringsAsFactors = FALSE
      )
      
      locations_df <- rbind(locations_df, location_entry)
      location_map[[loc_code]] <- nrow(locations_df)
    }
  }

  if (verbose) {
    cat("Aplicando representative_criteria a cada locación...\n")
  }

  # STEP 3: Apply representative_criteria to each location
  # This aggregates quality values across configurations for each location
  for (i in seq_len(nrow(locations_df))) {
    qualities <- as.numeric(locations_df$QUALITIES[[i]])
    qualities <- qualities[!is.na(qualities)]
    qualities <- round(qualities, significance)
    if (length(qualities) == 0) {
      locations_df$FINAL_QUALITY[i] <- NA
    } else if (representative_criteria == "min") {
      locations_df$FINAL_QUALITY[i] <- min(qualities)
    } else if (representative_criteria == "max") {
      locations_df$FINAL_QUALITY[i] <- max(qualities)
    } else if (representative_criteria == "mean") {
      locations_df$FINAL_QUALITY[i] <- mean(qualities)
    } else if (representative_criteria == "median") {
      locations_df$FINAL_QUALITY[i] <- median(qualities)
    } else if (representative_criteria == "mode") {
      mode_value <- as.numeric(names(sort(table(qualities), decreasing = TRUE)[1]))
      locations_df$FINAL_QUALITY[i] <- mode_value
    }
    
    # Normalize topology counts by dividing by 3
    locations_df$START_COUNT[i] <- locations_df$START_COUNT[i] / 3
    locations_df$STANDARD_COUNT[i] <- locations_df$STANDARD_COUNT[i] / 3
    locations_df$END_COUNT[i] <- locations_df$END_COUNT[i] / 3

    # Normalize combined QUALITY-TOPOLOGY counts by dividing by 3
    locations_df$ELITE_START_COUNT[i] <- locations_df$ELITE_START_COUNT[i] / 3
    locations_df$ELITE_STANDARD_COUNT[i] <- locations_df$ELITE_STANDARD_COUNT[i] / 3
    locations_df$ELITE_END_COUNT[i] <- locations_df$ELITE_END_COUNT[i] / 3
    locations_df$REGULAR_START_COUNT[i] <- locations_df$REGULAR_START_COUNT[i] / 3
    locations_df$REGULAR_STANDARD_COUNT[i] <- locations_df$REGULAR_STANDARD_COUNT[i] / 3
    locations_df$REGULAR_END_COUNT[i] <- locations_df$REGULAR_END_COUNT[i] / 3
  }

  if (verbose) {
    total_configs <- sum(locations_df$CONFIG_COUNT)
    total_unique_configs <- length(configurations_with_quality)
    cat("Total locaciones creadas:", nrow(locations_df), "\n")
    cat("Suma de CONFIG_COUNT:", total_configs, "\n")
    cat("Total configuraciones únicas:", total_unique_configs, "\n")
    if (total_configs != total_unique_configs) {
      cat("⚠️  ADVERTENCIA: La suma de CONFIG_COUNT no coincide con el total de configuraciones únicas!\n")
    }
    cat("Actualizando trayectorias con información de locaciones...\n")
  }

  # STEP 4: Update trajectories with location information
  # Map NEW_CONFIG_ID to LOCATION_CODE from configurations_with_quality
  config_to_location <- list()
  for (config_with_quality in configurations_with_quality) {
    config_id <- as.character(config_with_quality$NEW_CONFIG_ID)
    loc_code <- config_with_quality$LOCATION_CODE
    config_to_location[[config_id]] <- loc_code
  }

  # Update SOLUTION_1 and SOLUTION_2 using NEW_CONFIG_ID
  trajectories_df$SOLUTION_1 <- sapply(as.character(trajectories_df$NEW_CONFIG_ID_1), function(id) config_to_location[[id]])
  trajectories_df$SOLUTION_2 <- sapply(as.character(trajectories_df$NEW_CONFIG_ID_2), function(id) config_to_location[[id]])
  
  # Update quality and counter information for each location
  for (i in seq_len(nrow(locations_df))) {
    loc_code <- locations_df$LOCATION_CODE[i]
    loc_quality <- locations_df$FINAL_QUALITY[i]
    loc_config_count <- locations_df$CONFIG_COUNT[i]
    loc_elite_count <- locations_df$ELITE_COUNT[i]
    loc_regular_count <- locations_df$REGULAR_COUNT[i]
    loc_start_count <- locations_df$START_COUNT[i]
    loc_standard_count <- locations_df$STANDARD_COUNT[i]
    loc_end_count <- locations_df$END_COUNT[i]
    loc_elite_start_count <- locations_df$ELITE_START_COUNT[i]
    loc_elite_standard_count <- locations_df$ELITE_STANDARD_COUNT[i]
    loc_elite_end_count <- locations_df$ELITE_END_COUNT[i]
    loc_regular_start_count <- locations_df$REGULAR_START_COUNT[i]
    loc_regular_standard_count <- locations_df$REGULAR_STANDARD_COUNT[i]
    loc_regular_end_count <- locations_df$REGULAR_END_COUNT[i]
    
    # Update for SOLUTION_1
    trajectories_df$QUALITY_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_quality
    trajectories_df$CONFIG_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_config_count
    trajectories_df$ELITE_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_elite_count
    trajectories_df$REGULAR_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_regular_count
    trajectories_df$START_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_start_count
    trajectories_df$STANDARD_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_standard_count
    trajectories_df$END_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_end_count
    trajectories_df$ELITE_START_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_elite_start_count
    trajectories_df$ELITE_STANDARD_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_elite_standard_count
    trajectories_df$ELITE_END_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_elite_end_count
    trajectories_df$REGULAR_START_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_regular_start_count
    trajectories_df$REGULAR_STANDARD_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_regular_standard_count
    trajectories_df$REGULAR_END_COUNT_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_regular_end_count
    
    # Update for SOLUTION_2
    trajectories_df$QUALITY_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_quality
    trajectories_df$CONFIG_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_config_count
    trajectories_df$ELITE_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_elite_count
    trajectories_df$REGULAR_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_regular_count
    trajectories_df$START_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_start_count
    trajectories_df$STANDARD_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_standard_count
    trajectories_df$END_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_end_count
    trajectories_df$ELITE_START_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_elite_start_count
    trajectories_df$ELITE_STANDARD_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_elite_standard_count
    trajectories_df$ELITE_END_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_elite_end_count
    trajectories_df$REGULAR_START_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_regular_start_count
    trajectories_df$REGULAR_STANDARD_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_regular_standard_count
    trajectories_df$REGULAR_END_COUNT_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_regular_end_count
  }

  # Remove temporary columns
  trajectories_df$CONFIG_ID_1 <- NULL
  trajectories_df$CONFIG_ID_2 <- NULL
  trajectories_df$NEW_CONFIG_ID_1 <- NULL
  trajectories_df$NEW_CONFIG_ID_2 <- NULL

  # TEMPORAL: Calcular suma total de CONFIG_COUNT para validación
  total_config_count <- sum(locations_df$CONFIG_COUNT)
  
  # Validación: verificar que todas las locaciones están en SOLUTION_1 o SOLUTION_2
  unique_locations_in_file <- unique(c(trajectories_df$SOLUTION_1, trajectories_df$SOLUTION_2))
  locations_in_df <- locations_df$LOCATION_CODE
  missing_locations <- setdiff(locations_in_df, unique_locations_in_file)

  # Unificar por SOLUTION_2 para evitar duplicados
  unique_solution2_data <- trajectories_df[!duplicated(trajectories_df$SOLUTION_2), ]
  
  cat("========================================\n")
  cat("VALIDACIÓN - Archivo:", output_name, "\n")
  cat("----------------------------------------\n")
  cat("DATOS PRE-LOCACIONES (Configuraciones):\n")
  cat("  Total de configuraciones únicas:", length(configurations_with_quality), "\n")
  cat("  Suma CONFIG_COUNT (de configs):", sum(sapply(configurations_with_quality, function(x) x$CONFIG_COUNT)), "\n")
  cat("  Mejor calidad (FINAL_MNRG):", sprintf(paste0("%.", significance, "f"), min(sapply(configurations_with_quality, function(x) x$FINAL_MNRG), na.rm = TRUE)), "\n")
  cat("----------------------------------------\n")
  cat("DATOS POST-LOCACIONES (Agrupadas):\n")
  cat("  Total de locaciones creadas:", nrow(locations_df), "\n")
  cat("  Suma CONFIG_COUNT (de locations):", total_config_count, "\n")
  cat("  Mejor calidad (FINAL_QUALITY):", sprintf(paste0("%.", significance, "f"), min(locations_df$FINAL_QUALITY, na.rm = TRUE)), "\n")
  cat("----------------------------------------\n")
  cat("DATOS EN ARCHIVO (Usando solo SOLUTION_2):\n")
  cat("  Total de locaciones únicas (SOLUTION_2):", length(unique(trajectories_df$SOLUTION_2)), "\n")
  unique_solution2_data <- trajectories_df[!duplicated(trajectories_df$SOLUTION_2), ]
  cat("  Suma CONFIG_COUNT (CONFIG_COUNT_2):", sum(unique_solution2_data$CONFIG_COUNT_2, na.rm = TRUE), "\n")
  cat("  Mejor calidad (QUALITY_2):", sprintf(paste0("%.", significance, "f"), min(unique_solution2_data$QUALITY_2, na.rm = TRUE)), "\n")
  cat("  Total de trayectorias guardadas:", nrow(trajectories_df), "\n")
  
  # Verificar si la mejor locación está en el archivo
  best_location_idx <- which.min(locations_df$FINAL_QUALITY)
  best_location_code <- locations_df$LOCATION_CODE[best_location_idx]
  best_location_quality <- locations_df$FINAL_QUALITY[best_location_idx]
  is_in_solution1 <- best_location_code %in% trajectories_df$SOLUTION_1
  is_in_solution2 <- best_location_code %in% trajectories_df$SOLUTION_2
  if (is_in_solution1 || is_in_solution2) {
    cat("  ✓ Mejor locación", best_location_code, "con calidad", sprintf(paste0("%.", significance, "f"), best_location_quality), "SÍ está en trayectorias\n")
  } else {
    cat("  ⚠️  ADVERTENCIA: Mejor locación", best_location_code, "con calidad", sprintf(paste0("%.", significance, "f"), best_location_quality), "NO está en trayectorias!\n")
  }
  
  if (length(missing_locations) > 0) {
    cat("  ⚠️  ADVERTENCIA: Hay", length(missing_locations), "locaciones NO guardadas en trayectorias!\n")
    cat("  Locaciones faltantes:", paste(missing_locations, collapse = ", "), "\n")
  } else {
    cat("  ✓ Todas las locaciones están en las trayectorias (SOLUTION_1 o SOLUTION_2)\n")
  }
  cat("========================================\n")

  # Write output files
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file_path <- file.path(output_dir, output_name)
  write.table(trajectories_df, file = output_file_path, sep = ";", row.names = FALSE, quote = FALSE)
}

#' Reads and processes a parameters definition file for irace tuning.
#'
#' This function reads a CSV file that defines the parameters to be used in an irace tuning scenario,
#' processing both categorical/ordinal values and numerical ranges (integer or real).
#' Additionally, it constructs a structured dictionary (`param_domains`) that includes
#' the parameter domains and location settings, with built-in validations for correct formats.
#'
#' @param parameters_file `character(1)`\cr
#'   Path to the parameters CSV file (semicolon-separated).
#'
#' @return A list with two elements, params and domains:
#' \item{params}{A data frame with the original parameters and processed VALUES_ARRAY and
#'   LOCATIONS_ARRAY columns.}
#' \item{domains}{A list where each element corresponds to a parameter name and contains
#'   the structured values and locations for that parameter.}
#'
#' @export
read_parameters_file <- function(parameters_file) {
  # Read the parameters file
  params <- read.csv2(parameters_file, header = TRUE, stringsAsFactors = FALSE)
  # Auxiliary function to clean up the values
  clean_array <- function(x) {
    x <- gsub("[()]", "", x) # Delete parentheses
    trimws(x) # Trim leading and trailing whitespace
  }
  # Auxiliary function to parse the location dictionary
  parse_location_dict <- function(loc_vector) {
    loc_split <- strsplit(loc_vector, ":")
    # Validate that each element has exactly two parts (name and value)
    if (any(sapply(loc_split, length) != 2)) {
      stop("Error: Formato incorrecto en LOCATIONS_ARRAY (debe ser 'name:value')", call. = FALSE)
    }
    # Convert to named list
    loc_list <- sapply(loc_split, function(x) as.numeric(x[2]))
    names(loc_list) <- sapply(loc_split, function(x) x[1])
    return(loc_list)
  }
  # Initialize the domains list
  domains <- list()
  # Iterate through each parameter and process the values and locations
  params$VALUES_ARRAY <- mapply(function(type, values, locations, name) {
    values <- clean_array(values)
    locations <- clean_array(locations)
    # If the parameter is categorical or ordinal
    # then interpret the values and locations as vectors
    if (type %in% c("c", "o")) {
      values_vec <- strsplit(values, ",")[[1]]
      loc_vector <- strsplit(locations, ",")[[1]]
      loc_dict <- parse_location_dict(loc_vector)
      # Validate that the number of values matches the number of locations
      if (length(values_vec) != length(loc_dict)) {
        stop(paste("Error: Desajuste entre VALUES y LOCATIONS en parámetro:", name), call. = FALSE)
      }
      # Save in the domains structure
      domains[[name]] <<- list(
        values = values_vec,
        locations = loc_dict
      )
      return(values_vec)
    # If the parameter is integer or real
    } else if (type %in% c("i", "r")) {
      values_nums <- as.numeric(strsplit(values, ",")[[1]])
      locations_nums <- as.numeric(strsplit(locations, ",")[[1]])
      # Validate that the number of values and locations is correct
      if (length(values_nums) != 2) {
        stop(paste("Error: VALUES debe tener (min,max) para parámetro:", name), call. = FALSE)
      }
      # Validate that the number of locations is correct
      if (length(locations_nums) != 2) {
        stop(paste("Error: LOCATIONS debe tener (significance, step) para parámetro:", name), call. = FALSE)
      }
      # Save in the domains structure
      domains[[name]] <<- list(
        values = list(min = values_nums[1], max = values_nums[2]),
        locations = list(step = locations_nums[1], significance = locations_nums[2])
      )
      return(values_nums)
    } else {
      stop(paste("Error: Tipo de parámetro desconocido:", type), call. = FALSE)
    }
  }, params$TYPE, params$VALUES_ARRAY, params$LOCATIONS_ARRAY, params$NAME, SIMPLIFY = FALSE)
  # Convert the parameters data frame to a list
  params$LOCATIONS_ARRAY <- lapply(seq_len(nrow(params)), function(i) {
    type <- params$TYPE[i]
    if (type %in% c("c", "o")) {
      loc_vector <- strsplit(clean_array(params$LOCATIONS_ARRAY[i]), ",")[[1]]
      return(loc_vector)
    } else if (type %in% c("i", "r")) {
      return(as.numeric(strsplit(clean_array(params$LOCATIONS_ARRAY[i]), ",")[[1]]))
    }
  })
  return(list(
    params = params,
    domains = domains
  ))
}

#' Generate a unique location code for a given set of parameter values.
#' 
#' This function generates a unique location code based on the provided parameter values
#' and the definitions in the parameters structure. It handles categorical, ordinal,
#' integer, and real parameters, ensuring that the location code is formatted correctly
#' with leading zeros or 'X' characters for missing values.
#' 
#' @param parameters_values A list of parameter values.
#' @param parameters A list of parameter definitions.
#' 
#' @return A character string representing the unique location code.
#' 
#' @export
get_location_code <- function(parameters_values, parameters) {
  location_code <- ""
  # For each parameter, get the location code
  for (param_name in names(parameters_values)) {
    # Skip if the parameter is not in the parameters list
    if (!(param_name %in% names(parameters$domains))) {
      next
    }
    param_type <- parameters$params$TYPE[parameters$params$NAME == param_name]
    param_value <- parameters_values[[param_name]]
    # If the parameter is categorical or ordinal
    if (param_type %in% c("c", "o")) {
      loc_dict <- parameters$domains[[param_name]]$locations
      # If the parameter value is NA or "<NA>", use Xs for the code
      if (is.na(param_value) || param_value == "<NA>") {
        max_digits <- nchar(as.character(max(loc_dict)))
        code_part <- paste0(rep("X", max_digits), collapse = "")
      }
      # Otherwise, find the corresponding location code for the value
      else {
        # Convert param_value to character to ensure string matching
        param_value_str <- as.character(param_value)
        
        if (param_value_str %in% names(loc_dict)) {
          code_num <- loc_dict[[param_value_str]]
          max_value <- max(loc_dict)
          max_digits <- nchar(as.character(max_value))
          current_digits <- nchar(as.character(code_num))
          difference <- max_digits - current_digits
          code_part <- paste0(strrep("0", difference), code_num)
        } else {
          max_digits <- nchar(as.character(max(loc_dict)))
          code_part <- paste0(rep("X", max_digits), collapse = "")
        }
      }
    }
    # If the parameter is integer or real
    else if (param_type %in% c("i", "r")) {
      param_domain <- parameters$domains[[param_name]]
      lower_bound <- param_domain$values$min
      upper_bound <- param_domain$values$max
      significance <- param_domain$locations$significance
      step <- param_domain$locations$step
      # Validate that the parameter domains are correctly defined
      if (is.na(lower_bound) || is.na(upper_bound) || is.na(significance) || is.na(step)) {
        stop(paste0("Error: Parameter domain not properly defined for: ", param_name))
      }
      # If the parameter value is NA or "<NA>", use Xs for the code
      if (is.na(param_value) || param_value == "<NA>") {
        max_upper_scaled <- as.integer(upper_bound * (10^significance))
        # Validate the max upper scaled value
        if (is.na(max_upper_scaled) || max_upper_scaled <= 0) {
          stop(paste0("Error: max_upper_scaled invalid for parameter ", param_name))
        }
        total_digits <- nchar(as.character(max_upper_scaled))
        if (is.na(total_digits) || total_digits <= 0) {
          stop(paste0("Error: total_digits invalid for parameter ", param_name))
        }
        code_part <- paste0(rep("X", total_digits), collapse = "")
      } else {
        # Calculate the subrange index and the scaled value
        subrange_index <- floor((as.numeric(param_value) - lower_bound) / step)
        calculated_value <- lower_bound + subrange_index * step
        scaled_value <- as.integer(calculated_value * (10^significance))
        max_upper_scaled <- as.integer(upper_bound * (10^significance))
        # Validate the scaled value and max upper scaled value
        if (is.na(scaled_value) || is.na(max_upper_scaled)) {
          stop(paste0("Error: scaled_value or max_upper_scaled is NA for parameter ", param_name,
                      " (scaled_value = ", scaled_value, 
                      ", max_upper_scaled = ", max_upper_scaled, ")"))
        }
        current_digits <- nchar(as.character(scaled_value))
        max_upper_digits <- nchar(as.character(max_upper_scaled))
        # Validate the digits
        if (is.na(current_digits) || is.na(max_upper_digits)) {
          stop(paste0("Error: digits NA for parameter ", param_name,
                      " (scaled_value = ", scaled_value, 
                      ", max_upper_scaled = ", max_upper_scaled, ")"))
        }
        difference <- max_upper_digits - current_digits
        if (is.na(difference)) {
          stop(paste0("Error: difference is NA for parameter ", param_name))
        }
        if (difference < 0) {
          stop(paste0("Error: difference negative (", difference, ") for parameter ", param_name, 
                      " (scaled_value = ", scaled_value, 
                      ", max_upper_scaled = ", max_upper_scaled, ")"))
        }
        code_part <- paste0(strrep("0", difference), scaled_value)
      }
    }
    # If the parameter type is not supported
    else {
      stop(paste0("Error: Unsupported parameter type: ", param_type))
    }
    # Append the code part to the total location code
    location_code <- paste0(location_code, code_part)
  }
  return(location_code)
}

#' Parse command line arguments
#' 
#' This function parses command line arguments in the format `--key=value` and returns a named list of parameters.
#' 
#' @param args A character vector of command line arguments.
#' 
#' @return A named list where each key corresponds to an argument name and its value is the argument value.
#' 
#' @examples
#' \dontrun{
#' args <- c("--input=path/to/input.txt", "--output=path/to/output.txt")
#' parsed_args <- parse_arguments(args)
#' print(parsed_args)
#' }
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
  E(STN_i)$Type <- NA
  if (problem_type == "min") {
    E(STN_i)[fitness_to < fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to > fitness_from]$Type <- "WORSENING"
  } else {
    E(STN_i)[fitness_to > fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to < fitness_from]$Type <- "WORSENING"
  }
  E(STN_i)[fitness_to == fitness_from]$Type <- "EQUAL"

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

#' Load STN-i data from a file
#' 
#' This function loads STN-i data from a specified input file and validates its structure.
#' 
#' @param input_file A string specifying the path to the input file containing the STN-i data.
#' 
#' @return A list containing the loaded STN-i data, including the graph, problem type, best known solution, number of runs, and summaries.
#' 
#' @examples
#' \dontrun{
#' stn_i_data <- get_stn_i_data("path/to/stn_i_file.Rdata")
#' }
get_stn_i_data <- function(input_file) {
  # Check if file exists
  if (!file.exists(input_file)) {
    stop(paste("Input file does not exist:", input_file), call. = FALSE)
  }

  # Load the object and retrieve its name
  loaded_name <- load(input_file)
  stn_i_result <- get(loaded_name)

  # Validate structure
  expected_fields <- c(
    "STN_i",
    "network_name",
    "number_of_runs",
    "problem_type",
    "best_known_solution"
  )

  if (!is.list(stn_i_result) || !all(expected_fields %in% names(stn_i_result))) {
    stop("The loaded file does not contain a valid STN-i result structure.", call. = FALSE)
  }

  return(stn_i_result)
}

#' Get palette colors for STN-i visualization
#'
#' This function returns a list of colors for nodes and edges based on the specified palette option.
#'
#' @param palette A numeric value specifying the color palette option. Default is 1.
#'
#' @return A list containing node and edge colors for the specified palette.
#'
#' @examples
#' \dontrun{
#' colors <- get_stn_i_palette_colors(2)
#' print(colors)
#' }
get_stn_i_palette_colors <- function(palette = 1) {
  palette_colors <- switch(as.character(palette),
    "1" = list(
      node = list(regular = "gray70", elite = "orange", best = "red", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "2" = list(
      node = list(regular = "lightblue", elite = "blue", best = "darkblue", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "3" = list(
      node = list(regular = "lightgreen", elite = "green", best = "darkgreen", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "4" = list(
      node = list(regular = "lightpink", elite = "pink", best = "darkred", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "5" = list(
      node = list(regular = "lightgray", elite = "gray", best = "black", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4, 5.")
  )

  return(palette_colors)
}

#' Get layout data for a given graph object
#'
#' This function retrieves layout data for a given graph object based on the specified layout type.
#'
#' @param g A graph object of class igraph representing the STN-i.
#' @param layout A string specifying the layout type. Options include:
#'   "fr" (Fruchterman-Reingold),
#'   "kk" (Kamada-Kawai),
#'   "circle" (circular layout),
#'   "grid" (nodes on a grid),
#'   "sphere" (nodes on a sphere),
#'   "drl" (DrL force-directed layout),
#'   "graphopt" (force-directed using physics model),
#'   "random" (random placement).
#'   Default is "fr".
#'
#' These layouts were selected for their stability and interpretability across different graph sizes.
#'
#' @return A list containing the layout title, coordinates, and layout type.
#'
#' @examples
#' \dontrun{
#' layout_data <- get_layout_data(STN_i, layout = "kk")
#' }
get_layout_data <- function(g, layout = "fr") {
  layout_data <- switch(layout,
    "fr" = list(
      title = "Fruchterman-Reingold Layout",
      coords = layout_with_fr(g),
      layout_type = "fr"
    ),
    "kk" = list(
      title = "Kamada-Kawai Layout",
      coords = layout_with_kk(g),
      layout_type = "kk"
    ),
    "circle" = list(
      title = "Circle Layout",
      coords = layout_in_circle(g),
      layout_type = "circle"
    ),
    "grid" = list(
      title = "Grid Layout",
      coords = layout_on_grid(g),
      layout_type = "grid"
    ),
    "sphere" = list(
      title = "Sphere Layout",
      coords = layout_on_sphere(g),
      layout_type = "sphere"
    ),
    "drl" = list(
      title = "DrL Layout",
      coords = layout_with_drl(g),
      layout_type = "drl"
    ),
    "graphopt" = list(
      title = "Graphopt Layout",
      coords = layout_with_graphopt(g),
      layout_type = "graphopt"
    ),
    "random" = list(
      title = "Random Layout",
      coords = layout_randomly(g),
      layout_type = "random"
    ),
    stop("Invalid layout option. Choose from: fr, kk, circle, grid, sphere, drl, graphopt, random.")
  )

  return(layout_data)
}

#' Decorate nodes and edges an STN for visualising a single algorithm STN
#' 
#' This function decorates the nodes and edges of an STN graph object for visualization purposes.
#' 
#' @param STN_i Graph object
#' @param problem_type Boolean indicating minimisation or not
#' @param show_regular Boolean indicating whether to show regular nodes
#' @param show_start_regular Boolean indicating whether to show start regular nodes
#' @param palette_colors List of colors for the different node and edge types
#' 
#' @return Decorated STN-i graph object
#' 
#' @examples
#' \dontrun{
#' STN_i <- stn_i_decorate(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1))
#' }
stn_i_decorate <- function(STN_i, problem_type = "min", show_regular = TRUE, show_start_regular = TRUE, palette_colors) {

  # Filter vertices according to settings
  to_remove <- c()
  if (!show_regular) {
    to_remove <- c(to_remove, V(STN_i)[Quality == "REGULAR"]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(STN_i)[Topology == "START" & Quality == "REGULAR"]$name)
  }

  # Remove duplicates before deletion
  to_remove <- unique(to_remove)
  STN_i <- delete_vertices(STN_i, to_remove)

  # Retrieve edge list and corresponding fitness values
  edge_list <- as_edgelist(STN_i)
  node_fitness <- V(STN_i)$Fitness
  node_names <- V(STN_i)$name

  # Extract fitness of origin and destination nodes for each edge
  fitness_from <- node_fitness[match(edge_list[, 1], node_names)]
  fitness_to   <- node_fitness[match(edge_list[, 2], node_names)]

  # Assign edge types based on fitness comparison
  if (problem_type == "min") {
    E(STN_i)[fitness_to < fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to > fitness_from]$Type <- "WORSENING"
  } else {
    E(STN_i)[fitness_to > fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to < fitness_from]$Type <- "WORSENING"
  }
  E(STN_i)[fitness_to == fitness_from]$Type <- "EQUAL"

  # Assign edge colors based on type
  E(STN_i)$color[E(STN_i)$Type == "IMPROVING"] <- palette_colors$edge$improving
  E(STN_i)$color[E(STN_i)$Type == "WORSENING"] <- palette_colors$edge$worsening
  E(STN_i)$color[E(STN_i)$Type == "EQUAL"]     <- palette_colors$edge$equal

  # Set edge width proportional to number of visits (weight)
  E(STN_i)$width <- E(STN_i)$weight

  # Assign node shapes based on Topology type
  V(STN_i)[V(STN_i)$Topology == "STANDARD"]$shape <- "circle"
  V(STN_i)[V(STN_i)$Topology == "START"]$shape <- "square"
  V(STN_i)[V(STN_i)$Topology == "END"]$shape <- "triangle"

  # Assign node colors based on Quality
  V(STN_i)[V(STN_i)$Quality == "REGULAR"]$color <- palette_colors$node$regular
  V(STN_i)[V(STN_i)$Quality == "ELITE"]$color <- palette_colors$node$elite
  V(STN_i)[V(STN_i)$Quality == "BEST"]$color <- palette_colors$node$best

  # Set node size proportional to in-degree (number of incoming visits)
  V(STN_i)$size <- strength(STN_i, mode = "in") + 1

  # Slightly increase the size of BEST nodes for visibility
  V(STN_i)[V(STN_i)$Quality == "BEST"]$size <- V(STN_i)[V(STN_i)$Quality == "BEST"]$size + 0.5

  return(STN_i)
}

#' Function to create a plot for STN-i data
#'
#' This function processes the input STN-i file, decorates the STN object, and prepares layout data for plotting.
#'
#' @param input_file Path to the input STN-i file.
#' @param show_regular Boolean indicating whether to show regular nodes in the plot (default is TRUE).
#' @param show_start_regular Boolean indicating whether to show start regular nodes in the plot (default is TRUE).
#' @param palette_colors List of colors for the different node and edge types.
#' @param zoom_quantile Numeric value between 0 and 1 to define a zoom level for the plot (default is NA, meaning no zoom).
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#' result <- stn_i_plot_create("path/to/stn_i_file.Rdata", show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_stn_i_palette_colors(1), zoom_quantile = 0.5)
#' }
stn_i_plot_create <- function(input_file, show_regular = TRUE, show_start_regular = TRUE, palette_colors, zoom_quantile = NA) {
  # Load the STN-i data
  stn_i_result <- get_stn_i_data(input_file)

  # Obtain the STN object and problem type
  STN_i <- stn_i_result$STN_i
  problem_type <- stn_i_result$problem_type

  # Decorate the STN-i object
  STN_i <- stn_i_decorate(STN_i, problem_type, show_regular, show_start_regular, palette_colors)

  # If zooming is enabled, extract subgraph
  if (!is.na(zoom_quantile) && zoom_quantile > 0 && zoom_quantile < 1) {
    STN_i <- get_zoomed_graph(STN_i, zoom_quantile, problem_type)
  }

  # Return everything needed for external plotting
  return(STN_i)
}

#' Save a plot of the STN-i graph
#' 
#' This function saves a plot of the STN-i graph to a specified PDF file, applying custom shapes and colors for nodes and edges.
#' 
#' @param output_file_path A string specifying the path to the output PDF file.
#' @param STN_i A graph object of class igraph representing the STN-i.
#' @param layout_data A list containing layout information, including coordinates and title for the plot.
#' @param palette_colors A list of colors for nodes and edges, as returned by `get_stn_i_palette_colors()`.
#' @param nsizef A numeric factor to adjust node sizes (default is 1).
#' @param ewidthf A numeric factor to adjust edge widths (default is 0.5).
#' @param asize A numeric value for the arrow size of edges (default is 0.3).
#' @param ecurv A numeric value for the curvature of edges (default is 0.3).
#' 
#' @return None. The function saves the plot to the specified PDF file.
#' 
#' @examples
#' \dontrun{
#' save_stn_i_plot("output/stn_i_plot.pdf", STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3)
#' }
save_stn_i_plot <- function(output_file_path, STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3) {
  # Ensure the file name ends in .pdf
  if (!grepl("\\.pdf$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".pdf")
  }

  # Define triangle shape
  mytriangle <- function(coords, v = NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1 / 200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color, col = vertex.color,
            stars = cbind(vertex.size, vertex.size, vertex.size),
            add = TRUE, inches = FALSE)
  }
  # Register triangle as a custom shape (reuses circle clipping)
  add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

  # Define legend components based on the palette
  legend.txt <- c("Start", "Standard", "End", "Regular", "Elite", "Best", "Improve", "Equal", "Worse")
  legend.col <- c(
    palette_colors$node$shapes, palette_colors$node$shapes, palette_colors$node$shapes,
    palette_colors$node$regular, palette_colors$node$elite, palette_colors$node$best,
    palette_colors$edge$improving, palette_colors$edge$equal, palette_colors$edge$worsening
  )
  legend.shape <- c(15, 21, 17, 21, 21, 21, NA, NA, NA)
  legend.lty <- c(NA, NA, NA, NA, NA, NA, 1, 1, 1)

  # Open PDF device
  pdf(output_file_path)

  # Compute adjusted node sizes
  maxns <- max(V(STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(STN_i)$size + 1
  } else {
    nsize <- nsizef * V(STN_i)$size
  }

  ewidth <- ewidthf * E(STN_i)$width
  title <- paste(layout_data$title, "Nodes:", vcount(STN_i), "Edges:", ecount(STN_i), "Comp:", components(STN_i)$no)

  # Plot graph
  plot(STN_i, layout = layout_data$coords, vertex.label = "", vertex.size = nsize, main = title, edge.width = ewidth, edge.arrow.size = asize, edge.curved = ecurv)

  # Add legend
  legend("topleft", legend.txt, pch = legend.shape, col = legend.col, pt.bg = legend.col, lty = legend.lty, cex = 0.7, pt.cex = 1.35, bty = "n")

  # Close PDF device
  dev.off()

  message("STN-i plot saved successfully to: ", output_file_path)
}

#' Load and merge multiple STN-i data files from a specified folder.
#'
#' This function reads multiple `.Rdata` files containing STN-i graph objects,
#' merges them into a single graph, and extracts relevant metadata such as
#' algorithm names, problem type, best known solutions, and number of runs.
#'
#' @param input_folder The folder containing the `.Rdata` files.
#'
#' @return A list containing:
#' - `graphs`: A list of merged STN-i graph objects.
#' - `names`: Names of the algorithms extracted from the file names.
#' - `problem_type`: The type of problem (min or max) from the first file.
#' - `best_known_solution`: Best known solutions from each file.
#' - `number_of_runs`: Number of runs for each algorithm.
#'
#' @examples
#' \dontrun{
#' input_folder <- "path/to/stn_i_data"
#' merged_data <- load_stn_i_data(input_folder)
#' }
get_stns_i_data <- function(input_folder) {
  # Check if the input folder contains at least 2 .Rdata files
  files <- list.files(input_folder, pattern = "\\.Rdata$", full.names = TRUE)
  if (length(files) < 2) {
    stop("At least 2 .Rdata files required to merge STN-i data.")
  }

  # Initialize lists to store graphs and metadata
  graphs <- list()
  names <- character(length(files))
  problem_types <- c()
  number_of_runs_vec <- c()

  for (i in seq_along(files)) {
    input_file <- files[i]

    # Obtain the STN-i data from the file
    stn_i_result <- get_stn_i_data(input_file)

    STN_i <- stn_i_result$STN_i
    alg_name <- stn_i_result$network_name
    V(STN_i)$Network <- alg_name
    E(STN_i)$Network <- alg_name

    graphs[[i]] <- STN_i
    names[i] <- stn_i_result$network_name
    problem_types <- c(problem_types, stn_i_result$problem_type)
    number_of_runs_vec <- c(number_of_runs_vec, stn_i_result$number_of_runs)
  }

  if (length(unique(problem_types)) != 1) {
    stop("All input STN-i files must have the same problem type.")
  }

  return(list(
    graphs = graphs,
    names = names,
    problem_type = unique(problem_types),
    number_of_runs = number_of_runs_vec
  ))
}

#' Merge multiple STN-i graphs into a single network (manual method)
#'
#' This function builds a unified STN graph from multiple STN-i graphs,
#' resolving conflicts in node attributes (Fitness, Topology) using specified criteria,
#' and classifies shared and elite nodes. Unlike the union approach, it manually
#' constructs the graph from extracted nodes and edges.
#'
#' @param stns_i_data A list returned by `get_stns_i_data`, containing:
#'   - `graphs`: list of STN-i igraph objects
#'   - `names`: character vector of network names
#'   - `problem_type`: "min" or "max"
#'   - `number_of_runs`: vector with number of runs per network
#' @param criteria One of: "min", "max", "mean", "median", "mode" to resolve Fitness conflicts
#' @param verbose Boolean indicating whether to print debug information (default is FALSE)
#'
#' @return A list with:
#'   - `stnm`: the merged igraph object
#'   - `num_networks`: number of merged networks
#'   - `network_names`: names of the original networks
#'   - `best_known_solution`: best known solutions
#'
#' @examples
#' \dontrun{
#' merged_data <- merge_stns_i_data(stns_i_data, criteria = "mean")
#' }
merge_stns_i_data <- function(stns_i_data, criteria = "mean", verbose = FALSE) {

  # Check if the criteria is valid
  if (!criteria %in% c("min", "max", "mean", "median", "mode")) {
    stop("Invalid criteria. Choose from: 'min', 'max', 'mean', 'median', 'mode'.")
  }

  snts_i <- stns_i_data$graphs
  num_networks <- length(snts_i)
  number_of_runs <- sum(stns_i_data$number_of_runs)
  network_names <- stns_i_data$names
  problem_type <- stns_i_data$problem_type

  # Collect all nodes with debug information
  node_df_list <- list()
  for (i in seq_along(snts_i)) {
    g <- snts_i[[i]]
    
    # Debug information
    if (verbose) {
      cat(sprintf("\nProcessing network %d/%d:\n", i, length(snts_i)))
      cat(sprintf("Number of nodes: %d\n", vcount(g)))
    }
    
    # Extract and validate vertex attributes
    node_names <- V(g)$name
    node_fitness <- V(g)$Fitness
    node_topology <- V(g)$Topology
    node_count <- V(g)$Count
    node_quality <- V(g)$Quality
    node_network <- V(g)$Network
    
    # Debug attribute information
    if (verbose) {
      cat("Sample of Fitness values:", head(node_fitness), "\n")
      cat("Number of NA in Fitness:", sum(is.na(node_fitness)), "\n")
    }
    
    # Create data frame with explicit type conversion
    node_df <- data.frame(
      Node = as.character(node_names),
      Fitness = as.numeric(node_fitness),
      Topology = as.character(node_topology),
      Count = as.numeric(node_count),
      Quality = as.character(node_quality),
      Network = as.character(node_network),
      stringsAsFactors = FALSE
    )
    node_df$graph_id <- i

    if (verbose) {
      cat("Structure of node_df:\n")
      str(node_df)
    }

    node_df_list[[i]] <- node_df
  }
  nodes_all <- bind_rows(node_df_list)
  
  if (verbose) {
    cat("\nCompleted processing all networks.\n")

    # Debug combined data
    cat("\nAfter combining all networks:\n")
    cat("Total number of nodes:", nrow(nodes_all), "\n")
    cat("Number of NA in combined Fitness:", sum(is.na(nodes_all$Fitness)), "\n")

    # Debug before merge
    cat("\nBefore merging nodes:\n")
    cat("Unique nodes:", length(unique(nodes_all$Node)), "\n")
    cat("Range of Fitness values:", range(nodes_all$Fitness, na.rm = TRUE), "\n")
  }
  
  # Merge nodes with more detailed error handling
  merged_nodes <- nodes_all %>%
    group_by(Node) %>%
    summarise(
      Fitness = {
        vals <- Fitness[!is.na(Fitness)]
        if (length(vals) == 0) {
          warning(sprintf("No valid Fitness values for node %s", first(Node)))
          NA 
        } else {
          result <- switch(criteria,
            "min" = min(vals),
            "max" = max(vals),
            "mean" = mean(vals),
            "median" = median(vals),
            "mode" = as.numeric(names(sort(table(vals), decreasing = TRUE)[1]))
          )
          if (is.na(result)) {
            warning(sprintf("Calculation resulted in NA for node %s with values: %s", 
                          first(Node), paste(vals, collapse=", ")))
          }
          result
        }
      },
      Topology = {
        types <- unique(na.omit(Topology[Topology != ""]))
        if (length(types) == 0) {
          warning(sprintf("No valid Topology for node %s", first(Node)))
          "STANDARD"
        } else if ("END" %in% types) "END" 
        else if ("START" %in% types) "START" 
        else "STANDARD"
      },
      Count = sum(Count, na.rm = TRUE),
      Quality = {
        qual <- paste(Quality[!is.na(Quality) & Quality != ""], collapse = "")
        if (qual == "") warning(sprintf("Empty Quality for node %s", first(Node)))
        qual
      },
      Network = {
        net <- paste(Network[!is.na(Network) & Network != ""], collapse = "")
        if (net == "") warning(sprintf("Empty Network for node %s", first(Node)))
        net
      },
      n_networks = n(),
      .groups = "drop"
    )
    
  # Debug after merge
  if (verbose) {
    cat("\nAfter merging nodes:\n")
    cat("Number of merged nodes:", nrow(merged_nodes), "\n")
    cat("Number of NA in merged Fitness:", sum(is.na(merged_nodes$Fitness)), "\n")
    cat("Range of merged Fitness values:", range(merged_nodes$Fitness, na.rm = TRUE), "\n")
  }

  merged_nodes$Shared <- merged_nodes$n_networks > 1
  merged_nodes$Category <- mapply(function(q, shared) {
    # Split the Quality string into tags
    tags <- unlist(strsplit(q, "(?<=REGULAR)|(?<=ELITE)|(?<=BEST)", perl = TRUE))
    
    # Count occurrences of each tag (ignore BEST for this classification because it always changes)
    elite_count <- sum(tags == "ELITE")
    regular_count <- sum(tags == "REGULAR")
    #best_count <- sum(tags == "BEST")

    if (shared && elite_count > 0 && regular_count == 0) {
      "shared-elite"
    } else if (shared && elite_count > 0 && regular_count > 0) {
      "shared-mixed"
    } else if (shared && elite_count == 0) {
      "shared-regular"
    } else if (!shared && elite_count > 0) {
      "network-elite"
    } else {
      "network-regular"
    }
  }, merged_nodes$Quality, merged_nodes$Shared)

  # Rename for igraph compatibility
  names(merged_nodes)[names(merged_nodes) == "Node"] <- "name"

  # Build edge list
  edge_df_list <- list()
  for (i in seq_along(snts_i)) {
    g <- snts_i[[i]]
    edf <- data.frame(
      from = as.character(ends(g, es = E(g), names = TRUE)[,1]),
      to = as.character(ends(g, es = E(g), names = TRUE)[,2]),
      weight = E(g)$weight,
      Network = E(g)$Network,
      stringsAsFactors = FALSE
    )
    edf$weight <- E(g)$weight
    edf$Network <- E(g)$Network
    edge_df_list[[i]] <- edf
  }
  edges_all <- bind_rows(edge_df_list)

  merged_edges <- edges_all %>%
    group_by(from, to) %>%
    summarise(
      weight = sum(weight, na.rm = TRUE),
      Network = paste(Network[!is.na(Network) & Network != ""], collapse = ""),
      .groups = "drop"
    )

  # Create final graph
  merged_STN_i <- graph_from_data_frame(d = merged_edges, vertices = merged_nodes, directed = TRUE)

  # Identify and assign BEST node(s)
  fitness_vals <- V(merged_STN_i)$Fitness
  best_known_solution <- if (problem_type == "min") {
    best_known_solution <- min(fitness_vals)
  } else {
    best_known_solution <- max(fitness_vals)
  }
  best_ids <- if (problem_type == "min") {
    which(fitness_vals <= best_known_solution)
  } else {
    which(fitness_vals >= best_known_solution)
  }
  V(merged_STN_i)[best_ids]$Category <- "BEST"

  return(list(
    merged_STN_i = merged_STN_i,
    num_networks = num_networks,
    network_names = network_names,
    problem_type = problem_type,
    best_known_solution = best_known_solution,
    number_of_runs = number_of_runs
  ))
}

#' Save the merged STN-i data to a file
#' 
#'  This function saves the merged STN-i data to a specified output file path.
#' 
#'  @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#'  @param output_file_path A string specifying the path to the output file where the merged data will be saved.
#'  @param verbose A boolean indicating whether to print summary metrics before saving (default is FALSE).
#' 
#'  @return NULL
#' 
#' @examples
#' \dontrun{
#' save_merged_stn_i_data(merged_STN_i, "path/to/output.Rdata")
#' }
save_merged_stn_i_data <- function(merged_stn_i_data, output_file_path, verbose = FALSE) {
  # Ensure the file name ends in .Rdata
  if (!grepl("\\.Rdata$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".Rdata")
  }

  # Print summary metrics before saving
  g <- merged_stn_i_data$merged_STN_i
  if (verbose) {
    cat("\nSaving merged STN-i with summary metrics:\n")
    cat(" - Total nodes:", vcount(g), "\n")
    cat(" - Total edges:", ecount(g), "\n")
    cat(" - Shared nodes:", sum(V(g)$Shared), "\n")
    cat(" - shared-elite:", sum(V(g)$Category == "shared-elite"), "\n")
    cat(" - shared-regular:", sum(V(g)$Category == "shared-regular"), "\n")
    cat(" - shared-mixed:", sum(V(g)$Category == "shared-mixed"), "\n")
    cat(" - network-elite:", sum(V(g)$Category == "network-elite"), "\n")
    cat(" - network-regular:", sum(V(g)$Category == "network-regular"), "\n")
  }

  save(merged_stn_i_data, file = output_file_path)
  message(paste("Merged STN-i data saved to:", output_file_path))
}

#' Load merged STN-i data from a file
#' 
#' This function loads merged STN-i data from a specified input file and validates its structure.
#' 
#' @param input_file A string specifying the path to the input file containing the merged STN-i data.
#' 
#' @return A list containing the loaded merged STN-i data, including the merged graph, number of networks, network names, problem type, and best known solutions.
#' 
#' @examples
#' \dontrun{
#'  get_merged_stn_i_data("path/to/merged_stn_i_file.Rdata")
#' }
get_merged_stn_i_data <- function(input_file) {
  # Check if file exists
  if (!file.exists(input_file)) {
    stop(paste("Input file does not exist:", input_file), call. = FALSE)
  }

  # Load the object and retrieve its name
  loaded_name <- load(input_file)
  merged_stn_i_data <- get(loaded_name)

  # Validate structure
  expected_fields <- c(
    "merged_STN_i",
    "num_networks",
    "network_names",
    "problem_type",
    "best_known_solution",
    "number_of_runs"
  )

  if (!is.list(merged_stn_i_data) || !all(expected_fields %in% names(merged_stn_i_data))) {
    stop("The loaded file does not contain a valid merged STN-i result structure.", call. = FALSE)
  }

  return(merged_stn_i_data)
}

#' Get the palette colors for merged STN-i visualization
#'
#' This function returns a flat list of colors for merged STN-i visualization,
#' including shared categories, algorithm-specific elite/regular colors, and best node color.
#'
#' @param palette A numeric value specifying the color palette option (1 to 4).
#'
#' @return A named list of color values.
#'
#' @examples
#' \dontrun{
#' get_merged_stn_i_palette_colors(2)
#' }
get_merged_stn_i_palette_colors <- function(palette = 1) {
  palette_colors <- switch(as.character(palette),
    "1" = list(
      shapes = "black",
      shared_elite = "#FFD700",     # Gold
      shared_regular = "#CCCCCC",   # Light Gray
      shared_mixed = "#DA70D6",     # Orchid
      best = "red",             # Dark Orange
      algorithm_elite = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                          "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                          "#bcbd22", "#17becf"),
      algorithm_regular = c("#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
                            "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7",
                            "#dbdb8d", "#9edae5")
    ),
    "2" = list(
      shapes = "black",
      shared_elite = "#DAA520",     # Goldenrod
      shared_regular = "#D3D3D3",   # Light Gray
      shared_mixed = "#BA55D3",     # Medium Orchid
      best = "#FF6347",             # Tomato
      algorithm_elite = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                          "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                          "#999999", "#66c2a5"),
      algorithm_regular = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4",
                            "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec",
                            "#f2f2f2", "#b2df8a")
    ),
    "3" = list(
      shapes = "black",
      shared_elite = "#FFA500",     # Orange
      shared_regular = "#E0E0E0",   # Gray 88
      shared_mixed = "#DDA0DD",     # Plum
      best = "#B22222",             # Firebrick
      algorithm_elite = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3",
                          "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
                          "#a1d99b", "#9ecae1"),
      algorithm_regular = c("#ccece6", "#fdd0a2", "#bcbddc", "#fbb4b9",
                            "#c2e699", "#fff7bc", "#d9d9d9", "#e5f5e0",
                            "#edf8fb", "#f0f0f0")
    ),
    "4" = list(
      shapes = "black",
      shared_elite = "#C71585",     # Medium Violet Red
      shared_regular = "#B0C4DE",   # Light Steel Blue
      shared_mixed = "#9370DB",     # Medium Purple
      best = "#DC143C",             # Crimson
      algorithm_elite = c("#003f5c", "#2f4b7c", "#665191", "#a05195",
                          "#d45087", "#f95d6a", "#ff7c43", "#ffa600",
                          "#7a5195", "#ef5675"),
      algorithm_regular = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                            "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
                            "#cab2d6", "#6a3d9a")
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4.")
  )

  return(palette_colors)
}

#' Decorate the merged STN-i graph with colors and shapes
#'
#' This function decorates the merged STN-i graph by assigning colors and shapes to nodes
#' based on their category (shared, elite, regular, etc.) and topology (START, STANDARD, END).
#'
#' @param merged_STN_i The merged STN-i igraph object.
#' @param network_names A character vector of network names, used to assign algorithm colors.
#' @param problem_type A string indicating the problem type ("min" or "max").
#' @param show_shared_regular Logical; whether to show shared-regular nodes (default TRUE).
#' @param show_shared_mixed Logical; whether to show shared-mixed nodes (default TRUE).
#' @param show_regular Logical; whether to show network-regular (non-shared) nodes (default TRUE).
#' @param show_start_regular Logical; whether to show start regular nodes (default TRUE).
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#'
#' @return The decorated igraph object.
#' 
#' @examples
#' \dontrun{
#' merged_STN_i <- merged_stn_i_decorate(merged_STN_i, network_names, problem_type = "min", show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors = get_merged_stn_i_palette_colors(1))
#' }
merged_stn_i_decorate <- function(merged_STN_i, network_names, problem_type = "min", show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors) {

  # Filter vertices according to settings
  to_remove <- c()
  if (!show_shared_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "shared-regular"]$name)
  }
  if (!show_shared_mixed) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "shared-mixed"]$name)
  }
  if (!show_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Category == "network-regular"]$name)
  }
  if (!show_start_regular) {
    to_remove <- c(to_remove, V(merged_STN_i)[Topology == "START" & (Category == "network-regular" | Category == "shared-regular")]$name)
  }

  # Remove duplicates before deletion
  to_remove <- unique(to_remove)
  merged_STN_i <- delete_vertices(merged_STN_i, to_remove)

  # Set default color to NA
  V(merged_STN_i)$color <- NA

  # Assign colors based on category
  V(merged_STN_i)[Category == "shared-regular"]$color <- palette_colors$shared_regular
  V(merged_STN_i)[Category == "shared-elite"]$color <- palette_colors$shared_elite
  V(merged_STN_i)[Category == "shared-mixed"]$color <- palette_colors$shared_mixed
  V(merged_STN_i)[Category == "BEST"]$color <- palette_colors$best

  for (i in seq_along(network_names)) {
    network_name <- network_names[i]
    elite_color <- palette_colors$algorithm_elite[i]
    regular_color <- palette_colors$algorithm_regular[i]

    V(merged_STN_i)[Category == "network-elite" & Network == network_name]$color <- elite_color
    V(merged_STN_i)[Category == "network-regular" & Network == network_name]$color <- regular_color
  }

  # Set node shapes by topology
  V(merged_STN_i)$shape <- "circle"
  V(merged_STN_i)[Topology == "START"]$shape <- "square"
  V(merged_STN_i)[Topology == "END"]$shape <- "triangle"

  # Set node size proportional to in-degree (number of incoming visits)
  V(merged_STN_i)$size <- strength(merged_STN_i, mode = "in") + 1

  # Set edge width proportional to weight
  E(merged_STN_i)$width <- E(merged_STN_i)$weight

  # Set edge colors based on node colors
  # If both nodes have the same color, use that color for the edge;
  # otherwise, use the color of the "from" node
  edge_ends <- ends(merged_STN_i, es = E(merged_STN_i), names = FALSE)
  edge_colors <- apply(edge_ends, 1, function(e) {
    from_color <- V(merged_STN_i)$color[e[1]]
    to_color <- V(merged_STN_i)$color[e[2]]
    if (!is.na(from_color) && !is.na(to_color) && from_color == to_color) {
      return(from_color)
    } else {
      return(from_color)
    }
  })
  E(merged_STN_i)$color <- edge_colors

  return(merged_STN_i)
}

#' Create a plot for merged STN-i data
#' 
#' This function processes the input merged STN-i file, decorates the STN object,
#' and creates a plot.
#' 
#' @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#' @param show_shared Logical; whether to show shared nodes (default TRUE).
#' @param show_regular Logical; whether to show regular nodes (default TRUE).
#' @param show_elite Logical; whether to show elite nodes (default TRUE).
#' @param show_start_regular Logical; whether to show start regular nodes (default TRUE).
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#' @param zoom_quantile Numeric value between 0 and 1 to define a zoom level for the plot (default is NA, meaning no zoom).
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#'  merged_stn_i_plot_create(merged_stn_i_data, show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors = my_palette_colors)
#' }
merged_stn_i_plot_create <- function(merged_stn_i_data, show_shared_regular = TRUE, show_shared_mixed = TRUE, show_regular = TRUE, show_start_regular = TRUE, palette_colors, zoom_quantile = NA) {

  # Obtain the merged STN-i object, network names, and problem type
  merged_STN_i <- merged_stn_i_data$merged_STN_i
  network_names = merged_stn_i_data$network_names
  problem_type <- merged_stn_i_data$problem_type

  # Decorate the STN-i object
  merged_STN_i <- merged_stn_i_decorate(merged_STN_i, network_names, problem_type, show_shared_regular, show_shared_mixed, show_regular, show_start_regular, palette_colors)

  # If zooming is enabled, extract subgraph
  if (!is.na(zoom_quantile) && zoom_quantile > 0 && zoom_quantile < 1) {
    merged_STN_i <- get_zoomed_graph(merged_STN_i, zoom_quantile, problem_type)
  }

  # Return everything needed for external plotting
  return(merged_STN_i)
}

#' Save a plot of the merged STN-i graph
#'
#' This function saves a decorated merged STN-i graph to a PDF file,
#' including legends for shared and network-specific nodes, using a given layout.
#'
#' @param output_file_path Output path for the PDF plot.
#' @param merged_STN_i A decorated igraph object of the merged STN-i.
#' @param network_names A character vector of network names, used for legend labels.
#' @param layout_data A list with layout coordinates and title.
#' @param palette_colors A palette list returned from get_merged_stn_i_palette_colors().
#' @param nsizef Numeric factor for node sizes.
#' @param ewidthf Numeric factor for edge widths.
#' @param asize Arrow size for edges.
#' @param ecurv Curvature of edges.
#'
#' @return None. Saves a PDF to the specified path.
#' 
#' @examples
#' \dontrun{
#' save_merged_stn_i_plot("output/merged_stn_i_plot.pdf", merged_STN_i, network_names, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3)
#' }
save_merged_stn_i_plot <- function(output_file_path, merged_STN_i, network_names, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3) {

  # Ensure the file name ends in .pdf
  if (!grepl("\\.pdf$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".pdf")
  }

  # Triangle shape for END nodes
  mytriangle <- function(coords, v = NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1 / 200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color, col = vertex.color,
            stars = cbind(vertex.size, vertex.size, vertex.size),
            add = TRUE, inches = FALSE)
  }
  add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

  # Legend setup
  legend.txt <- c("Start", "Standard", "End", "Best")
  legend.col <- c(palette_colors$shapes, palette_colors$shapes, palette_colors$shapes, palette_colors$best)
  legend.shape <- c(15, 16, 17, 16)

  # Shared node categories
  legend.txt <- c(legend.txt, "Shared-Regular", "Shared-Elite", "Shared-Mixed")
  legend.col <- c(legend.col, palette_colors$shared_regular, palette_colors$shared_elite, palette_colors$shared_mixed)
  legend.shape <- c(legend.shape, 16, 16, 16)

  # Network-specific categories
  for (i in seq_along(network_names)) {
    legend.txt <- c(legend.txt,
                    paste0(network_names[i], "-Regular"),
                    paste0(network_names[i], "-Elite"))
    legend.col <- c(legend.col,
                    palette_colors$algorithm_regular[i],
                    palette_colors$algorithm_elite[i])
    legend.shape <- c(legend.shape, 16, 16)
  }

  # Open PDF device
  pdf(output_file_path)

  # Compute node sizes
  maxns <- max(V(merged_STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(merged_STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(merged_STN_i)$size + 1
  } else {
    nsize <- nsizef * V(merged_STN_i)$size
  }

  ewidth <- ewidthf * E(merged_STN_i)$width
  title <- paste(layout_data$title, "\nNodes:", vcount(merged_STN_i), "Edges:", ecount(merged_STN_i))

  # Plot
  plot(merged_STN_i, layout = layout_data$coords, vertex.label = "", vertex.size = nsize, main = title, edge.width = ewidth, edge.arrow.size = asize, edge.curved = ecurv)

  legend("topleft", legend.txt, pch = legend.shape, col = legend.col, pt.bg = legend.col, cex = 0.7, pt.cex = 1.35, bty = "n")

  dev.off()

  message("Merged STN-i plot saved successfully to: ", output_file_path)
}

#' Get zoomed subgraph of STN-i based on fitness quantile
#' 
#' This function extracts a subgraph from the given STN-i graph where the vertex fitness values are
#' either below or above a quantile threshold depending on the problem type.
#' 
#' @param graph An igraph object representing the STN-i graph or merged STN-i graph.
#' @param quantile_value A numeric value between 0 and 1 indicating the quantile threshold.
#' @param problem_type Either "min" (default) or "max" to define optimization direction.
#' 
#' @return An igraph object representing the zoomed subgraph.
#' 
#' @examples
#' zoomed_graph <- get_zoomed_graph(my_graph, quantile_value = 0.25, problem_type = "min")
get_zoomed_graph <- function(graph, quantile_value = 0.25, problem_type = "min") {

  if (!"Fitness" %in% vertex_attr_names(graph)) {
    stop("Graph must have a 'Fitness' vertex attribute for zooming.")
  }

  if (!(problem_type %in% c("min", "max"))) {
    stop("Invalid problem_type. Must be 'min' or 'max'.")
  }

  fitness_values <- V(graph)$Fitness
  threshold <- quantile(fitness_values, probs = quantile_value, na.rm = TRUE)

  # Select nodes based on the threshold and problem type
  if (problem_type == "min") {
    selected_nodes <- V(graph)[fitness_values <= threshold]
  } else {
    selected_nodes <- V(graph)[fitness_values >= threshold]
  }

  subgraph <- induced_subgraph(graph, selected_nodes)
  subgraph <- delete_vertices(subgraph, degree(subgraph) == 0)
  return(subgraph)
}

#' Get metrics from a single STN-i result
#'
#' This function extracts various metrics from a single STN-i result object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @param stn_i_result A list containing the STN-i result, including the graph object and metadata.
#'
#' @return A list of metrics extracted from the STN-i result, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @examples
#'
#' \dontrun{
#' stn_i_result <- get_stn_i_data("path/to/stn_i_file.Rdata")
#' metrics <- get_stn_i_metrics(stn_i_result)
#' }
get_stn_i_metrics <- function(stn_i_result) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the STN-i and metadata
  STN_i <- stn_i_result$STN_i
  network_name <- stn_i_result$network_name
  problem_type <- stn_i_result$problem_type
  best_known_solution <- stn_i_result$best_known_solution
  number_of_runs <- stn_i_result$number_of_runs

  # Retrieve edge list and corresponding fitness values
  edge_list <- as_edgelist(STN_i)
  node_fitness <- V(STN_i)$Fitness
  node_names <- V(STN_i)$name

  # Extract fitness of origin and destination nodes for each edge
  fitness_from <- node_fitness[match(edge_list[, 1], node_names)]
  fitness_to   <- node_fitness[match(edge_list[, 2], node_names)]

  # Direct metrics
  metrics$network_name <- network_name
  metrics$problem_type <- problem_type
  metrics$number_of_runs <- number_of_runs
  metrics$best_known_solution <- best_known_solution
  metrics$components <- components(STN_i)$no

  # Compute nodes quantity metrics
  metrics$nodes <- vcount(STN_i)
  metrics$regular_nodes <- sum(V(STN_i)$IS_ELITE == FALSE)
  metrics$elite_nodes <- sum(V(STN_i)$IS_ELITE == TRUE)
  metrics$start_nodes <- sum(V(STN_i)$TOPOLOGY == "START")
  metrics$standard_nodes <- sum(V(STN_i)$TOPOLOGY == "STANDARD")
  metrics$end_nodes <- sum(V(STN_i)$TOPOLOGY == "END")

  # Compute edges quantity metrics
  metrics$edges <- ecount(STN_i)
  metrics$worsening_edges <- sum(E(STN_i)$Type == "WORSENING")
  metrics$equal_edges <- sum(E(STN_i)$Type == "EQUAL")
  metrics$improving_edges <- sum(E(STN_i)$Type == "IMPROVING")

  # Compute degree metrics
  #metrics$average_degree <- mean(degree(STN_i, mode = "all"), na.rm = TRUE)
  #metrics$average_in_degree <- mean(degree(STN_i, mode = "in"), na.rm = TRUE)
  #metrics$average_out_degree <- mean(degree(STN_i, mode = "out"), na.rm = TRUE)
  #metrics$average_degree <- mean(degree(STN_i, mode = "all"), na.rm = TRUE)
  regular_nodes <- V(STN_i)[V(STN_i)$IS_ELITE == FALSE]
  if (length(regular_nodes) > 0) {
    metrics$average_regular_in_degree  <- mean(degree(STN_i, v = regular_nodes, mode = "in"), na.rm = TRUE)
    #metrics$average_regular_out_degree <- mean(degree(STN_i, v = regular_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_regular_in_degree  <- NA
    #metrics$average_regular_out_degree <- NA
  }
  elite_nodes <- V(STN_i)[V(STN_i)$IS_ELITE == TRUE]
  if (length(elite_nodes) > 0) {
    metrics$average_elite_in_degree  <- mean(degree(STN_i, v = elite_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_elite_out_degree <- mean(degree(STN_i, v = elite_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_elite_in_degree  <- NA
    metrics$average_elite_out_degree <- NA
  }

  # Compute best nodes metrics
  metrics$best_nodes <- sum(V(STN_i)$IS_BEST == TRUE)
  best_ids <- which(V(STN_i)$IS_BEST == TRUE)
  if (length(best_ids) > 0) {
    metrics$average_best_in_degree <- mean(degree(STN_i, v = best_ids, mode = "in"), na.rm = TRUE)
    metrics$average_best_out_degree <- mean(degree(STN_i, v = best_ids, mode = "out"), na.rm = TRUE)
    metrics$best_strength_in <- sum(strength(STN_i, vids = best_ids,  mode="in")) / number_of_runs
    metrics$start_best_nodes <- sum(V(STN_i)$Topology == "START" & V(STN_i)$Quality == "BEST")
    metrics$standard_best_nodes <- sum(V(STN_i)$Topology == "STANDARD" & V(STN_i)$Quality == "BEST")
    metrics$end_best_nodes <- sum(V(STN_i)$Topology == "END" & V(STN_i)$Quality == "BEST")
  } else {
    metrics$average_best_in_degree <- NA
    metrics$average_best_out_degree <- NA
    metrics$best_strength_in <- NA
    metrics$start_best_nodes <- NA
    metrics$standard_best_nodes <- NA
    metrics$end_best_nodes <- NA
  }
  start_ids <- which(V(STN_i)$Topology == "START")
  if (length(best_ids) > 0 & length(start_ids) > 0) {
    dist_matrix <- distances(STN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
    finite_distances <- dist_matrix[is.finite(dist_matrix)]
    metrics$average_path_length <- mean(finite_distances)
    metrics$paths <- length(finite_distances)
  } else {
    metrics$average_path_length <- NA
    metrics$paths <- NA
  }

  # Compute configurations metrics
  metrics$configurations <- sum(V(STN_i)$CONFIGURATIONS)
  metrics$regular_configurations <- sum(V(STN_i)$REGULARS)
  metrics$elite_configurations <- sum(V(STN_i)$ELITES)
  metrics$start_configurations <- sum(V(STN_i)$STARTS)
  metrics$standard_configurations <- sum(V(STN_i)$STANDARDS)
  metrics$end_configurations <- sum(V(STN_i)$ENDS)

  # Compute correlation for configurations metrics
  metrics$regular_start_configurations <- sum(V(STN_i)$REGULAR_STARTS)
  metrics$regular_standard_configurations <- sum(V(STN_i)$REGULAR_STANDARDS)
  metrics$regular_end_configurations <- sum(V(STN_i)$REGULAR_ENDS)
  metrics$elite_start_configurations <- sum(V(STN_i)$ELITE_STARTS)
  metrics$elite_standard_configurations <- sum(V(STN_i)$ELITE_STANDARDS)
  metrics$elite_end_configurations <- sum(V(STN_i)$ELITE_ENDS)

  # Compute max/min configurations metrics
  metrics$max_configurations <- max(V(STN_i)$CONFIGURATIONS)
  metrics$min_configurations <- min(V(STN_i)$CONFIGURATIONS)
  metrics$max_regular_configurations <- max(V(STN_i)$REGULARS)
  metrics$min_regular_configurations <- min(V(STN_i)$REGULARS)
  metrics$max_elite_configurations <- max(V(STN_i)$ELITES)
  metrics$min_elite_configurations <- min(V(STN_i)$ELITES)
  metrics$max_start_configurations <- max(V(STN_i)$STARTS)
  metrics$min_start_configurations <- min(V(STN_i)$STARTS)
  metrics$max_standard_configurations <- max(V(STN_i)$STANDARDS)
  metrics$min_standard_configurations <- min(V(STN_i)$STANDARDS)
  metrics$max_end_configurations <- max(V(STN_i)$ENDS)
  metrics$min_end_configurations <- min(V(STN_i)$ENDS)

  # Compute max/min correlation for configurations metrics
  metrics$max_regular_start_configurations <- max(V(STN_i)$REGULAR_STARTS)
  metrics$min_regular_start_configurations <- min(V(STN_i)$REGULAR_STARTS)
  metrics$max_regular_standard_configurations <- max(V(STN_i)$REGULAR_STANDARDS)
  metrics$min_regular_standard_configurations <- min(V(STN_i)$REGULAR_STANDARDS)
  metrics$max_regular_end_configurations <- max(V(STN_i)$REGULAR_ENDS)
  metrics$min_regular_end_configurations <- min(V(STN_i)$REGULAR_ENDS)
  metrics$max_elite_start_configurations <- max(V(STN_i)$ELITE_STARTS)
  metrics$min_elite_start_configurations <- min(V(STN_i)$ELITE_STARTS)
  metrics$max_elite_standard_configurations <- max(V(STN_i)$ELITE_STANDARDS)
  metrics$min_elite_standard_configurations <- min(V(STN_i)$ELITE_STANDARDS)
  metrics$max_elite_end_configurations <- max(V(STN_i)$ELITE_ENDS)
  metrics$min_elite_end_configurations <- min(V(STN_i)$ELITE_ENDS)

  # Compute percentage metrics for nodes
  if (metrics$nodes > 0) {
    metrics$best_nodes_rate <- metrics$best_nodes / metrics$nodes
    metrics$regular_nodes_rate <- metrics$regular_nodes / metrics$nodes
    metrics$elite_nodes_rate <- metrics$elite_nodes / metrics$nodes
    metrics$start_nodes_rate <- metrics$start_nodes / metrics$nodes
    metrics$standard_nodes_rate <- metrics$standard_nodes / metrics$nodes
    metrics$end_nodes_rate <- metrics$end_nodes / metrics$nodes
  } else {
    metrics$best_nodes_rate <- NA
    metrics$regular_nodes_rate <- NA
    metrics$elite_nodes_rate <- NA
    metrics$start_nodes_rate <- NA
    metrics$standard_nodes_rate <- NA
    metrics$end_nodes_rate <- NA
  }

  # Compute percentage metrics for edges
  if (metrics$edges > 0) {
    metrics$worsening_edges_rate <- metrics$worsening_edges / metrics$edges
    metrics$equal_edges_rate <- metrics$equal_edges / metrics$edges
    metrics$improving_edges_rate <- metrics$improving_edges / metrics$edges
  } else {
    metrics$worsening_edges_rate <- NA
    metrics$equal_edges_rate <- NA
    metrics$improving_edges_rate <- NA
  }

  # Compute percentage metrics for configurations and correlation metrics
  if (metrics$configurations > 0) {
    metrics$regular_configurations_rate <- metrics$regular_configurations / metrics$configurations
    metrics$elite_configurations_rate <- metrics$elite_configurations / metrics$configurations
    metrics$start_configurations_rate <- metrics$start_configurations / metrics$configurations
    metrics$standard_configurations_rate <- metrics$standard_configurations / metrics$configurations
    metrics$end_configurations_rate <- metrics$end_configurations / metrics$configurations
    metrics$regular_start_configurations_rate <- metrics$regular_start_configurations / metrics$configurations
    metrics$regular_standard_configurations_rate <- metrics$regular_standard_configurations / metrics$configurations
    metrics$regular_end_configurations_rate <- metrics$regular_end_configurations / metrics$configurations
    metrics$elite_start_configurations_rate <- metrics$elite_start_configurations / metrics$configurations
    metrics$elite_standard_configurations_rate <- metrics$elite_standard_configurations / metrics$configurations
    metrics$elite_end_configurations_rate <- metrics$elite_end_configurations / metrics$configurations
  } else {
    metrics$regular_configurations_rate <- NA
    metrics$elite_configurations_rate <- NA
    metrics$start_configurations_rate <- NA
    metrics$standard_configurations_rate <- NA
    metrics$end_configurations_rate <- NA
    metrics$regular_start_configurations_rate <- NA
    metrics$regular_standard_configurations_rate <- NA
    metrics$regular_end_configurations_rate <- NA
    metrics$elite_start_configurations_rate <- NA
    metrics$elite_standard_configurations_rate <- NA
    metrics$elite_end_configurations_rate <- NA
  }

  return(metrics)
}

#' Save STN-i metrics to a CSV file
#'
#' This function saves a list of STN-i metrics to a CSV file.
#'
#' @param stn_i_metrics A list of metrics extracted from STN-i results, typically returned by `get_stn_i_metrics()`.
#' @param output_file_path A string specifying the path to the output CSV file where the metrics will be saved.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_stn_i_metrics(stn_i_metrics, "path/to/stn_i_metrics.csv")
#' }
save_stn_i_metrics <- function(stn_i_metrics, output_file_path) {
  # Ensure the file name ends in .csv
  if (!grepl("\\.csv$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".csv")
  }

  # Convert the named list to a data frame: names as columns, values as first row
  metrics_df <- as.data.frame(stn_i_metrics, stringsAsFactors = FALSE)
  # Ensure it's a single row
  if (is.list(metrics_df) && nrow(metrics_df) != 1) {
    metrics_df <- as.data.frame(t(unlist(stn_i_metrics)), stringsAsFactors = FALSE)
  }

  # Save the data frame to a CSV file with semicolon separator and header
  write.table(
    metrics_df,
    file = output_file_path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    dec = "."
  )

  message(paste("STN-i metrics saved to:", output_file_path))
}

#' Get metrics from a merged STN-i object
#'
#' This function computes various metrics from a merged STN-i graph object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @param merged_stn_i_data A list containing the merged STN-i data, typically returned by `merge_stns_i_data()`.
#'
#' @return A list of metrics extracted from the merged STN-i object, including node and edge counts, fitness comparisons, and configuration rates.
#'
#' @examples
#' \dontrun{
#' merged_stn_i_data <- get_merged_stn_i_data("path/to/merged_stn_i_file.Rdata")
#' metrics <- get_merged_stn_i_metrics(merged_stn_i_data)
#' }
get_merged_stn_i_metrics <- function(merged_stn_i_data) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the merged STN-i object and metadata
  merged_STN_i <- merged_stn_i_data$merged_STN_i
  num_networks <- merged_stn_i_data$num_networks
  network_names <- merged_stn_i_data$network_names
  problem_type <- merged_stn_i_data$problem_type
  number_of_runs <- merged_stn_i_data$number_of_runs
  best_known_solution <- merged_stn_i_data$best_known_solution

  # General metrics
  metrics$network_names <- paste(network_names, collapse = ", ")
  metrics$problem_type <- problem_type
  metrics$best_known_solution <- best_known_solution
  metrics$number_of_networks <- num_networks

  # Compute normal metrics
  metrics$nodes <- vcount(merged_STN_i)
  metrics$start_nodes <- sum(V(merged_STN_i)$Topology == "START")
  metrics$standard_nodes <- sum(V(merged_STN_i)$Topology == "STANDARD")
  metrics$end_nodes <- sum(V(merged_STN_i)$Topology == "END")
  metrics$edges <- ecount(merged_STN_i)
  metrics$best_nodes <- sum(V(merged_STN_i)$Category == "BEST")
  metrics$average_degree <- mean(degree(merged_STN_i))
  metrics$average_in_degree <- mean(degree(merged_STN_i, mode = "in"))
  metrics$average_out_degree <- mean(degree(merged_STN_i, mode = "out"))

  best_ids <- which(V(merged_STN_i)$Category == "BEST")
  if (length(best_ids) > 0) {
    metrics$average_best_in_degree <- mean(degree(merged_STN_i, v = best_ids, mode = "in"), na.rm = TRUE)
    metrics$average_best_out_degree <- mean(degree(merged_STN_i, v = best_ids, mode = "out"), na.rm = TRUE)
    metrics$best_strength_in <- sum(strength(merged_STN_i, vids = best_ids,  mode="in")) / number_of_runs
    metrics$start_best_nodes <- sum(V(merged_STN_i)$Topology == "START" & V(merged_STN_i)$Category == "BEST")
    metrics$standard_best_nodes <- sum(V(merged_STN_i)$Topology == "STANDARD" & V(merged_STN_i)$Category == "BEST")
    metrics$end_best_nodes <- sum(V(merged_STN_i)$Topology == "END" & V(merged_STN_i)$Category == "BEST")
  } else {
    metrics$average_best_in_degree <- NA
    metrics$average_best_out_degree <- NA
    metrics$best_strength_in <- NA
    metrics$start_best_nodes <- NA
    metrics$standard_best_nodes <- NA
    metrics$end_best_nodes <- NA
  }

  start_ids <- which(V(merged_STN_i)$Topology == "START")
  if (length(best_ids) > 0 & length(start_ids) > 0) {
    dist_matrix <- distances(merged_STN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
    finite_distances <- dist_matrix[is.finite(dist_matrix)]
    metrics$average_path_length <- mean(finite_distances)
    metrics$paths <- length(finite_distances)
  } else {
    metrics$average_path_length <- NA
    metrics$paths <- NA
  }
  metrics$components <- components(merged_STN_i)$no

  # Compute node categories (only merged metrics)
  metrics$shared_nodes <- sum(V(merged_STN_i)$Shared == TRUE)
  metrics$shared_regular_nodes <- sum(V(merged_STN_i)$Category == "shared-regular")
  metrics$shared_elite_nodes <- sum(V(merged_STN_i)$Category == "shared-elite")
  metrics$shared_mixed_nodes <- sum(V(merged_STN_i)$Category == "shared-mixed")
  metrics$network_regular_nodes <- sum(V(merged_STN_i)$Category == "network-regular")
  metrics$network_elite_nodes <- sum(V(merged_STN_i)$Category == "network-elite")

  # Compute percentage metrics for nodes
  if (metrics$nodes > 0) {
    metrics$best_nodes_rate <- metrics$best_nodes / metrics$nodes
    metrics$start_rate <- metrics$start_nodes / metrics$nodes
    metrics$standard_rate <- metrics$standard_nodes / metrics$nodes
    metrics$end_rate <- metrics$end_nodes / metrics$nodes
    metrics$shared_rate <- metrics$shared_nodes / metrics$nodes
    metrics$shared_regular_rate <- metrics$shared_regular_nodes / metrics$nodes
    metrics$shared_elite_rate <- metrics$shared_elite_nodes / metrics$nodes
    metrics$shared_mixed_rate <- metrics$shared_mixed_nodes / metrics$nodes
    metrics$network_regular_rate <- metrics$network_regular_nodes / metrics$nodes
    metrics$network_elite_rate <- metrics$network_elite_nodes / metrics$nodes
  } else {
    metrics$best_nodes_rate <- NA
    metrics$start_rate <- NA
    metrics$standard_rate <- NA
    metrics$end_rate <- NA
    metrics$shared_rate <- NA
    metrics$shared_regular_rate <- NA
    metrics$shared_elite_rate <- NA
    metrics$shared_mixed_rate <- NA
    metrics$network_regular_rate <- NA
    metrics$network_elite_rate <- NA
  }

  # Compute degrees for shared nodes (only merged metrics)
  shared_nodes <- V(merged_STN_i)[V(merged_STN_i)$Shared == TRUE]
  if (length(shared_nodes) > 0) {
    metrics$average_shared_in_degree <- mean(degree(merged_STN_i, v = shared_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_out_degree <- mean(degree(merged_STN_i, v = shared_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_in_degree <- NA
    metrics$average_shared_out_degree <- NA
  }

  shared_regular_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-regular"]
  if (length(shared_regular_nodes) > 0) {
    metrics$average_shared_regular_in_degree <- mean(degree(merged_STN_i, v = shared_regular_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_regular_out_degree <- mean(degree(merged_STN_i, v = shared_regular_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_regular_in_degree <- NA
    metrics$average_shared_regular_out_degree <- NA
  }

  shared_elite_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-elite"]
  if (length(shared_elite_nodes) > 0) {
    metrics$average_shared_elite_in_degree <- mean(degree(merged_STN_i, v = shared_elite_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_elite_out_degree <- mean(degree(merged_STN_i, v = shared_elite_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_elite_in_degree <- NA
    metrics$average_shared_elite_out_degree <- NA
  }

  shared_mixed_nodes <- V(merged_STN_i)[V(merged_STN_i)$Category == "shared-mixed"]
  if (length(shared_mixed_nodes) > 0) {
    metrics$average_shared_mixed_in_degree <- mean(degree(merged_STN_i, v = shared_mixed_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_shared_mixed_out_degree <- mean(degree(merged_STN_i, v = shared_mixed_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_shared_mixed_in_degree <- NA
    metrics$average_shared_mixed_out_degree <- NA
  }

  return(metrics)
}

#' Save merged STN-i metrics to a CSV file
#'
#' This function saves a list of merged STN-i metrics to a CSV file.
#'
#' @param merged_stn_i_metrics A list of metrics extracted from merged STN-i results, typically returned by `get_merged_stn_i_metrics()`.
#' @param output_file_path A string specifying the path to the output CSV file where the metrics will be saved.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_merged_stn_i_metrics(merged_stn_i_metrics, "output/merged_stn_i_metrics.csv")
#' }
save_merged_stn_i_metrics <- function(merged_stn_i_metrics, output_file_path) {
  # Ensure the file name ends in .csv
  if (!grepl("\\.csv$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".csv")
  }

  # Convert the named list to a data frame: names as columns, values as first row
  metrics_df <- as.data.frame(merged_stn_i_metrics, stringsAsFactors = FALSE)
  # Ensure it's a single row
  if (is.list(metrics_df) && nrow(metrics_df) != 1) {
    metrics_df <- as.data.frame(t(unlist(merged_stn_i_metrics)), stringsAsFactors = FALSE)
  }

  # Save the data frame to a CSV file with semicolon separator and header
  write.table(
    metrics_df,
    file = output_file_path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    dec = "."
  )

  message(paste("Merged STN-i metrics saved to:", output_file_path))
}

#' Process elite configurations from multiple irace runs
#' 
#' This function processes multiple .Rdata files from irace runs and extracts
#' unique elite configurations, creating a mapping between runs and configurations.
#' 
#' @param runs_data A list of irace run data, each containing elites information
#' @param parameters The parameters data frame from irace
#' 
#' @return A list containing:
#'   - unique_elites: Data frame of unique elite configurations with parameter values
#'   - runs_mapping: Data frame mapping run information to configurations
#'
#' @keywords internal
process_elite_configurations <- function(runs_data, parameters) {
    param_names <- parameters$NAME
    param_types <- setNames(parameters$TYPE, parameters$NAME)
    
    unique_elites <- data.frame()
    runs_mapping <- data.frame(
        RUN_ID = integer(),
        RUN_NAME = character(),
        CONFIG_ID = integer(),
        CONFIG_POS = integer()
    )
    
    config_counter <- 1
    
    # Process each run
    for (run_id in seq_along(runs_data)) {
        run_data <- runs_data[[run_id]]
        run_name <- names(runs_data)[run_id]
        
        # Get elite configurations from this run
        elites <- get_run_elites(run_data)
        
        # Process each elite configuration
        for (i in seq_along(elites)) {
            elite_config <- elites[[i]]
            
            # Convert configuration to parameter values
            param_values <- get_parameter_values(elite_config, parameters)
            
            # Check if this configuration already exists
            existing_pos <- find_matching_configuration(param_values, unique_elites)
            
            if (is.na(existing_pos)) {
                # New unique configuration
                unique_elites <- rbind(unique_elites, param_values)
                config_pos <- nrow(unique_elites)
            } else {
                # Configuration already exists
                config_pos <- existing_pos
            }
            
            # Add mapping entry
            runs_mapping <- rbind(runs_mapping, data.frame(
                RUN_ID = run_id,
                RUN_NAME = run_name,
                CONFIG_ID = elite_config$id,
                CONFIG_POS = config_pos
            ))
        }
    }
    
    return(list(
        unique_elites = unique_elites,
        runs_mapping = runs_mapping
    ))
}

#' Get elite configurations from a single irace run
#' 
#' This function extracts all elite configurations from an irace run.
#' 
#' @param run_data The irace run data loaded from a .Rdata file
#' 
#' @return A list of configurations with their IDs
#'
#' @keywords internal
get_run_elites <- function(run_data) {
    # Initialize empty list for elite configurations
    elites <- list()
    elite_ids <- c()
    
    # Extract elite IDs from all iterations
    if (!is.null(run_data$allElites)) {
        for (iter in seq_along(run_data$allElites)) {
            elite_ids <- c(elite_ids, run_data$allElites[[iter]])
        }
        # Remove duplicates
        elite_ids <- unique(elite_ids)
        
        # Get configurations for each elite ID
        for (id in elite_ids) {
            # Find configuration in allConfigurations
            config_row <- run_data$allConfigurations[run_data$allConfigurations$.ID. == id, ]
            if (nrow(config_row) > 0) {
                elites[[length(elites) + 1]] <- list(
                    id = id,
                    values = config_row
                )
            }
        }
    }
    
    return(elites)
}

#' Extract parameter values from a configuration
#' 
#' This function extracts parameter values from a configuration in the format
#' required for the output file.
#' 
#' @param config The configuration object from irace containing values data frame
#' @param parameters The parameters data frame from irace
#' 
#' @return A data frame row with parameter values
#'
#' @keywords internal
get_parameter_values <- function(config, parameters) {
    # Get parameter names excluding special columns from irace
    param_names <- setdiff(names(config$values), c(".ID.", ".PARENT."))
    param_types <- setNames(parameters$TYPE, parameters$NAME)
    
    # Extract values for each parameter
    param_values <- sapply(param_names, function(pname) {
        value <- config$values[[pname]]
        if (is.na(value)) {
            return(NA)
        }
        type <- param_types[[pname]]
        if (type %in% c("c", "o", "cat", "ord")) {
            return(paste0('"', as.character(value), '"'))
        } else if (type %in% c("i", "int", "i,log")) {
            return(as.character(as.integer(value)))
        } else if (type %in% c("r", "real", "r,log")) {
            return(as.character(as.numeric(value)))
        } else {
            return(as.character(value))
        }
    })
    
    values_df <- as.data.frame(t(param_values), stringsAsFactors = FALSE)
    return(values_df)
}

#' Find matching configuration in existing set
#' 
#' This function checks if a configuration already exists in the set of unique configurations.
#' 
#' @param config The configuration to check
#' @param existing_configs Data frame of existing configurations
#' 
#' @return Position of matching configuration or NA if not found
#'
#' @keywords internal
find_matching_configuration <- function(config, existing_configs) {
    if (nrow(existing_configs) == 0) return(NA)
    
    for (i in 1:nrow(existing_configs)) {
        if (all(config == existing_configs[i,])) {
            return(i)
        }
    }
    return(NA)
}

#' Create configurations file for elite configurations only
#' 
#' This function creates a configurations file containing only elite configurations
#' from an irace run.
#' 
#' @param allConfigurations The allConfigurations data frame from iraceResults
#' @param elite_ids Vector of elite configuration IDs
#' @param parameters The parameters data frame
#' @param output_file Path to save the configurations CSV file
#' 
#' @return Data frame with elite configurations
#' 
#' @export
create_elite_configurations_file <- function(allConfigurations, elite_ids, parameters, output_file) {
  param_names <- intersect(parameters$NAME, colnames(allConfigurations))
  param_types <- setNames(parameters$TYPE, parameters$NAME)
  
  config_list <- lapply(elite_ids, function(id) {
    param_row <- allConfigurations[allConfigurations$.ID. == as.integer(id), , drop = FALSE]
    
    if (nrow(param_row) == 0) {
      return(NULL)
    }
    
    param_values <- sapply(param_names, function(pname) {
      if (!(pname %in% colnames(param_row)) || is.na(param_row[[pname]])) {
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
    
    c(
      CONFIG_ID = as.character(id),
      IS_ELITE = "TRUE",  # All configs in this file are elite
      param_values
    )
  })
  
  # Remove NULL entries
  config_list <- config_list[!sapply(config_list, is.null)]
  
  if (length(config_list) == 0) {
    config_df <- data.frame()
  } else {
    config_df <- as.data.frame(do.call(rbind, config_list), stringsAsFactors = FALSE)
  }
  
  write.table(config_df, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
  return(config_df)
}

#' Create trajectories file for elite configurations only
#' 
#' This function creates a trajectories file that captures only the paths
#' between elite configurations across iterations.
#' 
#' @param iraceResults The irace results from read_logfile
#' @param elite_ids Vector of elite configuration IDs
#' @param output_file Path to save the trajectories CSV file
#' 
#' @return Data frame with elite trajectories
#' 
#' @export
create_elite_trajectories_file <- function(iraceResults, elite_ids, output_file) {
  # Initialize data frame for trajectories
  trajectories <- data.frame(
    PATH = logical(),
    ORIGIN_ITER = integer(),
    ORIGIN_CONFIG_ID = character(),
    DESTINY_ITER = integer(),
    DESTINY_CONFIG_ID = character(),
    stringsAsFactors = FALSE
  )
  
  # Get number of iterations
  iterations <- length(iraceResults$allElites)
  raceData <- iraceResults$raceData
  
  # Create a set for quick lookup
  elite_set <- as.character(elite_ids)
  
  # Track which elite configs appear in which iterations
  elite_by_iteration <- list()
  for (iter in 1:iterations) {
    if (!is.null(raceData[[iter]])) {
      iter_configs <- rownames(raceData[[iter]])
      elite_by_iteration[[iter]] <- intersect(iter_configs, elite_set)
    } else {
      elite_by_iteration[[iter]] <- character(0)
    }
  }
  
  # Process each iteration
  for (iter in 1:iterations) {
    configs <- raceData[[iter]]
    if (is.null(configs) || nrow(configs) == 0) next
    
    # Process only elite configurations
    for (id_str in elite_by_iteration[[iter]]) {
      config_id <- as.integer(id_str)
      config <- configs[as.character(config_id), , drop = FALSE]
      
      if (iter == 1) {
        # First iteration: elite configs connect to themselves with PATH = FALSE
        trajectories <- rbind(trajectories, data.frame(
          PATH = FALSE,
          ORIGIN_ITER = 1,
          ORIGIN_CONFIG_ID = id_str,
          DESTINY_ITER = 1,
          DESTINY_CONFIG_ID = id_str,
          stringsAsFactors = FALSE
        ))
      } else {
        parent_id <- as.character(config$.PARENT.)
        
        # Check if parent is also elite
        is_parent_elite <- parent_id %in% elite_set
        
        if (!is.na(parent_id) && is_parent_elite && parent_id %in% elite_by_iteration[[iter-1]]) {
          # Parent is elite and was in previous iteration
          trajectories <- rbind(trajectories, data.frame(
            PATH = TRUE,
            ORIGIN_ITER = iter - 1,
            ORIGIN_CONFIG_ID = parent_id,
            DESTINY_ITER = iter,
            DESTINY_CONFIG_ID = id_str,
            stringsAsFactors = FALSE
          ))
        } else {
          # No elite parent connection, check if it survived from previous iteration
          if (id_str %in% elite_by_iteration[[iter-1]]) {
            # It survived, connect to itself
            trajectories <- rbind(trajectories, data.frame(
              PATH = FALSE,
              ORIGIN_ITER = iter - 1,
              ORIGIN_CONFIG_ID = id_str,
              DESTINY_ITER = iter,
              DESTINY_CONFIG_ID = id_str,
              stringsAsFactors = FALSE
            ))
          } else {
            # New elite in this iteration without elite parent, connect to itself
            trajectories <- rbind(trajectories, data.frame(
              PATH = FALSE,
              ORIGIN_ITER = iter,
              ORIGIN_CONFIG_ID = id_str,
              DESTINY_ITER = iter,
              DESTINY_CONFIG_ID = id_str,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Save file with semicolon separator
  write.table(trajectories, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
  return(trajectories)
}

# nolint end

