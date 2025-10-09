# Import required libraries
library(tools)
library(irace)
library(utils)

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
#' 
#' @return None
#' 
#' @export
process_rdata_files <- function(input_dir, parameters_file, results_dir, optimum_file = NULL, best_criteria = "min", is_na_ranking = FALSE) {
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
    cat("Procesando archivo:", basename(file), "\n")
    seed_dir <- file.path(results_dir, tools::file_path_sans_ext(basename(file)))
    dir.create(seed_dir, showWarnings = FALSE, recursive = TRUE)
    
    tryCatch({
      # Load irace results with suppressed output
      irace_results <- suppressMessages(suppressWarnings(read_logfile(file)))      # Create configurations file
      cat("  Creando archivo configurations.csv...\n")
      configs_file <- file.path(seed_dir, "configurations.csv")
      configs <- create_configurations_file(
        irace_results$allConfigurations,
        irace_results$allElites,
        length(irace_results$allElites) - 1,
        parameters,
        configs_file
      )
      
      # Create trajectories file
      cat("  Creando archivo trajectories.csv...\n")
      trajectories_file <- file.path(seed_dir, "trajectories.csv")
      trajectories <- create_trajectories_file(
        irace_results,
        irace_results$raceData,
        irace_results$allElites,
        length(irace_results$allElites) - 1,
        trajectories_file
      )
      
      # Create instances file
      cat("  Creando archivo instances.csv...\n")
      instances_file <- file.path(seed_dir, "instances.csv")
      instances_df <- create_instances_file(
        irace_results,
        irace_results$experiments,
        optimum_file,
        instances_file
      )
      
      # Create results file
      cat("  Creando archivo results.csv...\n")
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
#' @param optimum_data Data frame with optimum values for instances if exists
#' @param output_file Path to save the trajectories CSV file
#' 
#' @return Data frame with trajectories
#'
#' @export
create_instances_file <- function(iraceResults, experiments, optimum_data, output_file) {
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
      
      # Add optimal values if optimum_data is provided (and has the correct format and name file)
      if (!is.null(optimum_data)) {
        for (i in seq_len(nrow(instances_df))) {
          instance_name <- instances_df$NAME[i]
          opt_row <- optimum_data[optimum_data$INSTANCE == instance_name, ]
          instances_df$OPTIMUM[i] <- if (nrow(opt_row) > 0 && !is.na(opt_row$OPTIMUM)) opt_row$OPTIMUM else NA
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
#' @param quality_criteria Criteria for quality selection ('mean' or 'best')
#' @param representative_criteria Criteria for representative configuration selection ('min' or 'max')
#' @param significance Significance threshold for trajectory types
#'
#' @return None
#' 
#' @export
generate_stn_i <- function(input_dir, parameters_file, output_dir, output_name,
                          quality_criteria = "mean", representative_criteria = "min", significance = 2) {
  # Load the parameters file
  parameters <- read_parameters_file(parameters_file = parameters_file)

  # Define the type priority
  type_priority <- c("STANDARD", "START", "END")

  results_dirs <- list.dirs(path = input_dir, full.names = TRUE, recursive = FALSE)

  # Initialize counters and storage structures
  run_counter <- 1
  new_config_id <- 1
  configurations_list <- list()
  trajectories_list <- list()
  location_to_config_idx <- list()

  # Process each results folder (diferents runs by seeds)
  for (results_folder in results_dirs) {
    # Read the irace results files
    configurations_data <- read.csv(file.path(results_folder, "configurations.csv"), sep = ";")
    results_data <- read.csv(file.path(results_folder, "results.csv"), sep = ";")
    trajectories_data <- read.csv(file.path(results_folder, "trajectories.csv"), sep = ";")

    # Process the trajectories data on vectorized way 
    trajectories_batch <- data.frame(
      RUN_ID = rep(run_counter, nrow(trajectories_data)),
      PATH = trajectories_data$PATH,
      ITERATION_1 = trajectories_data$ORIGIN_ITER,
      CONFIG_ID_1 = trajectories_data$ORIGIN_CONFIG_ID,
      SOLUTION_1 = NA,
      QUALITY_1 = NA,
      ORIGIN_TYPE_1 = NA,
      TYPE_1 = NA,
      ORIGIN_IS_ELITE_1 = NA,
      IS_ELITE_1 = NA,
      ITERATION_2 = trajectories_data$DESTINY_ITER,
      CONFIG_ID_2 = trajectories_data$DESTINY_CONFIG_ID,
      SOLUTION_2 = NA,
      QUALITY_2 = NA,
      ORIGIN_TYPE_2 = NA,
      TYPE_2 = NA,
      ORIGIN_IS_ELITE_2 = NA,
      IS_ELITE_2 = NA,
      stringsAsFactors = FALSE
    )
    trajectories_list[[length(trajectories_list) + 1]] <- trajectories_batch

    for (i in seq_len(nrow(configurations_data))) {
      configuration <- configurations_data[i, ]
      results <- results_data[results_data$CONFIG_ID == configuration$CONFIG_ID, ]

      # Get only parameter columns
      param_cols <- setdiff(names(configuration), c("CONFIG_ID", "IS_ELITE"))
      param_values <- configuration[param_cols]
      location_code <- get_location_code(param_values, parameters)
      existing_config_idx <- location_to_config_idx[[location_code]]

      # Find configuration in configurations_list
      if (is.null(existing_config_idx)) {
        # Add new configuration
        config_entry <- list(
          CONFIG_ID = new_config_id,
          LOCATION_CODE = location_code,
          IS_ELITE = as.character(configuration$IS_ELITE),
          RUN_IDS = list(run_counter),
          OLD_CONFIG_IDS = list(configuration$CONFIG_ID),
          MNRG_VALUES = list(results$MNRG),
          TYPES = list(character(0))
        )
        # Add parameter values
        for (col_name in param_cols) {
          config_entry[[col_name]] <- configuration[[col_name]]
        }
        configurations_list[[length(configurations_list) + 1]] <- config_entry
        existing_config_idx <- length(configurations_list)
        location_to_config_idx[[location_code]] <- existing_config_idx
        new_config_id <- new_config_id + 1
      } else {
        # Update lists form existing configuration
        config_item <- configurations_list[[existing_config_idx]]
        config_item$RUN_IDS[[1]] <- c(config_item$RUN_IDS[[1]], run_counter)
        config_item$OLD_CONFIG_IDS[[1]] <- c(config_item$OLD_CONFIG_IDS[[1]], configuration$CONFIG_ID)
        config_item$MNRG_VALUES[[1]] <- c(config_item$MNRG_VALUES[[1]], results$MNRG)

        # Update IS_ELITE if necessary
        if (configuration$IS_ELITE == "TRUE" && config_item$IS_ELITE == "FALSE") {
          config_item$IS_ELITE <- "TRUE"
        }
        configurations_list[[existing_config_idx]] <- config_item
      }

      # Store information for later batch update
      current_batch_idx <- length(trajectories_list)
      if (current_batch_idx > 0) {
        # Update SOLUTION_1, SOLUTION_2 and ORIGIN_IS_ELITE_1, ORIGIN_IS_ELITE_2
        batch_data <- trajectories_list[[current_batch_idx]]
        config_id_matches_1 <- batch_data$CONFIG_ID_1 == configuration$CONFIG_ID
        config_id_matches_2 <- batch_data$CONFIG_ID_2 == configuration$CONFIG_ID
        
        batch_data$SOLUTION_1[config_id_matches_1] <- location_code
        batch_data$ORIGIN_IS_ELITE_1[config_id_matches_1] <- configuration$IS_ELITE
        batch_data$SOLUTION_2[config_id_matches_2] <- location_code
        batch_data$ORIGIN_IS_ELITE_2[config_id_matches_2] <- configuration$IS_ELITE
        
        trajectories_list[[current_batch_idx]] <- batch_data
      }

      # Search configuration in the current batch to determine trajectory types
      if (current_batch_idx > 0) {
        batch_data <- trajectories_list[[current_batch_idx]]
        total_iterations <- max(batch_data$ITERATION_2)
        trajectory_types <- c()
        # For CONFIG_ID_1
        idx_1 <- which(batch_data$CONFIG_ID_1 == configuration$CONFIG_ID)
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
            batch_data$ORIGIN_TYPE_1[idx] <- type
          }
        }
        # For CONFIG_ID_2
        idx_2 <- which(batch_data$CONFIG_ID_2 == configuration$CONFIG_ID)
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
            batch_data$ORIGIN_TYPE_2[idx] <- type
          }
        }
        # Update the batch data
        trajectories_list[[current_batch_idx]] <- batch_data
      }

      # Update TYPES
      if (length(trajectory_types) > 0) {
        config_item <- configurations_list[[existing_config_idx]]
        existing_types <- config_item$TYPES[[1]]
        if (length(existing_types) == 0) {
          config_item$TYPES[[1]] <- unique(trajectory_types)
        } else {
          config_item$TYPES[[1]] <- unique(c(existing_types, trajectory_types))
        }
        configurations_list[[existing_config_idx]] <- config_item
      }
    }

    run_counter <- run_counter + 1
  }

  cat("Convirtiendo listas a data.frames...\n")

  # Transform configurations_list in configurations_df
  if (length(configurations_list) > 0) {
    configurations_df <- do.call(rbind, lapply(configurations_list, function(x) {
      x$RUN_IDS <- I(x$RUN_IDS)
      x$OLD_CONFIG_IDS <- I(x$OLD_CONFIG_IDS)
      x$MNRG_VALUES <- I(x$MNRG_VALUES)
      x$TYPES <- I(x$TYPES)
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

  cat("Conversión completada. Configuraciones:", nrow(configurations_df), "Trayectorias:", nrow(trajectories_df), "\n")

  # Calculate qualities for each configuration and start grouping by locations
  locations_df <- data.frame()
  for (i in seq_len(nrow(configurations_df))) {
    config <- configurations_df[i, ]
    # Calculate final MNRG value based on quality_criteria
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

    # Search for existing location in locations_df
    if (nrow(locations_df) > 0 && config$LOCATION_CODE %in% locations_df$LOCATION_CODE) {
        # Search for existing location
        loc_idx <- which(locations_df$LOCATION_CODE == config$LOCATION_CODE)
        # Update IS_ELITE if necessary
        if (config$IS_ELITE == "TRUE" && locations_df$IS_ELITE[loc_idx] == "FALSE") {
          locations_df$IS_ELITE[loc_idx] <- "TRUE"
        }
        # Update QUALITIES list
        locations_df$QUALITIES[[loc_idx]] <- c(locations_df$QUALITIES[[loc_idx]], final_mnrg)
        # Update TYPE if necessary based on priority
        config_types <- config$TYPES[[1]]
        if (length(config_types) > 0) {
          # Get the best type from the configuration
          type_priorities <- match(config_types, type_priority)
          best_config_type <- config_types[which.min(type_priorities)]
          # Compare type priorities and update if necessary
          current_type <- locations_df$TYPE[loc_idx]
          current_priority <- match(current_type, type_priority)
          best_config_priority <- match(best_config_type, type_priority)
          if (best_config_priority < current_priority) {
            locations_df$TYPE[loc_idx] <- best_config_type
          }
        }
    } else {
      # Create new location entry and select best type
      config_types <- config$TYPES[[1]]
      if (length(config_types) > 0) {
        type_priorities <- match(config_types, type_priority)
        best_type <- config_types[which.min(type_priorities)]
      } else {
        best_type <- "STANDARD"
      }
      
      location_entry <- data.frame(
        LOCATION_CODE = config$LOCATION_CODE,
        IS_ELITE = config$IS_ELITE,
        TYPE = best_type,
        QUALITIES = I(list(final_mnrg)),
        FINAL_QUALITY = NA,
        stringsAsFactors = FALSE
      )
      locations_df <- rbind(locations_df, location_entry)
    }
  }

  # Calculate FINAL_QUALITY for each location based on QUALITIES and representative_criteria
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

    # Update trajectories_df with quality and type information
    loc_code <- locations_df$LOCATION_CODE[i]
    loc_quality <- locations_df$FINAL_QUALITY[i]
    loc_type <- locations_df$TYPE[i]
    loc_elite <- locations_df$IS_ELITE[i]
    trajectories_df$QUALITY_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_quality
    trajectories_df$TYPE_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_type
    trajectories_df$IS_ELITE_1[trajectories_df$SOLUTION_1 == loc_code] <- loc_elite
    trajectories_df$QUALITY_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_quality
    trajectories_df$TYPE_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_type
    trajectories_df$IS_ELITE_2[trajectories_df$SOLUTION_2 == loc_code] <- loc_elite
  }

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
