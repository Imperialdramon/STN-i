# Import required libraries
library(tools)
library(irace)
library(utils)

#' Read and process parameters file
#'
#' @param parameters_file Path to CSV parameter definition file
#' @return List with processed parameters and domains
#' @export
read_parameters_file <- function(parameters_file) {
  params <- read.csv2(parameters_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Clean parameter names and types
  params$NAME <- trimws(params$NAME)
  params$TYPE <- trimws(params$TYPE)
  
  # Process domains and locations
  domains <- process_parameter_domains(params)
  
  list(
    params = params,
    domains = domains
  )
}

#' Process parameter domains and locations
#'
#' @param params Data frame with parameter definitions
#' @return List of parameter domains
#' @keywords internal
process_parameter_domains <- function(params) {
  domains <- list()
  
  for (i in seq_len(nrow(params))) {
    param <- params[i, ]
    domains[[param$NAME]] <- list(
      type = param$TYPE,
      values = clean_values_array(param$VALUES_ARRAY),
      locations = clean_locations_array(param$LOCATIONS_ARRAY)
    )
  }
  
  domains
}

#' Process Rdata files and create Results structure
#'
#' @param input_dir Directory containing .Rdata files
#' @param parameters_file Path to parameters CSV file
#' @param results_dir Directory for Results files
#' @param optimum_file Optional path to instance optimum values file
#' @param best_criteria Criteria for best value selection ('min' or 'max')
#' @param is_na_ranking Whether to consider NA values in rankings
#' @return List with processed results
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
#' @keywords internal
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

#' Create trajectories file
#' @keywords internal
# Create instances file
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

# Create results file
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

# Create trajectories file
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

#' Generate STN-i file from processed results
#'
#' @param input_dir Directory with processed Results
#' @param parameters Parameter definitions
#' @param output_dir Output directory
#' @param output_name Output filename
#' @param best_criteria Criteria for best value
#' @param quality_criteria Criteria for quality
#' @param significance Significance level
#' @export
generate_stn_i <- function(input_dir, parameters, output_dir, output_name,
                          best_criteria, quality_criteria, significance) {
  
  # List all result directories
  result_dirs <- list.dirs(input_dir, full.names = TRUE, recursive = FALSE)
  
  # Initialize lists for collecting data
  all_configs <- list()
  all_experiments <- list()
  all_trajectories <- list()
  
  # Process each result directory
  for (dir in result_dirs) {
    # Read result files
    configs <- read.csv(file.path(dir, "configurations.csv"), sep = ";", stringsAsFactors = FALSE)
    experiments <- read.csv(file.path(dir, "results.csv"), sep = ";", stringsAsFactors = FALSE)
    trajectories <- read.csv(file.path(dir, "trajectories.csv"), sep = ";", stringsAsFactors = FALSE)
    
    # Add run identifier
    run_id <- basename(dir)
    configs$RUN <- run_id
    trajectories$RUN <- run_id
    
    # Store in lists
    all_configs[[run_id]] <- configs
    all_experiments[[run_id]] <- experiments
    all_trajectories[[run_id]] <- trajectories
  }
  
  # Combine results
  combined_results <- list(
    configurations = do.call(rbind, all_configs),
    experiments = all_experiments[[1]], # Take first one as reference
    trajectories = do.call(rbind, all_trajectories)
  )
  
  # Process configurations and qualities
  processed_results <- process_qualities(combined_results, 
                                      best_criteria,
                                      quality_criteria)
  
  # Generate location codes
  location_results <- generate_location_codes(processed_results,
                                           parameters,
                                           significance)
  
  # Write STN-i file
  write_stn_i_file(location_results, output_dir, output_name)
}

# Auxiliary functions for data processing

#' Clean values array from parameter definition
#' @keywords internal
clean_values_array <- function(values) {
  values <- gsub("[()]", "", values)
  trimws(values)
}

#' Clean locations array from parameter definition
#' @keywords internal
clean_locations_array <- function(locations) {
  locations <- gsub("[()]", "", locations)
  trimws(locations)
}

#' Process configurations from irace results
#' @keywords internal
process_configurations <- function(configurations, elites) {
  config_ids <- configurations$.ID.
  is_elite <- sapply(config_ids, function(id) {
    any(sapply(elites, function(elite_ids) id %in% elite_ids))
  })
  
  # Extraer columnas de parámetros
  param_cols <- setdiff(colnames(configurations), 
                       c(".ID.", ".PARENT.", ".ITERATION.", ".ELITE."))
  
  # Crear lista para almacenar configuraciones
  config_list <- list()
  
  # Procesar cada configuración
  for (i in seq_along(config_ids)) {
    config <- configurations[configurations$.ID. == config_ids[i], ]
    
    # Extraer valores de parámetros
    param_values <- sapply(param_cols, function(param) {
      value <- config[[param]]
      if (is.factor(value)) {
        as.character(value)
      } else if (is.integer(value)) {
        as.character(as.integer(value))
      } else {
        as.character(value)
      }
    })
    
    # Crear entrada de configuración
    config_entry <- c(
      CONFIG_ID = as.character(config_ids[i]),
      IS_ELITE = as.character(is_elite[i]),
      param_values
    )
    
    config_list[[i]] <- config_entry
  }
  
  # Convertir lista a data frame
  config_df <- as.data.frame(do.call(rbind, config_list), 
                            stringsAsFactors = FALSE)
  
  return(config_df)
}

#' Process experiments and results
#' @keywords internal
process_experiments <- function(experiments, optimum_file) {
  # Obtener IDs de configuraciones
  config_ids <- colnames(experiments)
  
  # Calcular métricas de calidad básicas para cada configuración
  quality_metrics <- lapply(config_ids, function(id) {
    qualities <- as.numeric(experiments[, id])
    qualities <- qualities[!is.na(qualities)]
    c(
      CONFIG_ID = id,
      INSTANCES = length(qualities),
      BQ = min(qualities, na.rm = TRUE),
      MQ = mean(qualities, na.rm = TRUE)
    )
  })
  results_df <- as.data.frame(do.call(rbind, quality_metrics))
  
  # Calcular rankings de calidad
  results_df$BQR <- rank(results_df$BQ, ties.method = "min")
  results_df$MQR <- rank(results_df$MQ, ties.method = "min")
  
  # Normalizar rankings (0-1)
  results_df$BQRN <- (results_df$BQR - 1) / (nrow(results_df) - 1)
  results_df$MQRN <- (results_df$MQR - 1) / (nrow(results_df) - 1)
  
  # Procesar GAPs si hay archivo de óptimos
  if (!is.null(optimum_file) && file.exists(optimum_file)) {
    optimum_data <- read.csv(optimum_file, header = TRUE, stringsAsFactors = FALSE)
    
    # Crear matriz de GAPs
    gaps_matrix <- matrix(NA, nrow = nrow(experiments), ncol = ncol(experiments))
    colnames(gaps_matrix) <- colnames(experiments)
    rownames(gaps_matrix) <- rownames(experiments)
    
    # Calcular mejores calidades
    best_qualities <- apply(experiments, 1, min, na.rm = TRUE)
    
    # Calcular GAPs
    for (i in seq_len(nrow(experiments))) {
      experiment_id <- as.integer(rownames(experiments)[i])
      opt_value <- optimum_data$OPTIMUM[optimum_data$INSTANCE == experiment_id]
      
      if (!is.null(opt_value) && !is.na(opt_value)) {
        best_quality <- opt_value
      } else {
        best_quality <- best_qualities[i]
      }
      
      for (j in seq_len(ncol(experiments))) {
        if (!is.na(experiments[i,j])) {
          gaps_matrix[i,j] <- (experiments[i,j] - best_quality) / best_quality
        }
      }
    }
    
    # Calcular métricas de GAP
    gap_metrics <- lapply(config_ids, function(id) {
      gaps <- gaps_matrix[, id]
      gaps <- gaps[!is.na(gaps)]
      c(
        BG = min(gaps),
        MG = mean(gaps)
      )
    })
    gap_df <- as.data.frame(do.call(rbind, gap_metrics))
    
    # Añadir métricas de GAP
    results_df$BG <- gap_df$BG
    results_df$MG <- gap_df$MG
    
    # Calcular rankings de GAP
    results_df$BGR <- rank(results_df$BG, ties.method = "min")
    results_df$MGR <- rank(results_df$MG, ties.method = "min")
    
    # Normalizar rankings de GAP
    results_df$BGRN <- (results_df$BGR - 1) / (nrow(results_df) - 1)
    results_df$MGRN <- (results_df$MGR - 1) / (nrow(results_df) - 1)
  }
  
  return(results_df)
}

#' Process trajectories from irace results
#' @keywords internal
process_trajectories <- function(irace_results) {
  trajectories <- data.frame(
    PATH = logical(),
    ORIGIN_ITER = integer(),
    ORIGIN_CONFIG_ID = character(),
    DESTINY_ITER = integer(),
    DESTINY_CONFIG_ID = character(),
    stringsAsFactors = FALSE
  )
  
  # Procesar cada iteración
  for (iter in seq_along(irace_results$allElites)) {
    configs <- irace_results$allConfigurations[irace_results$allConfigurations$.ITERATION. == iter, ]
    
    # Procesar cada configuración
    for (i in seq_len(nrow(configs))) {
      config <- configs[i, ]
      
      # Primera iteración
      if (iter == 1) {
        trajectories <- rbind(trajectories, data.frame(
          PATH = TRUE,
          ORIGIN_ITER = 0,
          ORIGIN_CONFIG_ID = "0",
          DESTINY_ITER = iter,
          DESTINY_CONFIG_ID = as.character(config$.ID.),
          stringsAsFactors = FALSE
        ))
      } else {
        # Conectar con el padre
        if (!is.na(config$.PARENT.)) {
          trajectories <- rbind(trajectories, data.frame(
            PATH = TRUE,
            ORIGIN_ITER = iter - 1,
            ORIGIN_CONFIG_ID = as.character(config$.PARENT.),
            DESTINY_ITER = iter,
            DESTINY_CONFIG_ID = as.character(config$.ID.),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(trajectories)
}

#' Combine results from multiple runs
#' @keywords internal
combine_run_results <- function(results) {
  # Inicializar listas para almacenar datos combinados
  all_configs <- list()
  all_experiments <- list()
  all_trajectories <- list()
  
  # Combinar resultados de todas las ejecuciones
  for (run_name in names(results)) {
    run_data <- results[[run_name]]
    
    # Verificar y procesar configuraciones si existen
    if (!is.null(run_data$configurations) && nrow(run_data$configurations) > 0) {
      configs <- run_data$configurations
      configs$RUN <- run_name
      all_configs[[run_name]] <- configs
    }
    
    # Verificar y procesar experimentos si existen
    if (!is.null(run_data$experiments)) {
      # Convertir experimentos a data.frame si no lo es
      if (is.matrix(run_data$experiments)) {
        run_data$experiments <- as.data.frame(run_data$experiments)
      }
      if (is.data.frame(run_data$experiments)) {
        all_experiments[[run_name]] <- run_data$experiments
      }
    }
    
    # Verificar y procesar trayectorias si existen
    if (!is.null(run_data$trajectories) && nrow(run_data$trajectories) > 0) {
      trajectories <- run_data$trajectories
      trajectories$RUN <- run_name
      all_trajectories[[run_name]] <- trajectories
    }
  }
  
  # Combinar configuraciones
  combined_configs <- if (length(all_configs) > 0) {
    do.call(rbind, all_configs)
  } else {
    data.frame()
  }
  
  # Combinar experimentos (mantener como data.frame)
  combined_experiments <- if (length(all_experiments) > 0) {
    all_experiments[[1]]  # Tomar el primer conjunto como base
  } else {
    data.frame()
  }
  
  # Combinar trayectorias
  combined_trajectories <- if (length(all_trajectories) > 0) {
    do.call(rbind, all_trajectories)
  } else {
    data.frame()
  }
  
  combined <- list(
    configurations = combined_configs,
    experiments = combined_experiments,
    trajectories = combined_trajectories
  )
  
  return(combined)
}

#' Process configuration qualities
#' @keywords internal
process_qualities <- function(results, best_criteria, quality_criteria) {
  # Verificar que tenemos datos
  if (is.null(results$configurations) || nrow(results$configurations) == 0) {
    return(data.frame())
  }

  # Procesar cada configuración
  configs <- results$configurations
  configs$FINAL_QUALITY <- NA
  
  # Verificar que experiments es un data.frame
  if (is.null(results$experiments) || !is.data.frame(results$experiments)) {
    return(configs)
  }
  
  for (i in seq_len(nrow(configs))) {
    config <- configs[i, ]
    
    # Obtener valores de calidad
    if (config$CONFIG_ID %in% colnames(results$experiments)) {
      quality_values <- results$experiments[[config$CONFIG_ID]]
      quality_values <- as.numeric(quality_values[!is.na(quality_values)])
      
      # Calcular calidad final según el criterio
      if (length(quality_values) == 0) {
        final_quality <- NA
      } else if (quality_criteria == "min") {
        final_quality <- min(quality_values, na.rm = TRUE)
      } else if (quality_criteria == "max") {
        final_quality <- max(quality_values, na.rm = TRUE)
      } else if (quality_criteria == "mean") {
        final_quality <- mean(quality_values, na.rm = TRUE)
      } else if (quality_criteria == "median") {
        final_quality <- median(quality_values, na.rm = TRUE)
      } else if (quality_criteria == "mode") {
        tab <- table(quality_values)
        if (length(tab) > 0) {
          final_quality <- as.numeric(names(sort(tab, decreasing = TRUE)[1]))
        } else {
          final_quality <- NA
        }
      } else {
        final_quality <- NA
      }
      
      # Añadir calidad final
      configs$FINAL_QUALITY[i] <- final_quality
    }
  }
  
  # Remover filas con NA si es necesario
  configs <- configs[!is.na(configs$FINAL_QUALITY), ]
  
  # Ordenar según el criterio de mejor valor si hay datos
  if (nrow(configs) > 0) {
    if (best_criteria == "min") {
      configs <- configs[order(configs$FINAL_QUALITY), ]
    } else {
      configs <- configs[order(-configs$FINAL_QUALITY), ]
    }
  }
  
  return(configs)
}

#' Generate location codes
#' @keywords internal
generate_location_codes <- function(results, parameters, significance) {
  processed_configs <- results
  
  # Generar códigos de localización para cada configuración
  for (i in seq_len(nrow(processed_configs))) {
    config <- processed_configs[i, ]
    
    # Obtener valores de parámetros
    param_values <- as.list(config[parameters$params$NAME])
    
    # Generar código de localización
    location_code <- get_location_code(param_values, parameters, significance)
    
    # Añadir código de localización
    processed_configs$LOCATION_CODE[i] <- location_code
  }
  
  return(processed_configs)
}

#' Write STN-i file
#' @keywords internal
write_stn_i_file <- function(results, output_dir, output_name) {
  # Asegurar que el directorio existe
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Preparar data frame final
  stn_i_df <- results$trajectories
  
  # Añadir información de calidad y tipo
  stn_i_df$QUALITY <- results$configurations$FINAL_QUALITY[
    match(stn_i_df$DESTINY_CONFIG_ID, results$configurations$CONFIG_ID)
  ]
  
  # Determinar tipo de nodo
  stn_i_df$TYPE <- "STANDARD"
  stn_i_df$TYPE[stn_i_df$ORIGIN_ITER == 0] <- "START"
  stn_i_df$TYPE[stn_i_df$DESTINY_CONFIG_ID %in% 
                  results$configurations$CONFIG_ID[results$configurations$IS_ELITE == "TRUE"]] <- "END"
  
  # Guardar archivo
  output_path <- file.path(output_dir, output_name)
  write.table(stn_i_df, 
              file = output_path, 
              sep = ";",
              row.names = FALSE,
              quote = FALSE)
}