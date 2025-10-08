# Import required libraries
library(tools)
library(irace)
library(utils)

#' Process irace results and generate STN-i file
#'
#' @param input_dir Directory containing .Rdata files
#' @param parameters_file CSV file with parameter definitions
#' @param output_dir Output directory for STN-i files
#' @param output_name Name of the output STN-i file
#' @param best_criteria Criteria for best value selection ('min' or 'max')
#' @param quality_criteria Criteria for configuration quality ('min', 'max', 'mean', 'median', 'mode')
#' @param significance Significance level for numerical parameters
#' @param instances_file Optional CSV file with instance optimal values
#' @export
process_and_generate_stn_i <- function(input_dir, parameters_file, output_dir, output_name,
                                     best_criteria = "min", quality_criteria = "mean",
                                     significance = 2, instances_file = NULL) {
  
  # Read parameters
  parameters <- read_parameters_file(parameters_file)
  
  # Process Rdata files and collect results
  results <- process_rdata_files(input_dir, parameters_file, instances_file)
  
  # Generate STN-i file
  generate_stn_i(results, parameters, output_dir, output_name,
                 best_criteria, quality_criteria, significance)
}

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
#' @return List with processed results
#' @export
process_rdata_files <- function(input_dir, parameters_file, results_dir, optimum_file = NULL) {
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
        results_file
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
  
  return(all_results)
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
create_results_file <- function(experiments, instances_df, output_file) {
  if (is.null(experiments) || ncol(experiments) == 0) {
    cat("  Advertencia: No hay datos de experimentos\n")
    results_df <- data.frame(
      CONFIG_ID = character(),
      INSTANCES = integer(),
      BQ = numeric(), BQR = integer(), BQRN = numeric(),
      MQ = numeric(), MQR = integer(), MQRN = numeric(),
      BG = numeric(), BGR = integer(), BGRN = numeric(),
      MG = numeric(), MGR = integer(), MGRN = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    tryCatch({
      # Get configuration IDs
      config_ids <- colnames(experiments)
      
      # Calculate basic quality metrics for each configuration
      quality_metrics <- lapply(config_ids, function(id) {
        qualities <- as.numeric(experiments[, id])
        qualities <- qualities[!is.na(qualities)]
        if (length(qualities) == 0) {
          c(
            CONFIG_ID = id,
            INSTANCES = 0,
            BQ = NA,
            MQ = NA
          )
        } else {
          c(
            CONFIG_ID = id,
            INSTANCES = length(qualities),
            BQ = min(qualities, na.rm = TRUE),
            MQ = mean(qualities, na.rm = TRUE)
          )
        }
      })
      results_df <- as.data.frame(do.call(rbind, quality_metrics))
      
      if (nrow(results_df) > 0) {
        # Calculate quality rankings
        results_df$BQR <- rank(results_df$BQ, ties.method = "min")
        results_df$MQR <- rank(results_df$MQ, ties.method = "min")
        
        # Normalize rankings (0-1)
        results_df$BQRN <- (results_df$BQR - 1) / (nrow(results_df) - 1)
        results_df$MQRN <- (results_df$MQR - 1) / (nrow(results_df) - 1)
        
        # Calculate GAPs if we have instance data
        if (!is.null(instances_df) && nrow(instances_df) > 0) {
          gaps_matrix <- matrix(NA, nrow = nrow(experiments), ncol = ncol(experiments))
          colnames(gaps_matrix) <- colnames(experiments)
          rownames(gaps_matrix) <- rownames(experiments)
          
          best_qualities <- apply(experiments, 1, min, na.rm = TRUE)
          
          for (i in seq_len(nrow(experiments))) {
            experiment_id <- as.integer(rownames(experiments)[i])
            opt_value <- instances_df$OPTIMUM[instances_df$EXPERIMENT_ID == experiment_id]
            best_quality <- if (!is.null(opt_value) && !is.na(opt_value)) opt_value else best_qualities[i]
            
            for (j in seq_len(ncol(experiments))) {
              value <- experiments[i, j]
              if (!is.na(value) && !is.na(best_quality) && best_quality != 0) {
                gaps_matrix[i, j] <- 100 * abs(value - best_quality) / best_quality
              }
            }
          }
          
          # Calculate GAP metrics
          gap_metrics <- lapply(config_ids, function(id) {
            gaps <- gaps_matrix[, id]
            gaps <- gaps[!is.na(gaps)]
            if (length(gaps) == 0) {
              c(BG = NA, MG = NA)
            } else {
              c(BG = min(gaps, na.rm = TRUE),
                MG = mean(gaps, na.rm = TRUE))
            }
          })
          gap_df <- as.data.frame(do.call(rbind, gap_metrics))
          
          # Add GAP metrics and calculate rankings
          results_df$BG <- gap_df$BG
          results_df$MG <- gap_df$MG
          results_df$BGR <- rank(results_df$BG, ties.method = "min")
          results_df$MGR <- rank(results_df$MG, ties.method = "min")
          results_df$BGRN <- (results_df$BGR - 1) / (nrow(results_df) - 1)
          results_df$MGRN <- (results_df$MGR - 1) / (nrow(results_df) - 1)
        } else {
          # Add empty GAP columns
          results_df$BG <- NA
          results_df$MG <- NA
          results_df$BGR <- NA
          results_df$MGR <- NA
          results_df$BGRN <- NA
          results_df$MGRN <- NA
        }
      }
      
    }, error = function(e) {
      cat("  Error procesando resultados:", conditionMessage(e), "\n")
      results_df <- data.frame(
        CONFIG_ID = character(),
        INSTANCES = integer(),
        BQ = numeric(), BQR = integer(), BQRN = numeric(),
        MQ = numeric(), MQR = integer(), MQRN = numeric(),
        BG = numeric(), BGR = integer(), BGRN = numeric(),
        MG = numeric(), MGR = integer(), MGRN = numeric(),
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