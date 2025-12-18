# nolint start

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
  
  # Check if we're processing all iterations
  if (length(allElites) < iterations) {
    warning(sprintf("allElites has only %d elements but iterations=%d was requested", 
                    length(allElites), iterations))
    iterations <- length(allElites)
  }
  
  # Convert all IDs to integers for consistent comparison
  is_elite <- sapply(config_ids, function(id) {
    id_int <- as.integer(id)
    any(sapply(allElites[1:iterations], function(elite_ids) {
      id_int %in% as.integer(elite_ids)
    }))
  })
  
  # Identify the best configuration (first elite from last iteration)
  best_config_id <- if (length(allElites) >= iterations && length(allElites[[iterations]]) > 0) {
    as.integer(allElites[[iterations]][1])
  } else {
    NA_integer_
  }
  
  is_best <- sapply(config_ids, function(id) {
    !is.na(best_config_id) && as.integer(id) == best_config_id
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
      IS_BEST = as.character(is_best[i]),
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

  # Process each Rdata file
  for (file in files) {
    if (verbose) cat("Procesando archivo:", basename(file), "\n")
    seed_dir <- file.path(results_dir, tools::file_path_sans_ext(basename(file)))
    dir.create(seed_dir, showWarnings = FALSE, recursive = TRUE)

    tryCatch({
      # Load irace results with suppressed output
      irace_results <- suppressMessages(suppressWarnings(read_logfile(file)))
      if (verbose) cat("  Creando archivo configurations.csv...\n")
      configs_file <- file.path(seed_dir, "configurations.csv")
      configs <- create_configurations_file(
        allConfigurations = irace_results$allConfigurations,
        allElites = irace_results$allElites,
        iterations = length(irace_results$allElites),
        parameters = parameters,
        output_file = configs_file
      )

      # Create trajectories file
      if (verbose) cat("  Creando archivo trajectories.csv...\n")
      trajectories_file <- file.path(seed_dir, "trajectories.csv")
      trajectories <- create_trajectories_file(
        iraceResults = irace_results,
        raceData = irace_results$raceData,
        allElites = irace_results$allElites,
        iterations = length(irace_results$allElites),
        output_file = trajectories_file
      )

      # Create instances file
      if (verbose) cat("  Creando archivo instances.csv...\n")
      instances_file <- file.path(seed_dir, "instances.csv")
      instances_df <- create_instances_file(
        iraceResults = irace_results,
        experiments = irace_results$experiments,
        optimum_file = optimum_file,
        output_file = instances_file
      )

      # Create results file
      if (verbose) cat("  Creando archivo results.csv...\n")
      results_file <- file.path(seed_dir, "results.csv")
      results <- create_results_file(
        experiments = irace_results$experiments,
        instances_df = instances_df,
        output_file = results_file,
        best_criteria = best_criteria,
        is_na_ranking = is_na_ranking
      )
    }, error = function(e) {
      cat("Error procesando archivo", basename(file), ":", conditionMessage(e), "\n")
    })
  }
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
        # --- Scaled integer arithmetic for numeric stability ---
        scale          <- 10^significance
        scaled_lower   <- as.integer(round(lower_bound * scale))
        scaled_upper   <- as.integer(round(upper_bound * scale))
        scaled_step    <- as.integer(round(step * scale))
        scaled_value   <- as.integer(round(as.numeric(param_value) * scale))
        
        if (scaled_step <= 0) {
          stop(paste0("Error: Invalid step size for parameter ", param_name))
        }
        if (scaled_upper < scaled_lower) {
          stop(paste0("Error: Upper bound < lower bound for parameter ", param_name))
        }
        
        # Maximum valid index (integer division)
        max_index <- (scaled_upper - scaled_lower) %/% scaled_step
        
        # Subrange index, clamped to [0, max_index]
        subrange_index <- (scaled_value - scaled_lower) %/% scaled_step
        subrange_index <- pmin(pmax(subrange_index, 0L), as.integer(max_index))
        
        # Start of the corresponding subrange
        calculated_scaled <- scaled_lower + subrange_index * scaled_step
        
        # --- Prepare code formatting ---
        current_digits   <- nchar(as.character(calculated_scaled))
        max_upper_digits <- nchar(as.character(scaled_upper))
        difference       <- max_upper_digits - current_digits
        
        if (difference < 0) {
          stop(paste0("Error: Negative digit difference for parameter ", param_name))
        }
        
        code_part <- paste0(strrep("0", difference), calculated_scaled)
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

          # Update ELITE or REGULAR count
          if (is_elite) {
            config_item$ELITE_COUNT <- config_item$ELITE_COUNT + 1
          } else {
            config_item$REGULAR_COUNT <- config_item$REGULAR_COUNT + 1
          }

          # Update topology counts
          for (type in unique_types) {
            if (type == "START") {
              config_item$START_COUNT <- config_item$START_COUNT + 1
              if (is_elite) {
                config_item$ELITE_START_COUNT <- config_item$ELITE_START_COUNT + 1
              } else {
                config_item$REGULAR_START_COUNT <- config_item$REGULAR_START_COUNT + 1
              }
            } else if (type == "END") {
              config_item$END_COUNT <- config_item$END_COUNT + 1
              if (is_elite) {
                config_item$ELITE_END_COUNT <- config_item$ELITE_END_COUNT + 1
              } else {
                config_item$REGULAR_END_COUNT <- config_item$REGULAR_END_COUNT + 1
              }
            } else if (type == "STANDARD") {
              config_item$STANDARD_COUNT <- config_item$STANDARD_COUNT + 1
              if (is_elite) {
                config_item$ELITE_STANDARD_COUNT <- config_item$ELITE_STANDARD_COUNT + 1
              } else {
                config_item$REGULAR_STANDARD_COUNT <- config_item$REGULAR_STANDARD_COUNT + 1
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

  # Write output files
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file_path <- file.path(output_dir, output_name)
  write.table(trajectories_df, file = output_file_path, sep = ";", row.names = FALSE, quote = FALSE)
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

# nolint end
