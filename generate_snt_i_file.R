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
#' @return A list with two elements:
#'   \describe{
#'     \item{`params`}{A `data.frame` containing the processed parameters.}
#'     \item{`domains`}{A list of parameter domains, with structured values and locations
#'       depending on the parameter type (`c`, `o`, `i`, `r`).}
#'   }
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

#' Generate location code from configuration
#'
#' This function generates a location code based on the configuration values
#' and the parameter domains.
#'
#' @param parameters_values A named vector containing the parameter values.
#' @param parameters A list containing the parameter domains and their locations.
#'
#' @return A string representing the location code.
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

#' Generate STN-i file from irace results
#' This function processes the results of multiple irace runs in a given scenario folder,
#' consolidating configurations and trajectories, and generates an STN-i file.
#'
#' @param scenary_folder `character(1)`\cr
#'  Path to the folder containing the results of multiple irace runs.
#' @param parameters_file `character(1)`\cr
#' Path to the parameters CSV file (semicolon-separated).
#' @param output_folder `character(1)`\cr
#' Path to the folder where the STN-i file will be saved.
#' @param output_file_name `character(1)`\cr
#' Name of the output STN-i file.
#' @param criteria `character(1)`\cr
#' Criteria for selecting configuration values: "min", "max", "mean", "median
#' or "mode". Default is "min".
#' @param significance `numeric(1)`\cr
#' Significance level for numerical parameters. Default is 2.
#' @return None. The function saves the STN-i file in the specified output folder.
#'
generate_stn_i_file <- function (scenary_folder, parameters_file, output_folder, output_file_name,
  criteria = "mean", best = 'min') {

  # Load the parameters file
  parameters <- read_parameters_file(parameters_file = parameters_file)
  # Define the type priority
  type_priority <- c("STANDARD", "START", "END")

  results_folders <- list.dirs(path = scenary_folder, full.names = TRUE, recursive = FALSE)

  run_counter <- 1
  new_config_id <- 1
  configurations_list <- list()
  trajectories_list <- list()
  
  # Para optimizar búsqueda de configuraciones duplicadas
  location_to_config_idx <- list()

  # Process each results folder (diferents runs by seeds)
  for (results_folder in results_folders) {
    # Read the irace results files
    configurations_data <- read.csv(file.path(results_folder, "configurations.csv"), sep = ";")
    results_data <- read.csv(file.path(results_folder, "results.csv"), sep = ";")
    trajectories_data <- read.csv(file.path(results_folder, "trajectories.csv"), sep = ";")

    # Procesar trajectories de manera vectorizada
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

      # Obtener solo los parámetros (excluyendo CONFIG_ID e IS_ELITE)
      param_cols <- setdiff(names(configuration), c("CONFIG_ID", "IS_ELITE"))
      param_values <- configuration[param_cols]
      location_code <- get_location_code(param_values, parameters)

      # Buscar configuración existente usando location_code (más eficiente)
      existing_config_idx <- location_to_config_idx[[location_code]]

      # Revisar si la configuración ya existe
      if (is.null(existing_config_idx)) {
        # Nueva configuración - crear entrada
        config_entry <- list(
          CONFIG_ID = new_config_id,
          LOCATION_CODE = location_code,
          IS_ELITE = as.character(configuration$IS_ELITE),
          RUN_IDS = list(run_counter),
          OLD_CONFIG_IDS = list(configuration$CONFIG_ID),
          MGRN_VALUES = list(results$MGRN),
          TYPES = list(character(0))
        )
        # Agregar los parámetros
        for (col_name in param_cols) {
          config_entry[[col_name]] <- configuration[[col_name]]
        }
        configurations_list[[length(configurations_list) + 1]] <- config_entry
        existing_config_idx <- length(configurations_list)
        location_to_config_idx[[location_code]] <- existing_config_idx
        new_config_id <- new_config_id + 1
      } else {
        # Configuración existente - actualizar listas
        config_item <- configurations_list[[existing_config_idx]]
        config_item$RUN_IDS[[1]] <- c(config_item$RUN_IDS[[1]], run_counter)
        config_item$OLD_CONFIG_IDS[[1]] <- c(config_item$OLD_CONFIG_IDS[[1]], configuration$CONFIG_ID)
        config_item$MGRN_VALUES[[1]] <- c(config_item$MGRN_VALUES[[1]], results$MGRN)

        # Actualizar IS_ELITE: si la configuración actual es TRUE, prevalece sobre FALSE
        if (configuration$IS_ELITE == "TRUE" && config_item$IS_ELITE == "FALSE") {
          config_item$IS_ELITE <- "TRUE"
        }
        configurations_list[[existing_config_idx]] <- config_item
      }

      # Almacenar información para actualización batch posterior
      current_batch_idx <- length(trajectories_list)
      if (current_batch_idx > 0) {
        # Actualizar SOLUTION y ORIGIN_IS_ELITE para esta configuración
        batch_data <- trajectories_list[[current_batch_idx]]
        config_id_matches_1 <- batch_data$CONFIG_ID_1 == configuration$CONFIG_ID
        config_id_matches_2 <- batch_data$CONFIG_ID_2 == configuration$CONFIG_ID
        
        batch_data$SOLUTION_1[config_id_matches_1] <- location_code
        batch_data$ORIGIN_IS_ELITE_1[config_id_matches_1] <- configuration$IS_ELITE
        batch_data$SOLUTION_2[config_id_matches_2] <- location_code
        batch_data$ORIGIN_IS_ELITE_2[config_id_matches_2] <- configuration$IS_ELITE
        
        trajectories_list[[current_batch_idx]] <- batch_data
      }

      # Calcular tipos de manera más eficiente
      if (current_batch_idx > 0) {
        batch_data <- trajectories_list[[current_batch_idx]]
        total_iterations <- max(batch_data$ITERATION_2)
        
        # Encontrar tipos para esta configuración
        trajectory_types <- c()
        
        # Para CONFIG_ID_1
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
        
        # Para CONFIG_ID_2
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
        
        trajectories_list[[current_batch_idx]] <- batch_data
      }

      # Actualizar TYPES en configurations de manera más eficiente
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

  # Convertir listas a data.frames de manera eficiente
  cat("Convirtiendo listas a data.frames...\n")
  
  # Convertir configurations_list a configurations_df
  if (length(configurations_list) > 0) {
    configurations_df <- do.call(rbind, lapply(configurations_list, function(x) {
      # Convertir listas internas a I() para mantener estructura
      x$RUN_IDS <- I(x$RUN_IDS)
      x$OLD_CONFIG_IDS <- I(x$OLD_CONFIG_IDS)
      x$MGRN_VALUES <- I(x$MGRN_VALUES)
      x$TYPES <- I(x$TYPES)
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  } else {
    configurations_df <- data.frame()
  }
  
  # Convertir trajectories_list a trajectories_df
  if (length(trajectories_list) > 0) {
    trajectories_df <- do.call(rbind, trajectories_list)
  } else {
    trajectories_df <- data.frame()
  }
  
  cat("Conversión completada. Configuraciones:", nrow(configurations_df), "Trayectorias:", nrow(trajectories_df), "\n")

  # Calcular cada calidad de cada configuración de configuraciones, usando como criterio el criteria
  # e iniciar agrupación por locaciones particulares
  locations_df <- data.frame()
  for (i in seq_len(nrow(configurations_df))) {
    config <- configurations_df[i, ]
    
    # Calcular FINAL_MGRN para esta configuración
    mgrn_values <- as.numeric(config$MGRN_VALUES[[1]])
    mgrn_values <- mgrn_values[!is.na(mgrn_values)]
    if (length(mgrn_values) == 0) {
      final_mgrn <- NA
    } else if (criteria == "min") {
      final_mgrn <- min(mgrn_values)
    } else if (criteria == "max") {
      final_mgrn <- max(mgrn_values)
    } else if (criteria == "mean") {
      final_mgrn <- mean(mgrn_values)
    } else if (criteria == "median") {
      final_mgrn <- median(mgrn_values)
    } else if (criteria == "mode") {
      mode_value <- as.numeric(names(sort(table(mgrn_values), decreasing = TRUE)[1]))
      final_mgrn <- mode_value
    } else {
      stop("Error: criteria debe ser 'min', 'max', 'mean', 'median' o 'mode'", call. = FALSE)
    }
    
    # Procesar agrupación por locaciones
    if (nrow(locations_df) > 0 && config$LOCATION_CODE %in% locations_df$LOCATION_CODE) {
        # Busca la locación existente
        loc_idx <- which(locations_df$LOCATION_CODE == config$LOCATION_CODE)
        # Actualizar IS_ELITE si es necesario
        if (config$IS_ELITE == "TRUE" && locations_df$IS_ELITE[loc_idx] == "FALSE") {
          locations_df$IS_ELITE[loc_idx] <- "TRUE"
        }
        # Actualizar QUALITIES
        locations_df$QUALITIES[[loc_idx]] <- c(locations_df$QUALITIES[[loc_idx]], final_mgrn)
        # Actualizar TYPE si el nuevo tipo tiene mayor prioridad
        config_types <- config$TYPES[[1]]
        if (length(config_types) > 0) {
          # Seleccionar el tipo de mayor prioridad de la configuración
          type_priorities <- match(config_types, type_priority)
          best_config_type <- config_types[which.min(type_priorities)]
          
          # Comparar con el tipo actual de la locación
          current_type <- locations_df$TYPE[loc_idx]
          current_priority <- match(current_type, type_priority)
          best_config_priority <- match(best_config_type, type_priority)
          
          if (best_config_priority < current_priority) {
            locations_df$TYPE[loc_idx] <- best_config_type
          }
        }
    } else {
      # Crear entrada de locación
      # Seleccionar el TYPE de mayor prioridad de la configuración
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
        QUALITIES = I(list(final_mgrn)),
        FINAL_QUALITY = NA,
        stringsAsFactors = FALSE
      )
      locations_df <- rbind(locations_df, location_entry)
    }
  }

  # Agregar los datos finales de calidad a locations_df y los de trajectories_df
  for (i in seq_len(nrow(locations_df))) {
    qualities <- as.numeric(locations_df$QUALITIES[[i]])
    qualities <- qualities[!is.na(qualities)]
    if (length(qualities) == 0) {
      locations_df$FINAL_QUALITY[i] <- NA
    } else if (criteria == "min") {
      locations_df$FINAL_QUALITY[i] <- min(qualities)
    } else if (criteria == "max") {
      locations_df$FINAL_QUALITY[i] <- max(qualities)
    } else if (criteria == "mean") {
      locations_df$FINAL_QUALITY[i] <- mean(qualities)
    } else if (criteria == "median") {
      locations_df$FINAL_QUALITY[i] <- median(qualities)
    } else if (criteria == "mode") {
      mode_value <- as.numeric(names(sort(table(qualities), decreasing = TRUE)[1]))
      locations_df$FINAL_QUALITY[i] <- mode_value
    }

    # Actualizar trajectories_df con QUALITY, TYPE, IS_ELITE finales
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

  # Escribir el archivo de la trayectoria final considerando todos los datos calculados y procesados de trajectories_df con el nombre de salida indicado
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  output_file_path <- file.path(output_folder, output_file_name)
  write.table(trajectories_df, file = output_file_path, sep = ";", row.names = FALSE, quote = FALSE)
}

escenary_folder <- "Experiments/ACOTSP-QGR/BL-45/Results"
output_folder <- "Experiments/ACOTSP-QGR/BL-45/STN-i-Files"
# parameters_file <- "Experiments/ACOTSP-QGR/Parameters/L1.csv"
# output_file_name <- "BL-45-L1.stn-i"
# parameters_file <- "Experiments/ACOTSP-QGR/Parameters/L2.csv"
# output_file_name <- "BL-45-L2.stn-i"
# parameters_file <- "Experiments/ACOTSP-QGR/Parameters/L3.csv"
# output_file_name <- "BL-45-L3.stn-i"
parameters_file <- "Experiments/ACOTSP-QGR/Parameters/L4.csv"
output_file_name <- "BL-45-L4.stn-i"
# parameters_file <- "Experiments/ACOTSP-QGR/Parameters/L5.csv"
# output_file_name <- "BL-45-L5.stn-i"

generate_stn_i_file(escenary_folder, parameters_file, output_folder, output_file_name,
  criteria = "min", best = 'min')
