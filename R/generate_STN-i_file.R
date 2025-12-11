# nolint start

#########################################################################
# STN-i File Generation Script
# Author: Pablo Estobar
#
# Description:
# This script processes .Rdata files and parameters to generate STN-i files.
# It allows processing of multiple parameter files, handles general and
# location-specific parameters, and supports different criteria for evaluating
# configuration quality and representativeness.
#
# Usage:
# Rscript generate_STN-i_file.R --input=<input_dir> --general_parameters=<general_params_file> /
#                               --parameters=<params_file_or_dir> --output=<output_dir> /
#                               [--name=<output_name>] [--best_criteria=<min|max>] /
#                               [--quality_criteria=<min|max|mean|median|mode>] /
#                               [--significance=<integer>] [--instances=<instances_file>] /
#                               [--na_ranking=<TRUE|FALSE>] /
#                               [--representative_criteria=<min|max|mean|median|mode>] /
#                               [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input                   : (Required) Directory containing input .Rdata files
# --general_parameters      : (Required) CSV file with general parameters for data processing
# --parameters              : (Required) Directory or CSV file with location parameters
# --output                  : (Required) Output directory for STN-i files
# --name                    : (Optional) Name of the output STN-i file
# --best_criteria           : (Optional) Criterion for best value configurations ('min' or 'max', default: 'min')
# --quality_criteria        : (Optional) Quality criterion ('min','max','mean','median','mode', default: 'mean')
# --significance            : (Optional) Significance level for numerical parameters (default: 2)
# --instances               : (Optional) CSV file with instance optimum values
# --na_ranking              : (Optional) Consider NA as worst possible value in rankings (default: FALSE)
# --representative_criteria : (Optional) Criterion for representative configuration (default: 'mean')
# --verbose                 : (Optional) Show detailed processing information (default: FALSE)
#
# Requirements:
# - R with the following packages installed:
#     - tools
#     - irace
#     - utils
#     - optparse
#
# Notes:
# - Input files must be valid .Rdata files
# - Parameter files must be in CSV format
# - The script will create output directories if they don't exist
# - Designed for command-line execution with named arguments
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("tools", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("irace", quietly = TRUE)) {
  stop("Error: The irace package is not installed. Please install it with 'install.packages(\"irace\")'", call. = FALSE)
}
if (!requireNamespace("utils", quietly = TRUE)) {
  stop("Error: The utils package is not installed. Please install it with 'install.packages(\"utils\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("Error: The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(tools)
library(irace)
library(utils)
library(optparse)

# ---------- Load utility functions ----------
source("R/utils.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Directorio de entrada con los archivos .Rdata"),

  make_option(c("-g", "--general_parameters"),
              type="character",
              help="Archivo CSV con los parámetros generales para proceso de datos"),

  make_option(c("-p", "--parameters"),
              type="character",
              help="Directorio o archivo CSV con los parámetros de locaciones"),

  make_option(c("-o", "--output"),
              type="character",
              help="Directorio de salida para los archivos STN-i"),

  make_option(c("-n", "--name"),
              type="character",
              default=NULL,
              help="Nombre del archivo STN-i de salida [default= %default]"),

  make_option(c("-b", "--best_criteria"),
              type="character", 
              default="min",
              help="Criterio para seleccionar el mejor valor ('min' o 'max') de una configuración pre-agrupamiento [default= %default]"),

  make_option(c("-c", "--quality_criteria"),
              type="character", 
              default="mean",
              help="Criterio para la calidad de configuraciones agrupadas ('min', 'max', 'mean', 'median', 'mode') [default= %default]"),

  make_option(c("-s", "--significance"),
              type="integer",
              default=2,
              help="Nivel de significancia para parámetros numéricos [default= %default]"),

  make_option(c("-t", "--instances"),
              type="character",
              default=NULL,
              help="Archivo CSV con los valores óptimos de las instancias [default= %default]"),

  make_option(c("-k", "--na_ranking"),
              type="logical",
              default=FALSE,
              help="Considerar NA como peor valor posible en rankings [default= %default]"),

  make_option(c("-r", "--representative_criteria"),
              type="character",
              default="mean",
              help="Criterio para la configuración representativa de una locación ('min', 'max', 'mean', 'median', 'mode') [default= %default]"),

  make_option(c("-v", "--verbose"),
              type="logical",
              default=FALSE,
              help="Mostrar información detallada durante el procesamiento [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$input)) {
  stop("El directorio de entrada es requerido (-i/--input)")
}
if (is.null(opt$general_parameters)) {
  stop("El archivo de parámetros generales es requerido (-g/--general_parameters)")
}
if (is.null(opt$parameters)) {
  stop("El archivo o directorio de parámetros es requerido (-p/--parameters)")
}
if (is.null(opt$output)) {
  stop("El directorio de salida es requerido (-o/--output)")
}

# Process input directory
input_dir <- normalizePath(opt$input)

# Process general parameters file
general_parameters_file <- normalizePath(opt$general_parameters)
if (!file.exists(general_parameters_file)) {
  stop(sprintf("El archivo de parámetros generales no existe: %s", general_parameters_file))
}

# Process parameters file or directory
parameters_path <- normalizePath(opt$parameters)
if (file.exists(parameters_path)) {
  if (dir.exists(parameters_path)) {
    # Es un directorio, obtener lista de archivos CSV
    parameter_files <- list.files(parameters_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(parameter_files) == 0) {
      stop(sprintf("No se encontraron archivos CSV en el directorio: %s", parameters_path))
    }
  } else {
    # Es un archivo individual
    parameter_files <- c(parameters_path)
  }
} else {
  stop(sprintf("El archivo o directorio de parámetros no existe: %s", parameters_path))
}

# Create output directory if it doesn't exist
output_dir <- normalizePath(opt$output, mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Validate if best criteria is either 'min' or 'max'
if (!(opt$best_criteria %in% c("min", "max"))) {
  stop("El criterio 'best_criteria' debe ser 'min' o 'max'")
}

# Validate if quality criteria is valid
if (!(opt$quality_criteria %in% c("min", "max", "mean", "median", "mode"))) {
  stop("El criterio 'quality_criteria' debe ser uno de los siguientes: 'min', 'max', 'mean', 'median', 'mode'")
}

# Validate if significance is a positive integer
if (!is.numeric(opt$significance) || opt$significance <= 0 || opt$significance != as.integer(opt$significance)) {
  stop("El nivel de significancia debe ser un entero positivo")
}

# Validate instances file if provided
if (!is.null(opt$instances)) {
  instances_file <- normalizePath(opt$instances)
  if (!file.exists(instances_file)) {
    stop(sprintf("El archivo de instancias no existe: %s", instances_file))
  }
} else {
  instances_file <- NULL
}

# Generate default output filename if not provided
if (is.null(opt$name)) {
  basename_input <- basename(input_dir)
  basename_params <- tools::file_path_sans_ext(basename(parameters_file))
  opt$name <- sprintf("%s_%s.csv", basename_input, basename_params)
}

# Validate if na_ranking is logical
if (!is.logical(opt$na_ranking)) {
  stop("El argumento 'na_ranking' debe ser TRUE o FALSE")
}

# Validate if representative criteria is valid
if (!(opt$representative_criteria %in% c("min", "max", "mean", "median", "mode"))) {
  stop("El criterio 'representative' debe ser uno de los siguientes: 'min', 'max', 'mean', 'median', 'mode'")
}

# Create Results directory path
results_dir <- file.path(dirname(input_dir), "Results")

# Check if we need to process Rdata files
# In Individuals-Elites mode, Results should already exist from Individuals processing
# In normal Individuals mode, we need to process Rdata files first
need_process_rdata <- !dir.exists(results_dir) || length(list.files(results_dir)) == 0

# ---------- Functions ----------

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
        iraceResults = irace_results,
        allConfigurations = irace_results$allConfigurations,
        allElites = irace_results$allElites,
        maxElites = length(irace_results$allElites) - 1,
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
        maxElites = length(irace_results$allElites) - 1,
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

  # # TEMPORAL: Calcular suma total de CONFIG_COUNT para validación
  # total_config_count <- sum(locations_df$CONFIG_COUNT)
  
  # # Validación: verificar que todas las locaciones están en SOLUTION_1 o SOLUTION_2
  # unique_locations_in_file <- unique(c(trajectories_df$SOLUTION_1, trajectories_df$SOLUTION_2))
  # locations_in_df <- locations_df$LOCATION_CODE
  # missing_locations <- setdiff(locations_in_df, unique_locations_in_file)

  # # Unificar por SOLUTION_2 para evitar duplicados
  # unique_solution2_data <- trajectories_df[!duplicated(trajectories_df$SOLUTION_2), ]
  
  # cat("========================================\n")
  # cat("VALIDACIÓN - Archivo:", output_name, "\n")
  # cat("----------------------------------------\n")
  # cat("DATOS PRE-LOCACIONES (Configuraciones):\n")
  # cat("  Total de configuraciones únicas:", length(configurations_with_quality), "\n")
  # cat("  Suma CONFIG_COUNT (de configs):", sum(sapply(configurations_with_quality, function(x) x$CONFIG_COUNT)), "\n")
  # cat("  Mejor calidad (FINAL_MNRG):", sprintf(paste0("%.", significance, "f"), min(sapply(configurations_with_quality, function(x) x$FINAL_MNRG), na.rm = TRUE)), "\n")
  # cat("----------------------------------------\n")
  # cat("DATOS POST-LOCACIONES (Agrupadas):\n")
  # cat("  Total de locaciones creadas:", nrow(locations_df), "\n")
  # cat("  Suma CONFIG_COUNT (de locations):", total_config_count, "\n")
  # cat("  Mejor calidad (FINAL_QUALITY):", sprintf(paste0("%.", significance, "f"), min(locations_df$FINAL_QUALITY, na.rm = TRUE)), "\n")
  # cat("----------------------------------------\n")
  # cat("DATOS EN ARCHIVO (Usando solo SOLUTION_2):\n")
  # cat("  Total de locaciones únicas (SOLUTION_2):", length(unique(trajectories_df$SOLUTION_2)), "\n")
  # unique_solution2_data <- trajectories_df[!duplicated(trajectories_df$SOLUTION_2), ]
  # cat("  Suma CONFIG_COUNT (CONFIG_COUNT_2):", sum(unique_solution2_data$CONFIG_COUNT_2, na.rm = TRUE), "\n")
  # cat("  Mejor calidad (QUALITY_2):", sprintf(paste0("%.", significance, "f"), min(unique_solution2_data$QUALITY_2, na.rm = TRUE)), "\n")
  # cat("  Total de trayectorias guardadas:", nrow(trajectories_df), "\n")
  
  # # Verificar si la mejor locación está en el archivo
  # best_location_idx <- which.min(locations_df$FINAL_QUALITY)
  # best_location_code <- locations_df$LOCATION_CODE[best_location_idx]
  # best_location_quality <- locations_df$FINAL_QUALITY[best_location_idx]
  # is_in_solution1 <- best_location_code %in% trajectories_df$SOLUTION_1
  # is_in_solution2 <- best_location_code %in% trajectories_df$SOLUTION_2
  # if (is_in_solution1 || is_in_solution2) {
  #   cat("  ✓ Mejor locación", best_location_code, "con calidad", sprintf(paste0("%.", significance, "f"), best_location_quality), "SÍ está en trayectorias\n")
  # } else {
  #   cat("  ⚠️  ADVERTENCIA: Mejor locación", best_location_code, "con calidad", sprintf(paste0("%.", significance, "f"), best_location_quality), "NO está en trayectorias!\n")
  # }
  
  # if (length(missing_locations) > 0) {
  #   cat("  ⚠️  ADVERTENCIA: Hay", length(missing_locations), "locaciones NO guardadas en trayectorias!\n")
  #   cat("  Locaciones faltantes:", paste(missing_locations, collapse = ", "), "\n")
  # } else {
  #   cat("  ✓ Todas las locaciones están en las trayectorias (SOLUTION_1 o SOLUTION_2)\n")
  # }
  # cat("========================================\n")

  # Write output files
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file_path <- file.path(output_dir, output_name)
  write.table(trajectories_df, file = output_file_path, sep = ";", row.names = FALSE, quote = FALSE)
}

# ---------- Process Rdata files if needed ----------

if (need_process_rdata && opt$verbose) {
  cat("Procesando archivos Rdata y creando estructura Results usando parámetros generales...\n")
  process_rdata_files(
    input_dir = input_dir,
    parameters_file = general_parameters_file,
    results_dir = results_dir,
    optimum_file = instances_file,
    best_criteria = opt$best_criteria,
    is_na_ranking = opt$na_ranking,
    verbose = opt$verbose
  )
}

# Process each parameter file
for (parameter_file in parameter_files) {
  # Generate output name if not provided
  if (is.null(opt$name)) {
    file_basename <- tools::file_path_sans_ext(basename(parameter_file))
    current_name <- sprintf("%s_%s.csv", basename(input_dir), file_basename)
  } else {
    # If name is provided and multiple files, add suffix
    if (length(parameter_files) > 1) {
      file_basename <- tools::file_path_sans_ext(basename(parameter_file))
      base_name <- tools::file_path_sans_ext(opt$name)
      ext <- tools::file_ext(opt$name)
      current_name <- sprintf("%s-%s.%s", base_name, file_basename, ext)
    } else {
      current_name <- opt$name
    }
  }
  
  if (opt$verbose) cat(sprintf("\nProcesando archivo de parámetros: %s\n", basename(parameter_file)))
  if (opt$verbose) cat(sprintf("Generando archivo STN-i: %s\n", current_name))
  
  # Generate STN-i file using current parameters
  generate_stn_i(
    input_dir = results_dir,
    parameters_file = parameter_file,
    output_dir = output_dir,
    output_name = current_name,
    quality_criteria = opt$quality_criteria,
    representative_criteria = opt$representative_criteria,
    significance = opt$significance,
    verbose = opt$verbose
  )
}

#  ---------- Clean up ----------

# Clear the workspace and garbage collection
rm(list = ls())
gc()
quit(save = "no")

# nolint end
