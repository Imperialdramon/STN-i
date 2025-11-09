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
#                               [--name=<output_name>] [--best=<min|max>] /
#                               [--criteria=<min|max|mean|median|mode>] /
#                               [--significance=<integer>] [--instances=<instances_file>] /
#                               [--na_ranking=<TRUE|FALSE>] /
#                               [--representative=<min|max|mean|median|mode>] /
#                               [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input              : (Required) Directory containing input .Rdata files
# --general_parameters : (Required) CSV file with general parameters for data processing
# --parameters         : (Required) Directory or CSV file with location parameters
# --output            : (Required) Output directory for STN-i files
# --name              : (Optional) Name of the output STN-i file
# --best              : (Optional) Criterion for best value ('min' or 'max', default: 'min')
# --criteria          : (Optional) Quality criterion ('min','max','mean','median','mode', default: 'mean')
# --significance      : (Optional) Significance level for numerical parameters (default: 2)
# --instances         : (Optional) CSV file with instance optimum values
# --na_ranking        : (Optional) Consider NA as worst possible value in rankings (default: FALSE)
# --representative    : (Optional) Criterion for representative configuration (default: 'mean')
# --verbose           : (Optional) Show detailed processing information (default: FALSE)
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

  make_option(c("-b", "--best"),
              type="character", 
              default="min",
              help="Criterio para seleccionar el mejor valor ('min' o 'max') de una configuración pre-agrupamiento [default= %default]"),

  make_option(c("-c", "--criteria"),
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

  make_option(c("-r", "--representative"),
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
# if (!dir.exists(input_dir)) {
#   stop(sprintf("El directorio de entrada no existe: %s", input_dir))
# }

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
if (!(opt$best %in% c("min", "max"))) {
  stop("El criterio 'best' debe ser 'min' o 'max'")
}

# Validate if quality criteria is valid
if (!(opt$criteria %in% c("min", "max", "mean", "median", "mode"))) {
  stop("El criterio 'criteria' debe ser uno de los siguientes: 'min', 'max', 'mean', 'median', 'mode'")
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
if (!(opt$representative %in% c("min", "max", "mean", "median", "mode"))) {
  stop("El criterio 'representative' debe ser uno de los siguientes: 'min', 'max', 'mean', 'median', 'mode'")
}

# Create Results directory path
results_dir <- file.path(dirname(input_dir), "Results")

# Check if we need to process Rdata files
# In Individuals-Elites mode, Results should already exist from Individuals processing
# In normal Individuals mode, we need to process Rdata files first
need_process_rdata <- !dir.exists(results_dir) || length(list.files(results_dir)) == 0

if (need_process_rdata && opt$verbose) {
  cat("Procesando archivos Rdata y creando estructura Results usando parámetros generales...\n")
  process_rdata_files(
    input_dir = input_dir,
    parameters_file = general_parameters_file,
    results_dir = results_dir,
    optimum_file = instances_file,
    best_criteria = opt$best,
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
    quality_criteria = opt$criteria,
    representative_criteria = opt$representative,
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
