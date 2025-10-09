#!/usr/bin/env Rscript

# Import required libraries
library(optparse)
source("R/functions.R")

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), 
              type="character", 
              help="Directorio de entrada con los archivos .Rdata"),
  
  make_option(c("-p", "--parameters"),
              type="character",
              help="Archivo CSV con los parámetros"),
  
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
              help="Criterio para seleccionar el mejor valor ('min' o 'max') [default= %default]"),
  
  make_option(c("-c", "--criteria"),
              type="character", 
              default="mean",
              help="Criterio para la calidad de configuraciones ('min', 'max', 'mean', 'median', 'mode') [default= %default]"),
  
  make_option(c("-s", "--significance"),
              type="integer",
              default=2,
              help="Nivel de significancia para parámetros numéricos [default= %default]"),
  
  make_option(c("-t", "--instances"),
              type="character",
              default=NULL,
              help="Archivo CSV con los valores óptimos de las instancias [default= %default]"),
              
  make_option(c("--na_ranking"),
              type="logical",
              default=FALSE,
              help="Considerar NA como peor valor posible en rankings [default= %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$input)) {
  stop("El directorio de entrada es requerido (-i/--input)")
}
if (is.null(opt$parameters)) {
  stop("El archivo de parámetros es requerido (-p/--parameters)")
}
if (is.null(opt$output)) {
  stop("El directorio de salida es requerido (-o/--output)")
}

# Process input directory
input_dir <- normalizePath(opt$input)
if (!dir.exists(input_dir)) {
  stop(sprintf("El directorio de entrada no existe: %s", input_dir))
}

# Process parameters file
parameters_file <- normalizePath(opt$parameters)
if (!file.exists(parameters_file)) {
  stop(sprintf("El archivo de parámetros no existe: %s", parameters_file))
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

# Create Results directory path
results_dir <- file.path(dirname(input_dir), "Results")

# First process Rdata files and create Results structure
cat("Procesando archivos Rdata y creando estructura Results...\n")
process_rdata_files(
  input_dir = input_dir,
  parameters_file = parameters_file,
  results_dir = results_dir,
  optimum_file = instances_file,
  best_criteria = opt$best,
  is_na_ranking = opt$na_ranking
)

# Then generate STN-i file from processed Results
# cat("Generando archivo STN-i desde Results...\n")
# generate_stn_i(
#   input_dir = results_dir,
#   parameters = read_parameters_file(parameters_file),
#   output_dir = output_dir,
#   output_name = opt$name,
#   best_criteria = opt$best,
#   quality_criteria = opt$criteria,
#   significance = opt$significance
# )
