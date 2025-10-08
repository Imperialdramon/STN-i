library(tools)
library(irace)
library(utils)
library(ggplot2)
library(reshape2)

# Función auxiliar para crear archivo de configuraciones
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
        return(NA_character_)
      }
      value <- param_row[[pname]]
      type <- param_types[[pname]]
      if (type == "c" || type == "cat" || type == "s" || type == "string") {
        return(as.character(value))
      } else if (type == "i" || type == "int" || type == "i,log") {
        return(as.character(as.integer(value)))
      } else if (type == "r" || type == "real" || type == "r,log") {
        return(as.character(as.numeric(value)))
      } else if (type == "o" || type == "ord") {
        return(as.character(value))
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
}

# Crear archivo de trayectorias
create_trajectories_file <- function(iraceResults, raceData, allElites, iterations, output_file) {
  # Inicializar data frame para las trayectorias
  trajectories <- data.frame(
    PATH = logical(),
    ORIGIN_ITER = integer(),
    ORIGIN_CONFIG_ID = character(),
    DESTINY_ITER = integer(),
    DESTINY_CONFIG_ID = character(),
    stringsAsFactors = FALSE
  )

  # Procesar cada iteración
  for (iter in 1:iterations) {
    configs <- raceData[[iter]]
    
    # Para cada configuración en la iteración actual
    for (i in 1:nrow(configs)) {
      config <- configs[i, ]
      config_id <- as.character(config$.ID.)
      
      if (iter == 1) {
        # Configuraciones de primera iteración se conectan consigo mismas con PATH = FALSE
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
        # Verificar si era élite en la iteración anterior
        is_elite <- config_id %in% allElites[[iter-1]]
        
        # Si tiene padre en la iteración anterior, crear conexión
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
          # Si es élite y no tiene conexión con la iteración anterior, se conecta consigo misma
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
  
  # Guardar archivo con separador punto y coma
  write.table(trajectories, file = output_file, sep = ";", row.names = FALSE, quote = FALSE)
}

# Función para crear archivo de instancias
create_instances_file <- function(iraceResults, experiments, optimum_data, output_file) {
  instances_full <- iraceResults$scenario$instances
  experiment_ids <- as.integer(rownames(experiments))
  
  # Crear data frame base con las filas que efectivamente están en experiments
  instances_df <- data.frame(
    EXPERIMENT_ID = experiment_ids,
    INSTANCE_ID = sapply(experiment_ids, function(id) iraceResults$state$instances_log[id]$instanceID),
    SEED = sapply(experiment_ids, function(id) iraceResults$state$instances_log[id]$seed),
    NAME = sapply(experiment_ids, function(id) basename(instances_full[iraceResults$state$instances_log[id]$instanceID])),
    OPTIMUM = NA,
    stringsAsFactors = FALSE
  )
  
  # Agregar columna de óptimos si existe la data
  if (!is.null(optimum_data)) {
    for (i in seq_len(nrow(instances_df))) {
      instance_name <- instances_df$NAME[i]
      opt_row <- optimum_data[optimum_data$INSTANCE == instance_name, ]
      instances_df$OPTIMUM[i] <- if (nrow(opt_row) > 0 && !is.na(opt_row$OPTIMUM)) opt_row$OPTIMUM else NA
    }
  }
  
  # Guardar archivo con separador punto y coma
  write.table(instances_df, file = output_file, sep = ";", 
              row.names = FALSE, quote = FALSE)
              
  # Retornar el data frame
  return(instances_df)
}

# Función para crear archivo de resultados
create_results_file <- function(experiments, instances_df, output_file) {
  # Obtener IDs de configuraciones
  config_ids <- colnames(experiments)
  
  # Calcular métricas de calidad básicas para cada configuración
  quality_metrics <- lapply(config_ids, function(id) {
    qualities <- as.numeric(experiments[, id])
    qualities <- qualities[!is.na(qualities)]
    c(
      CONFIG_ID = id,
      INSTANCES = length(qualities),
      BQ = min(qualities),
      MQ = mean(qualities)
    )
  })
  results_df <- as.data.frame(do.call(rbind, quality_metrics))

  # Calcular rankings de calidad
  results_df$BQR <- rank(results_df$BQ, ties.method = "min")
  results_df$MQR <- rank(results_df$MQ, ties.method = "min")

  # Normalizar rankings (0-1)
  results_df$BQRN <- (results_df$BQR - 1) / (nrow(results_df) - 1)
  results_df$MQRN <- (results_df$MQR - 1) / (nrow(results_df) - 1)

  # Crear matriz de GAPs usando óptimos de instances_df
  gaps_matrix <- matrix(NA, nrow = nrow(experiments), ncol = ncol(experiments))
  colnames(gaps_matrix) <- colnames(experiments)
  rownames(gaps_matrix) <- rownames(experiments)

  # Se obtienen por si son necesarios
  best_qualities <- apply(experiments, 1, function(x) min(x, na.rm = TRUE))

  for (i in seq_len(nrow(experiments))) {
    experiment_id <- as.integer(rownames(experiments)[i])
    opt_value <- instances_df$OPTIMUM[instances_df$EXPERIMENT_ID == experiment_id]
    if (!is.null(opt_value) && !is.na(opt_value)) {
      best_quality <- opt_value
    } else {
      best_quality <- best_qualities[i]
    }
    for (j in seq_len(ncol(experiments))) {
      value <- experiments[i, j]
      if (!is.na(value)) {
        gaps_matrix[i, j] <- 100 * abs(value - best_quality) / best_quality
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
  
  # Añadir métricas de GAP al data frame de resultados
  results_df$BG <- gap_df$BG
  results_df$MG <- gap_df$MG
  
  # Calcular rankings de GAP
  results_df$BGR <- rank(results_df$BG, ties.method = "min")
  results_df$MGR <- rank(results_df$MG, ties.method = "min")
  
  # Normalizar rankings de GAP (0-1)
  results_df$BGRN <- (results_df$BGR - 1) / (nrow(results_df) - 1)
  results_df$MGRN <- (results_df$MGR - 1) / (nrow(results_df) - 1)
  
  # Reordenar columnas según el formato especificado
  results_df <- results_df[, c("CONFIG_ID", "INSTANCES", 
                              "BQ", "BQR", "BQRN",
                              "MQ", "MQR", "MQRN",
                              "BG", "BGR", "BGRN",
                              "MG", "MGR", "MGRN")]
  
  # Guardar archivo con separador punto y coma
  write.table(results_df, file = output_file, sep = ";", 
              row.names = FALSE, quote = FALSE)
  
  return(results_df)
}

# Definir la función para procesar archivos .Rdata
process_rdata_files <- function(input_dir, parameters_file, optimum_file, results_dir) {
  # Listar todos los archivos .Rdata en el directorio de entrada
  files <- list.files(input_dir, pattern = "\\.Rdata$", full.names = TRUE)
  # Crear el directorio de resultados si no existe
  dir.create(results_dir, showWarnings = FALSE)

  # Leer parámetros desde CSV con separador ';'
  parameters <- read.csv(parameters_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  # Limpiar posibles espacios en los nombres de columnas
  parameters$NAME <- trimws(parameters$NAME)
  parameters$TYPE <- trimws(parameters$TYPE)
  parameters$CONDITIONAL <- trimws(parameters$CONDITIONAL)

  # Validar si el archivo de óptimos existe
  if (!is.null(optimum_file) && file.exists(optimum_file)) {
    optimum_data <- read.csv(optimum_file, header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("INSTANCE", "OPTIMUM") %in% colnames(optimum_data))) {
      stop("El archivo de óptimos debe contener las columnas 'INSTANCE' y 'OPTIMUM'.")
    }
  } else {
    optimum_data <- NULL
  }

  # Procesar cada archivo .Rdata
  for (file in files) {
    # Crear carpeta con seed de nombre (nombre del archivo rdata sin extensión)
    seed_dir <- file.path(results_dir, file_path_sans_ext(basename(file)))
    dir.create(seed_dir, showWarnings = FALSE)

    # Cargar el archivo .Rdata
    iraceResults <- read_logfile(file)

    # Acceso a datos a través de iraceResults$
    experiments <- iraceResults$experiments
    allConfigurations <- iraceResults$allConfigurations
    allElites <- iraceResults$allElites
    raceData <- iraceResults$raceData
    iterations <- length(allElites) - 1

    # Proceso de creación de archivo de configuraciones
    create_configurations_file(
      allConfigurations = allConfigurations,
      allElites = allElites,
      iterations = iterations,
      parameters = parameters,
      output_file = file.path(seed_dir, "configurations.csv")
    )

    # Proceso de creación de archivo de trayectorias
    create_trajectories_file(
      iraceResults = iraceResults,
      raceData = raceData,
      allElites = allElites,
      iterations = iterations,
      output_file = file.path(seed_dir, "trajectories.csv")
    )

    # Proceso de creación de archivo de instancias usadas y obtener el dataframe
    instances_df <- create_instances_file(
      iraceResults = iraceResults,
      experiments = experiments,
      optimum_data = optimum_data,
      output_file = file.path(seed_dir, "instances.csv")
    )

    # Proceso de creación de archivo de resultados generales usando instances_df
    results_df <- create_results_file(
      experiments = experiments,
      instances_df = instances_df,
      output_file = file.path(seed_dir, "results.csv")
    )
  }
}

# Definir los directorios y el archivo de parámetros
input_directory <- "Experiments/ACOTSP-QGR/BH/Data"
optimum_file <- NULL
parameters_file <- "Experiments/ACOTSP-QGR/Parameters.csv"
results_directory <- "Experiments/ACOTSP-QGR/BH/Results"
process_rdata_files(input_directory, parameters_file, optimum_file, results_directory)

# Definir los directorios y el archivo de parámetros
input_directory <- "Experiments/ACOTSP-QGR/BH-90/Data"
optimum_file <- NULL
parameters_file <- "Experiments/ACOTSP-QGR/Parameters.csv"
results_directory <- "Experiments/ACOTSP-QGR/BH-90/Results"
process_rdata_files(input_directory, parameters_file, optimum_file, results_directory)

# Definir los directorios y el archivo de parámetros
input_directory <- "Experiments/ACOTSP-QGR/BL/Data"
optimum_file <- NULL
parameters_file <- "Experiments/ACOTSP-QGR/Parameters.csv"
results_directory <- "Experiments/ACOTSP-QGR/BL/Results"
process_rdata_files(input_directory, parameters_file, optimum_file, results_directory)

# Definir los directorios y el archivo de parámetros
input_directory <- "Experiments/ACOTSP-QGR/BL-45/Data"
optimum_file <- NULL
parameters_file <- "Experiments/ACOTSP-QGR/Parameters.csv"
results_directory <- "Experiments/ACOTSP-QGR/BL-45/Results"
process_rdata_files(input_directory, parameters_file, optimum_file, results_directory)
