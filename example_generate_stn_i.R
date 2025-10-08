#!/usr/bin/env Rscript

# Definir rutas para el ejemplo BL-45
input_directory <- "Experiments/ACOTSP/BL-45/Data"
parameters_file <- "Experiments/ACOTSP/Parameters/L4.csv"
output_directory <- "Experiments/ACOTSP/BL-45/STN-i-Files"
output_file <- "BL-45-L4.stn-i"

# Llamar al script con los argumentos correspondientes
cmd_args <- sprintf(
  "Rscript R/generate_STN-i_file.R -i '%s' -p '%s' -o '%s' -n '%s' -c 'min' -b 'min'",
  input_directory,
  parameters_file,
  output_directory,
  output_file
)

# Imprimir el comando para referencia
cat("Ejecutando:", cmd_args, "\n")

# Ejecutar el comando
system(cmd_args)