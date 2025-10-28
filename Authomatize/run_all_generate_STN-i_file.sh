#!/bin/bash

# ==============================================================================
# Script: run_all_generate_STN-i_file.sh
# Description: Generates .csv STN-i files for all experiments and locations
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_generate_STN-i_file.sh
#
#   2. Run the script:
#        ./run_all_generate_STN-i_file.sh
#
#   Output:
#     - Log file in Logs/run_all_generate_STN-i_file.log
#     - Generated STN-i .csv files in respective experiment directories
# ==============================================================================

# Create log directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Set log file path
LOG_FILE="$LOG_DIR/run_all_generate_STN-i_file.log"
echo "=== STN-i data generation started at $(date) ===" > "$LOG_FILE"

# Function to run the Rscript and log output
run_generate_rdata() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/generate_STN-i_file.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
declare -A experiments
experiments["ACOTSP"]="BL BL-45 BH BH-90"
experiments["PSO-X"]="BL BL-32 BH BH-65"

# Define optimum files per algorithm
declare -A optimum_files
optimum_files["ACOTSP"]="Optimum.csv"
optimum_files["PSO-X"]="Optimum.csv"


# Loop over all combinations of algorithm, experiment, and level
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    input_dir="Experiments/$alg/Individuals/$exp/Data"
    parameters="Experiments/$alg/Others/Parameters.csv"
    locations="Experiments/$alg/Locations"
    output_dir="Experiments/$alg/Individuals/$exp/STN-i-Files"
    output_file="$exp.csv"
    optimum_file="Experiments/$alg/Others/Optimum.csv"

    run_generate_rdata \
      -i "$input_dir" \
      -g "$parameters" \
      -p "$locations" \
      -o "$output_dir" \
      -n "$output_file" \
      -t "$optimum_file" \
      -b "min" \
      -k FALSE \
      -c "mean" \
      -r "min" \
      -s 8 \
      -v FALSE
  done
done

echo "=== STN-i files generation finished at $(date) ===" >> "$LOG_FILE"
