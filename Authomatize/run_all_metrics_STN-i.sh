#!/bin/bash

# ==============================================================================
# Script: run_all_metrics_STN-i.sh
# Description: Generates .csv metrics files for all experiments and locations
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_metrics_STN-i.sh
#
#   2. Run the script:
#        ./run_all_metrics_STN-i.sh
#
#   Output:
#     - Log file in Logs/run_all_metrics_STN-i.log
#     - Generated .csv metrics files in respective experiment directories
# ==============================================================================

# Create log directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Set log file path
LOG_FILE="$LOG_DIR/run_all_metrics_STN-i.log"
echo "=== STN-i data generation started at $(date) ===" > "$LOG_FILE"

# Function to run the Rscript and log output
run_generate_rdata() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/metrics_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
declare -A experiments
experiments["ACOTSP"]="BL BL-45 BH BH-90"
experiments["PSO-X"]="BL BL-32 BH BH-65"

# Define locations type per algorithm
declare -A locations_type
locations_type["ACOTSP"]="L1 L2 L3 L4 L5"
locations_type["PSO-X"]="L1 L2 L3 L4 L5"

# Loop over all combinations of algorithm, experiment, and level
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for loc in ${locations_type[$alg]}; do
      input_file="Experiments/${alg}/Individuals/${exp}/STN-i-RData/${exp}-${loc}_stn_i.RData"
      output_dir="Experiments/${alg}/Individuals/${exp}/STN-i-Metrics"

      run_generate_rdata \
        -i "$input_file" \
        -o "$output_dir" \
        -v FALSE
    done
  done
done

echo "=== STN-i files generation finished at $(date) ===" >> "$LOG_FILE"
