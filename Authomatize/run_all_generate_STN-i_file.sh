#!/bin/bash

# ==============================================================================
# Script: run_all_generate_STN-i_file.sh
# Description: Generates .csv STN-i files for all experiments and locations
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_generate_STN-i_file.sh
#
#   2. Run the script (using Individuals by default):
#        ./run_all_generate_STN-i_file.sh
#
#   3. Run the script with Individuals-Elites (uses Results from Individuals):
#        ./run_all_generate_STN-i_file.sh --mode=Individuals-Elites
#
#   Output (Individuals mode):
#     - Log file in Logs/run_all_generate_STN-i_file_Individuals.log
#     - Generated STN-i .csv files in Individuals/*/STN-i-Files/
#
#   Output (Individuals-Elites mode):
#     - Log file in Logs/run_all_generate_STN-i_file_Individuals-Elites.log
#     - Generated STN-i .csv files in Individuals-Elites/*/STN-i-Files/
#     - Uses shared Results directory from Individuals processing
# ==============================================================================

# Parse command line arguments
MODE="Individuals"  # Default mode
for arg in "$@"; do
  case $arg in
    --mode=*)
      MODE="${arg#--mode=}"
      ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--mode=Individuals|Individuals-Elites]"
      exit 1
      ;;
  esac
done

# Validate mode
if [[ "$MODE" != "Individuals" && "$MODE" != "Individuals-Elites" ]]; then
  echo "Error: Invalid mode '$MODE'. Must be 'Individuals' or 'Individuals-Elites'"
  exit 1
fi

echo "Running with mode: $MODE"

# Create log directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Set log file path
LOG_FILE="$LOG_DIR/run_all_generate_STN-i_file_${MODE}.log"
echo "=== STN-i data generation started at $(date) ===" > "$LOG_FILE"
echo "Mode: $MODE" >> "$LOG_FILE"

# Function to run the Rscript and log output
run_generate_rdata() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/generate_STN-i_file.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
declare -A experiments
#experiments["ACOTSP"]="BL BL-45 BH BH-90"
experiments["PSO-X"]="BL BL-32 BH BH-65"

# Define optimum files per algorithm
declare -A optimum_files
#optimum_files["ACOTSP"]="Optimum.csv"
optimum_files["PSO-X"]="Optimum.csv"

# For Individuals-Elites mode, check if Results directory exists
if [[ "$MODE" == "Individuals-Elites" ]]; then
  # Use the shared Results directory from Individuals processing
  RESULTS_BASE_DIR="Experiments"
  
  for alg in "${!experiments[@]}"; do
    for exp in ${experiments[$alg]}; do
      results_dir="$RESULTS_BASE_DIR/$alg/$MODE/$exp/Data"
      if [[ ! -d "$results_dir" ]]; then
        echo "    Warning: Individuals directory not found for $alg: $results_dir" | tee -a "$LOG_FILE"
        echo "    Make sure Individuals processing has completed successfully" | tee -a "$LOG_FILE"
      fi
    done
  done
fi

# Loop over all combinations of algorithm, experiment, and level
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    input_dir="Experiments/$alg/$MODE/$exp"
    if [[ "$MODE" == "Individuals" ]]; then
      input_dir="${input_dir}/Data"
    else
      input_dir="${input_dir}/Results"
    fi

    parameters="Experiments/$alg/Others/Parameters.csv"
    locations="Experiments/$alg/Locations"
    output_dir="Experiments/$alg/$MODE/$exp/STN-i-Files"
    output_file="$exp.csv"
    optimum_file="Experiments/$alg/Others/Optimum.csv"

    # Validate input directory exists
    if [[ ! -d "$input_dir" ]]; then
      echo "    Skipping: Input directory not found: $input_dir" | tee -a "$LOG_FILE"
      continue
    fi

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
