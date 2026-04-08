#!/bin/bash

# ==============================================================================
# Script: run_all_generate_elite_STN-i_file.sh
# Description: Generates elite STN-i .csv files for all scenarios and locations
#
# Usage:
#   1. Make executable: chmod +x run_all_generate_elite_STN-i_file.sh
#   2. Run: ./run_all_generate_elite_STN-i_file.sh
#
# Output:
#     - Log file: Logs/run_all_generate_elite_STN-i_file.log
#     - STN-i files: Experiments/<algorithm>/Individuals-Elites/<scenario>/STN-i-Files/
# ==============================================================================

LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

LOG_FILE="$LOG_DIR/run_all_generate_elite_STN-i_file.log"
echo "=== Elite STN-i generation started at $(date) ===" > "$LOG_FILE"

# Define algorithms and scenarios
declare -A scenarios
scenarios["ACOTSP"]="BL BL-45 BH BH-90"
#scenarios["PSO-X"]="BL BL-32 BH BH-65"

run_generate_elite_stn_i() {
  local alg=$1
  local scenario=$2
  local elites_dir=$3
  local scenario_dir=$4
  local general_params=$5
  local params_dir=$6
  local output_dir=$7
  local optimum_file=$8
  
  echo ">> Generating elite STN-i for $alg / $scenario..." | tee -a "$LOG_FILE"

  if ! Rscript R/generate_elite_STN-i_file.R \
    --elites_dir="$elites_dir" \
    --scenario_dir="$scenario_dir" \
    --general_parameters="$general_params" \
    --parameters="$params_dir" \
    --output="$output_dir" \
    --optimum_file="$optimum_file" \
    --name="$scenario.csv" \
    --best_criteria="min" \
    --quality_criteria="mean" \
    --significance=2 \
    --representative_criteria="mean" \
    --verbose=TRUE >> "$LOG_FILE" 2>&1; then
    echo "Error: Failed to generate elite STN-i for $alg / $scenario" | tee -a "$LOG_FILE"
    return 1
  else
    echo "Success: Elite STN-i generated for $alg / $scenario" | tee -a "$LOG_FILE"
    return 0
  fi
}

# Process each algorithm
for alg in "${!scenarios[@]}"; do
  echo "" | tee -a "$LOG_FILE"
  echo "============================================" | tee -a "$LOG_FILE"
  echo "Processing algorithm: $alg" | tee -a "$LOG_FILE"
  echo "============================================" | tee -a "$LOG_FILE"

  for scenario in ${scenarios[$alg]}; do
    elites_dir="Experiments/$alg/Individuals-Elites/General-Data/All-Elites"
    scenario_dir="Experiments/$alg/Individuals/$scenario"
    general_params="Experiments/$alg/Others/Parameters.csv"
    params_dir="Experiments/$alg/Locations"
    output_dir="Experiments/$alg/Individuals-Elites/$scenario/STN-i-Files"
    optimum_file="Experiments/$alg/Others/Optimum_Elite.csv"

    # Verify paths exist
    if [[ ! -d "$elites_dir" ]]; then
      echo "Error: Elites directory not found: $elites_dir" | tee -a "$LOG_FILE"
      continue
    fi

    if [[ ! -d "$scenario_dir" ]]; then
      echo "Error: Scenario directory not found: $scenario_dir" | tee -a "$LOG_FILE"
      continue
    fi

    if [[ ! -f "$general_params" ]]; then
      echo "Error: General parameters file not found: $general_params" | tee -a "$LOG_FILE"
      continue
    fi

    if [[ ! -d "$params_dir" ]]; then
      echo "Error: Parameters directory not found: $params_dir" | tee -a "$LOG_FILE"
      continue
    fi

    if [[ ! -f "$optimum_file" ]]; then
      echo "Error: Optimum file not found: $optimum_file" | tee -a "$LOG_FILE"
      continue
    fi

    mkdir -p "$output_dir"

    run_generate_elite_stn_i "$alg" "$scenario" "$elites_dir" "$scenario_dir" \
                            "$general_params" "$params_dir" "$output_dir" "$optimum_file"
  done
done

echo "" | tee -a "$LOG_FILE"
echo "=== Elite STN-i generation finished at $(date) ===" >> "$LOG_FILE"
echo "Log file: $LOG_FILE"
