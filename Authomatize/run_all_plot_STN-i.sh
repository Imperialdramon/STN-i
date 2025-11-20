#!/bin/bash

# ==============================================================================
# Script: run_all_plot_STN-i.sh
# Description: Generates PDF plots for all STN-i .Rdata files with customizable options
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_plot_STN-i.sh
#
#   2. Run the script (using Individuals by default):
#        ./run_all_plot_STN-i.sh
#
#   3. Run the script with Individuals-Elites:
#        ./run_all_plot_STN-i.sh --mode=Individuals-Elites
#
#   4. Run with custom options:
#        ./run_all_plot_STN-i.sh --palette=2 --layout=kk --size_factor=1.5
#
#   Output:
#     - Log file in Logs/run_all_plot_STN-i_{MODE}.log
#     - Generated PDF plots in respective experiment directories
# ==============================================================================

# Parse command line arguments
MODE="Individuals"  # Default mode
SIZE_FACTOR=1.0     # Default size factor
VERBOSE="FALSE"     # Default verbose

for arg in "$@"; do
  case $arg in
    --mode=*)
      MODE="${arg#--mode=}"
      ;;
    --size_factor=*)
      SIZE_FACTOR="${arg#--size_factor=}"
      ;;
    --verbose=*)
      VERBOSE="${arg#--verbose=}"
      ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--mode=Individuals|Individuals-Elites] [--size_factor=FACTOR] [--verbose=TRUE|FALSE]"
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
LOG_FILE="$LOG_DIR/run_all_plot_STN-i_${MODE}.log"
echo "=== STN-i plot generation started at $(date) ===" > "$LOG_FILE"
echo "Mode: $MODE" >> "$LOG_FILE"

# Function to run the Rscript and log output
run_plot_stn_i() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == -f* ]] && output_file="${arg#-f}"
  done

  echo ">> Plotting: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/plot_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to plot $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
declare -A experiments
#experiments["ACOTSP"]="BL BL-45 BH BH-90"
#experiments["PSO-X"]="BL BL-32 BH BH-65"
experiments["ACOTSP"]="BL"

# Define locations type per algorithm
declare -A locations_type
#locations_type["ACOTSP"]="L0 L1 L2 L3 L4 L5"
#locations_type["PSO-X"]="L0 L1 L2 L3 L4 L5"
locations_type["ACOTSP"]="L0"

# Valid options to iterate
VALID_LAYOUTS=("fr")
#VALID_LAYOUTS=("fr" "kk" "circle" "grid" "sphere" "random" "drl" "graphopt")

VALID_PALETTES=(1)
#VALID_PALETTES=(1 2 3 4 5)

VALID_SHOW_REGULAR=("TRUE" "FALSE")
# Note: SHOW_START_REGULAR only makes sense when SHOW_REGULAR=TRUE
# When SHOW_REGULAR=FALSE, SHOW_START_REGULAR must also be FALSE

VALID_ZOOM_QUANTILES=("NA")
#VALID_ZOOM_QUANTILES=("NA" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9")

# Loop over all combinations of algorithm, experiment, and level
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for loc in ${locations_type[$alg]}; do
      input_file="Experiments/${alg}/${MODE}/${exp}/STN-i-RData/${exp}-${loc}_stn_i.Rdata"
      base_output_dir="Experiments/${alg}/${MODE}/${exp}/STN-i-Plots"

      # Validate input file exists
      if [[ ! -f "$input_file" ]]; then
        echo "⚠️  Skipping: Input file not found: $input_file" | tee -a "$LOG_FILE"
        continue
      fi

      # Loop over valid options
      for palette in "${VALID_PALETTES[@]}"; do
        for layout in "${VALID_LAYOUTS[@]}"; do
          for show_regular in "${VALID_SHOW_REGULAR[@]}"; do
            # Determine valid SHOW_START_REGULAR values based on SHOW_REGULAR
            if [[ "$show_regular" == "TRUE" ]]; then
              valid_start_regular=("TRUE" "FALSE")
            else
              # When SHOW_REGULAR=FALSE, SHOW_START_REGULAR must be FALSE
              valid_start_regular=("FALSE")
            fi
            
            for show_start_regular in "${valid_start_regular[@]}"; do
              for zoom_quantile in "${VALID_ZOOM_QUANTILES[@]}"; do
                output_dir="${base_output_dir}/${loc}"
                mkdir -p "$output_dir"
                
                output_file="${exp}-${loc}-P_${palette}-L_${layout}-SR_${show_regular}-SSR_${show_start_regular}-Z_${zoom_quantile}.pdf"

                run_plot_stn_i \
                  -i "$input_file" \
                  -o "$output_dir" \
                  -f "$output_file" \
                  -l "$layout" \
                  -r "$show_regular" \
                  -s "$show_start_regular" \
                  -z "$SIZE_FACTOR" \
                  -p "$palette" \
                  -q "$zoom_quantile" \
                  -v "$VERBOSE"
              done
            done
          done
        done
      done
    done
  done
done

echo "=== STN-i plot generation finished at $(date) ===" >> "$LOG_FILE"
