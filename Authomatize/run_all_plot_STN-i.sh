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
    echo "Error: Failed to plot $output_file" | tee -a "$LOG_FILE"
  else
    echo "Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
# Note: Add or remove experiments as needed (space-separated)
declare -A experiments
experiments["ACOTSP"]="BL BL-45 BH BH-90"
experiments["PSO-X"]="BL BL-32 BH BH-65"

# Define location types for each algorithm
# Note: Add or remove location types as needed (space-separated)
declare -A locations_type
#locations_type["ACOTSP"]="L0 L1 L2 L3 L4 L5"
#locations_type["PSO-X"]="L0 L1 L2 L3 L4 L5"
locations_type["ACOTSP"]="L0"
locations_type["PSO-X"]="L0"

# List of valid graph layouts
# Note: Change layout function based on different criteria
VALID_LAYOUTS=("fr")
#VALID_LAYOUTS=("fr" "kk" "circle" "grid" "sphere" "random" "drl" "graphopt")

# List of valid color palette
# Note: Change color schemes based on different criteria
VALID_PALETTES=(1)
#VALID_PALETTES=(1 2 3 4 5)

# List of valid shape options
# Note: Change node shapes based on different criteria
VALID_SHAPE_OPTIONS=(1)
#VALID_SHAPE_OPTIONS=(1 2 3)

# List of valid size types
# Note: Change node sizes based on different criteria
VALID_SIZE_TYPES=("equals" "elite_out_degree")
#VALID_SIZE_TYPES=("equals" "configurations" "out_degree" "in_degree" "degree" "elite_out_degree")

# List of all valid combinations: "SHOW_REGULAR SHOW_START_REGULAR SHOW_SINGLE_NODES"
# Note: SHOW_REGULAR = FALSE implies SHOW_START_REGULAR = FALSE (no regular nodes to show)
# Note 2: SHOW_SINGLE_NODES = FALSE implies SHOW_START_REGULAR = FALSE (no single nodes to show)
# Therefore: SHOW_START_REGULAR = TRUE only when SHOW_REGULAR = TRUE AND SHOW_SINGLE_NODES = TRUE
# VALID_SHOW_PARAMETERS=(
#   "FALSE FALSE TRUE"
#   "FALSE FALSE FALSE"
#   "TRUE FALSE TRUE"
#   "TRUE FALSE FALSE"
#   "TRUE TRUE TRUE"
# )
VALID_SHOW_PARAMETERS=(
  "FALSE FALSE FALSE"
  "FALSE FALSE TRUE"
)

# List of valid zoom quantiles (NA means no zoom)
# Note: Zooming in may hide some nodes if they are outside the quantile range (based on quality)
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
        echo "    Skipping: Input file not found: $input_file" | tee -a "$LOG_FILE"
        continue
      fi

      # Loop over valid options
      for palette in "${VALID_PALETTES[@]}"; do
        for layout in "${VALID_LAYOUTS[@]}"; do
          for shape_option in "${VALID_SHAPE_OPTIONS[@]}"; do
            for size_type in "${VALID_SIZE_TYPES[@]}"; do
              for show_params in "${VALID_SHOW_PARAMETERS[@]}"; do
                # Extract individual parameters from the combination
                read -r show_regular show_start_regular show_single_nodes <<< "$show_params"
                
                for zoom_quantile in "${VALID_ZOOM_QUANTILES[@]}"; do
                  output_dir="${base_output_dir}/${loc}"
                  mkdir -p "$output_dir"
                  
                  output_file="${exp}-${loc}-P_${palette}-L_${layout}-W_${shape_option}-T_${size_type}-SR_${show_regular}-SSR_${show_start_regular}-SN_${show_single_nodes}-Z_${zoom_quantile}.pdf"

                  run_plot_stn_i \
                    -i "$input_file" \
                    -o "$output_dir" \
                    -f "$output_file" \
                    -l "$layout" \
                    -r "$show_regular" \
                    -s "$show_start_regular" \
                    -n "$show_single_nodes" \
                    -w "$shape_option" \
                    -t "$size_type" \
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
done

echo "=== STN-i plot generation finished at $(date) ===" >> "$LOG_FILE"
