#!/bin/bash

# ==============================================================================
# Script: aggregate_all_metrics_STN-i.sh
# Description: Aggregates ALL individual experiment metrics into three files
#              per algorithm, stored in Others/:
#              - All_Metrics.csv (complete metrics)
#              - All_Metrics_nodes.csv (node-focused metrics)
#              - All_Metrics_configurations.csv (configuration-focused metrics)
#
# Usage:
#   1. Run with Individuals (default):
#        ./aggregate_all_metrics_STN-i.sh
#
#   2. Run with Individuals-Elites:
#        ./aggregate_all_metrics_STN-i.sh --mode=Individuals-Elites
#
# Output:
#   Experiments/<Algorithm>/Others/All_Metrics.csv
#   Experiments/<Algorithm>/Others/All_Metrics_nodes.csv
#   Experiments/<Algorithm>/Others/All_Metrics_configurations.csv
#   (or with _Elites suffix if using Individuals-Elites mode)
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

# Define algorithms to process
algorithms=("ACOTSP" "PSO-X")

# Define experiments per algorithm
declare -A experiments
experiments["ACOTSP"]="BH BH-90 BL BL-45"
experiments["PSO-X"]="BH BH-65 BL BL-32"

# Define levels for each algorithm
levels="L0 L1 L2 L3 L4 L5"

# Metric file types to aggregate
metric_types=("" "_nodes" "_configurations")

# Determine output suffix based on mode
if [[ "$MODE" == "Individuals-Elites" ]]; then
  output_suffix="_Elites"
else
  output_suffix=""
fi

# Process each algorithm
for alg in "${algorithms[@]}"; do
    echo "Processing $alg ($MODE)..."
    
    # Create Others directory if it doesn't exist
    others_dir="Experiments/$alg/Others"
    mkdir -p "$others_dir"
    
    # Process each metric type (complete, nodes, configurations)
    for metric_type in "${metric_types[@]}"; do
        # Define output file based on metric type
        if [[ -z "$metric_type" ]]; then
            output_file="$others_dir/All_Metrics${output_suffix}.csv"
            file_suffix="_stn_i_metrics.csv"
            echo "  Processing complete metrics..."
        elif [[ "$metric_type" == "_nodes" ]]; then
            output_file="$others_dir/All_Metrics${output_suffix}_nodes.csv"
            file_suffix="_stn_i_metrics_nodes.csv"
            echo "  Processing node-focused metrics..."
        elif [[ "$metric_type" == "_configurations" ]]; then
            output_file="$others_dir/All_Metrics${output_suffix}_configurations.csv"
            file_suffix="_stn_i_metrics_configurations.csv"
            echo "  Processing configuration-focused metrics..."
        fi
        
        # Initialize/clear the output file with empty content
        > "$output_file"
        header_written=false
        
        # Process each experiment type
        for exp in ${experiments[$alg]}; do
            # Process each level
            for lvl in $levels; do
                metrics_file="Experiments/$alg/$MODE/$exp/STN-i-Metrics/${exp}-${lvl}_stn_i_metrics${metric_type}.csv"
                
                if [[ -f "$metrics_file" ]]; then
                    if ! $header_written; then
                        # Write header only once
                        head -n 1 "$metrics_file" > "$output_file"
                        header_written=true
                    fi
                    # Append data
                    tail -n +2 "$metrics_file" >> "$output_file"
                    echo "      ✅ Added: ${exp}-${lvl}${metric_type}"
                else
                    echo "      ⚠️  Missing: ${exp}-${lvl}${metric_type}"
                fi
            done
        done
        
        if [[ -f "$output_file" ]] && [[ -s "$output_file" ]]; then
            echo "    ✅ Saved to: $output_file"
        else
            echo "    ⚠️  Warning: $output_file is empty or was not created"
        fi
    done
    
    echo "✅ Completed $alg ($MODE)"
done

echo "🎉 All metrics have been aggregated successfully!"