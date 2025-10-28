#!/bin/bash

# ==============================================================================
# Script: aggregate_all_metrics_STN-i.sh
# Description: Aggregates ALL individual experiment metrics into one file
#              per algorithm, stored in Others/All_Metrics.csv
#
# Output:
#   Experiments/<Algorithm>/Others/All_Metrics.csv
# ==============================================================================

# Define algorithms to process
algorithms=("ACOTSP" "PSO-X")

# Define experiments per algorithm
declare -A experiments
experiments["ACOTSP"]="BH BH-90 BL BL-45"
experiments["PSO-X"]="BH BH-65 BL BL-32"

# Define levels for each algorithm
levels="L1 L2 L3 L4 L5"

# Process each algorithm
for alg in "${algorithms[@]}"; do
    echo "Processing $alg..."
    
    # Create Others directory if it doesn't exist
    others_dir="Experiments/$alg/Others"
    mkdir -p "$others_dir"
    
    # Initialize/clear the output file with empty content
    output_file="$others_dir/All_Metrics.csv"
    > "$output_file"
    header_written=false
    
    # Process each experiment type
    for exp in ${experiments[$alg]}; do
        echo "  Processing experiment: $exp"
        
        # Process each level
        for lvl in $levels; do
            metrics_file="Experiments/$alg/Individuals/$exp/STN-i-Metrics/${exp}-${lvl}_stn_i_metrics.csv"
            
            if [[ -f "$metrics_file" ]]; then
                if ! $header_written; then
                    # Write header only once
                    head -n 1 "$metrics_file" > "$output_file"
                    header_written=true
                fi
                # Append data
                tail -n +2 "$metrics_file" >> "$output_file"
                echo "    ✅ Added metrics from: $metrics_file"
            else
                echo "    ⚠️ Missing file: $metrics_file"
            fi
        done
    done
    
    echo "✅ Completed $alg - Output saved to: $output_file"
done

echo "🎉 All metrics have been aggregated successfully!"