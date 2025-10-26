# Search Trajectory Network for irace (STN-i)

This repository contains the implementation of Search Trajectory Networks (STN) adapted for the irace algorithm. STN-i allows visualization and analysis of the search behavior of irace through network representations.

## Table of Contents
- [Overview](#overview)
- [Project Structure](#project-structure)
- [Scripts Description](#scripts-description)
- [Usage Examples](#usage-examples)
- [Automation Scripts](#automation-scripts)

## Overview

STN-i creates network representations where:
- Nodes represent configurations evaluated by irace
- Edges represent transitions between configurations
- Node attributes include fitness values and configuration types
- Edge attributes include transition frequencies

The project supports:
- Individual STN-i generation for single irace executions
- Merged STN-i creation from multiple executions
- Network visualization with various layout algorithms
- Metric calculation for network analysis

## Project Structure

```
STN-i/
├── R/                      # R scripts
├── Experiments/           
│   ├── ACOTSP/            # ACOTSP algorithm data
│   │   ├── Individuals/   # Individual executions
│   │   ├── Locations/     # Location files
│   │   ├── Multiple/      # Merged networks
│   │   └── Others/        # Additional data
│   └── PSO-X/             # PSO algorithm data
├── Authomatize/           # Automation scripts
└── Logs/                  # Execution logs
```

## Scripts Description

### Data Generation Scripts

#### `get_elites.R`
Extracts and processes elite configurations from multiple irace executions.

```bash
Rscript R/get_elites.R \
  --directories=<dir1,dir2,...> \
  --output=<output_dir> \
  --name=<output_name> \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -d    | --directories | Comma-separated list of directories containing .Rdata files | - | Yes |
| -o    | --output | Directory where output files will be saved | - | Yes |
| -n    | --name | Base name for output files | - | Yes |
| -v    | --verbose | Show detailed processing information | FALSE | No |

The script generates two output files:
1. `<name>_configs.txt`: Tab-separated file containing unique elite configurations
2. `<name>_mapping.csv`: CSV file mapping runs to configurations

#### `generate_STN-i_file.R`
Generates STN-i files from irace execution data.

```bash
Rscript R/generate_STN-i_file.R \
  --input=<input_folder> \
  --output=<output_folder> \
  [--problem_type=<min|max>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter  | Description                                  | Default | Required |
|-------|----------------|----------------------------------------------|---------|----------|
| -i    | --input        | Path to irace execution results folder       | -       | Yes      |
| -o    | --output       | Path where STN-i files will be saved        | -       | Yes      |
| -p    | --problem_type | Optimization type (min/max)                 | min     | No       |
| -v    | --verbose      | Show detailed processing information        | FALSE   | No       |

#### `generate_elite_STN-i_file.R`
Processes elite configurations from irace testing results and generates STN-i files for each scenario. This script filters testing data to include only elite configurations from each run.

```bash
Rscript R/generate_elite_STN-i_file.R \
  --elites_dir=<elites_dir> \
  --directories=<dir1,dir2,...> \
  --output=<output_dir> \
  --parameters=<params_file> \
  [--best=<min|max>] \
  [--na_ranking=TRUE|FALSE] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -e    | --elites_dir  | Directory containing elite data (irace.Rdata with testing results) | - | Yes |
| -d    | --directories | Comma-separated list of scenario directories with .Rdata files | - | Yes |
| -o    | --output      | Output directory for elite STN-i files | - | Yes |
| -p    | --parameters  | CSV file with parameters definition | - | Yes |
| -b    | --best        | Criterion for best value ('min' or 'max') | min | No |
| -n    | --na_ranking  | Consider NA as worst value in rankings | FALSE | No |
| -v    | --verbose     | Show detailed processing information | FALSE | No |

The script generates four files per run:
1. `configurations.csv`: Elite configurations with parameter values
2. `instances.csv`: Testing instances with seeds and optimal values
3. `results.csv`: Performance metrics for elite configurations
4. `trajectories.csv`: Evolution paths between elite configurations

**Usage Examples:**

ACOTSP case:
```bash
Rscript R/generate_elite_STN-i_file.R \
  -e "Experiments/ACOTSP/Individuals-Elites/General-Data" \
  -d "Experiments/ACOTSP/Individuals/BH/Data,Experiments/ACOTSP/Individuals/BH-90/Data,Experiments/ACOTSP/Individuals/BL/Data,Experiments/ACOTSP/Individuals/BL-45/Data" \
  -o "Experiments/ACOTSP/Individuals-Elites" \
  -p "Experiments/ACOTSP/Others/Parameters.csv" \
  -b "min" \
  -v FALSE
```

PSO-X case:
```bash
Rscript R/generate_elite_STN-i_file.R \
  -e "Experiments/PSO-X/Individuals-Elites/General-Data" \
  -d "Experiments/PSO-X/Individuals/BH/Data,Experiments/PSO-X/Individuals/BH-65/Data,Experiments/PSO-X/Individuals/BL/Data,Experiments/PSO-X/Individuals/BL-32/Data" \
  -o "Experiments/PSO-X/Individuals-Elites" \
  -p "Experiments/PSO-X/Others/Parameters.csv" \
  -b "min" \
  -v FALSE
```

#### `generate_STN-i_Rdata.R`
Creates R data objects from STN-i files.

```bash
Rscript R/generate_STN-i_Rdata.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--problem_type=<min|max>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter  | Description                                  | Default | Required |
|-------|----------------|----------------------------------------------|---------|----------|
| -i    | --input        | Path to STN-i file (.csv)                   | -       | Yes      |
| -o    | --output       | Path where RData files will be saved        | -       | Yes      |
| -p    | --problem_type | Optimization type (min/max)                 | min     | No       |
| -v    | --verbose      | Show detailed processing information        | FALSE   | No       |

#### `generate_merged_STN-i_Rdata.R`
Merges multiple STN-i networks into a single unified network.

```bash
Rscript R/generate_merged_STN-i_Rdata.R \
  --input=<input_folder> \
  --output=<output_folder> \
  [--output_file=<file_name>] \
  [--criteria=<mean|min|max|median|mode>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter  | Description                                  | Default | Required |
|-------|----------------|----------------------------------------------|---------|----------|
| -i    | --input        | Folder containing STN-i RData files         | -       | Yes      |
| -o    | --output       | Path where merged network will be saved     | -       | Yes      |
| -f    | --output_file  | Name for the output file                    | auto    | No       |
| -c    | --criteria     | Method for resolving node conflicts         | mean    | No       |
| -v    | --verbose      | Show detailed processing information        | FALSE   | No       |

### Visualization Scripts

#### `plot_STN-i.R`
Generates visual representations of individual STN-i networks.

```bash
Rscript R/plot_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<file_name>] \
  [--layout_type=<layout>] \
  [--show_regular=TRUE|FALSE] \
  [--show_start_regular=TRUE|FALSE] \
  [--size_factor=<value>] \
  [--palette=<1-5>] \
  [--zoom_quantile=<0-1>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter      | Description                              | Default | Required |
|-------|-------------------|------------------------------------------|---------|----------|
| -i    | --input           | Path to STN-i RData file                | -       | Yes      |
| -o    | --output          | Path where plots will be saved          | -       | Yes      |
| -f    | --output_file     | Name for the output PDF file            | auto    | No       |
| -l    | --layout_type     | Network layout algorithm                | fr      | No       |
| -r    | --show_regular    | Include regular nodes                   | TRUE    | No       |
| -s    | --show_start_regular | Include start regular nodes          | FALSE   | No       |
| -z    | --size_factor     | Node/edge size scaling                  | 1       | No       |
| -p    | --palette         | Color scheme (1-5)                      | 1       | No       |
| -q    | --zoom_quantile   | Focus on network portion (0-1)          | NA      | No       |
| -v    | --verbose         | Show detailed processing information    | FALSE   | No       |

Available layout types: fr, kk, circle, grid, sphere, random, drl, graphopt

#### `plot_merged_STN-i.R`
Visualizes merged STN-i networks with additional features.

```bash
Rscript R/plot_merged_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<file_name>] \
  [--layout_type=<layout>] \
  [--show_regular=TRUE|FALSE] \
  [--show_start_regular=TRUE|FALSE] \
  [--show_shared_regular=TRUE|FALSE] \
  [--show_shared_mixed=TRUE|FALSE] \
  [--size_factor=<value>] \
  [--palette=<1-5>] \
  [--zoom_quantile=<0-1>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter    | Description                             | Default | Required |
|-------|-------------------|-----------------------------------------|---------|----------|
| -i    | --input           | Path to merged STN-i RData file         | -       | Yes      |
| -o    | --output          | Path where plots will be saved          | -       | Yes      |
| -f    | --output_file     | Name for the output PDF file            | auto    | No       |
| -l    | --layout_type     | Network layout algorithm                | fr      | No       |
| -r    | --show_regular    | Include regular nodes                   | TRUE    | No       |
| -s    | --show_start_regular | Include start regular nodes          | FALSE   | No       |
| -g    | --show_shared_regular | Show shared regular nodes           | TRUE    | No       |
| -m    | --show_shared_mixed | Show shared mixed nodes               | TRUE    | No       |
| -z    | --size_factor     | Node/edge size scaling                  | 1       | No       |
| -p    | --palette         | Color scheme (1-5)                      | 1       | No       |
| -q    | --zoom_quantile   | Focus on network portion (0-1)          | NA      | No       |
| -v    | --verbose         | Show detailed processing information    | FALSE   | No       |

### Metric Calculation Scripts

#### `metrics_STN-i.R`
Calculates network metrics for individual STN-i networks.

```bash
Rscript R/metrics_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<file_name>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter | Description                                 | Default | Required |
|-------|----------------|---------------------------------------------|---------|----------|
| -i    | --input        | Path to STN-i RData file                    | -       | Yes      |
| -o    | --output       | Path where metrics will be saved            | -       | Yes      |
| -f    | --output_file  | Name for the output CSV file                | auto    | No       |
| -v    | --verbose      | Show detailed processing information        | FALSE   | No       |

The following metrics are calculated for each STN-i network:

##### Network Structure Metrics
| Metric Name | Description | Type | Range | Can be NA | Notes |
|-------------|-------------|------|--------|-----------|-------|
| nodes | Total number of configurations in the network | Integer | [1, ∞) | No | |
| regular_nodes | Number of non-elite configurations | Integer | [0, nodes] | No | |
| elite_nodes | Number of elite configurations | Integer | [0, nodes] | No | nodes = regular_nodes + elite_nodes |
| start_nodes | Number of initial configurations | Integer | [0, nodes] | No | |
| standard_nodes | Number of intermediate configurations | Integer | [0, nodes] | No | |
| end_nodes | Number of final configurations | Integer | [0, nodes] | No | nodes = start_nodes + standard_nodes + end_nodes |
| edges | Total number of transitions between configurations | Integer | [0, ∞) | No | |
| worsening_edges | Number of transitions to worse configurations | Integer | [0, edges] | No | |
| equal_edges | Number of transitions to equal quality configurations | Integer | [0, edges] | No | |
| improving_edges | Number of transitions to better configurations | Integer | [0, edges] | No | edges = worsening + equal + improving |

##### Configuration Type Metrics
| Metric Name | Description | Type | Range | Can be NA | Notes |
|-------------|-------------|------|--------|-----------|-------|
| regular_start_nodes | Number of non-elite initial configurations | Integer | [0, start_nodes] | No | |
| elite_start_nodes | Number of elite initial configurations | Integer | [0, start_nodes] | No | start_nodes = regular_start + elite_start |
| regular_start_configuration_rate | Proportion of initial configurations that are non-elite | Float | [0, 1] | No | regular_start_nodes/start_nodes |
| elite_start_configuration_rate | Proportion of initial configurations that are elite | Float | [0, 1] | No | elite_start_nodes/start_nodes |
| regular_configuration_rate | Overall proportion of non-elite configurations | Float | [0, 1] | No | regular_nodes/nodes |
| elite_configuration_rate | Overall proportion of elite configurations | Float | [0, 1] | No | elite_nodes/nodes |
| best_nodes | Number of configurations with best performance | Integer | [1, nodes] | No | |
| start_best_nodes | Number of initial configurations with best performance | Integer | [0, best_nodes] | No | |
| standard_best_nodes | Number of intermediate configurations with best performance | Integer | [0, best_nodes] | No | |
| end_best_nodes | Number of final configurations with best performance | Integer | [0, best_nodes] | No | |

##### Connectivity Metrics
| Metric Name | Description | Type | Range | Can be NA | Notes |
|-------------|-------------|------|--------|-----------|-------|
| average_degree | Average number of connections per node | Float | [0, ∞) | No | |
| average_in_degree | Average number of incoming connections | Float | [0, ∞) | No | |
| average_out_degree | Average number of outgoing connections | Float | [0, ∞) | No | |
| average_regular_in_degree | Average incoming connections for non-elite nodes | Float | [0, ∞) | No | |
| average_regular_out_degree | Average outgoing connections for non-elite nodes | Float | [0, ∞) | No | |
| average_elite_in_degree | Average incoming connections for elite nodes | Float | [0, ∞) | No | |
| average_elite_out_degree | Average outgoing connections for elite nodes | Float | [0, ∞) | No | |
| average_best_in_degree | Average incoming connections for best nodes | Float | [0, ∞) | No | |
| average_best_out_degree | Average outgoing connections for best nodes | Float | [0, ∞) | No | |
| best_strength_in | Average weight of incoming connections to best nodes | Float | [0, ∞) | Yes | NA if no incoming connections |

##### Path and Component Metrics
| Metric Name | Description | Type | Range | Can be NA | Notes |
|-------------|-------------|------|--------|-----------|-------|
| average_path_length | Average number of steps between connected nodes | Float | [0, ∞) | Yes | NA if disconnected |
| paths | Number of unique paths in the network | Integer | [0, ∞) | No | |
| components | Number of connected components | Integer | [1, nodes] | No | |
| average_configurations_survival_rates | Average survival rate of configurations | Float | [0, 1] | No | |

##### Rate Metrics
| Metric Name | Description | Type | Range | Can be NA | Notes |
|-------------|-------------|------|--------|-----------|-------|
| best_nodes_rate | Proportion of best performing configurations | Float | [0, 1] | No | best_nodes/nodes |
| regular_nodes_rate | Proportion of non-elite configurations | Float | [0, 1] | No | regular_nodes/nodes |
| elite_nodes_rate | Proportion of elite configurations | Float | [0, 1] | No | elite_nodes/nodes |
| start_nodes_rate | Proportion of initial configurations | Float | [0, 1] | No | start_nodes/nodes |
| standard_nodes_rate | Proportion of intermediate configurations | Float | [0, 1] | No | standard_nodes/nodes |
| end_nodes_rate | Proportion of final configurations | Float | [0, 1] | No | end_nodes/nodes |
| worsening_edges_rate | Proportion of transitions to worse configurations | Float | [0, 1] | No | worsening_edges/edges |
| equal_edges_rate | Proportion of transitions to equal configurations | Float | [0, 1] | No | equal_edges/edges |
| improving_edges_rate | Proportion of transitions to better configurations | Float | [0, 1] | No | improving_edges/edges |


#### `metrics_merged_STN-i.R`
Computes metrics for merged networks with additional shared node statistics.

```bash
Rscript R/metrics_merged_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<file_name>] \
  [--verbose=TRUE|FALSE]
```

| Short | Long Parameter  | Description                                  | Default | Required |
|-------|----------------|----------------------------------------------|---------|----------|
| -i    | --input        | Path to merged STN-i RData file             | -       | Yes      |
| -o    | --output       | Path where metrics will be saved            | -       | Yes      |
| -f    | --output_file  | Name for the output CSV file                | auto    | No       |
| -v    | --verbose      | Show detailed processing information        | FALSE   | No       |

Additional metrics over metrics_STN-i.R:
- Shared node statistics
- Inter-network connectivity measures
- Algorithm comparison metrics

On work...

## Usage Examples

Here's a complete workflow example using the ACOTSP data included in the repository:

1. Generate STN-i file from ACOTSP results:
```bash
Rscript R/generate_STN-i_file.R \
  --input=Experiments/ACOTSP/BH-90/Data \
  --output=Experiments/ACOTSP/BH-90/STN-i-Files \
  --problem_type=min
```

2. Create RData object:
```bash
Rscript R/generate_STN-i_Rdata.R \
  --input=Experiments/ACOTSP/BH-90/STN-i-Files/BH-90-L1.csv \
  --output=Experiments/ACOTSP/BH-90/STN-i-RData
```

3. Generate visualization:
```bash
Rscript R/plot_STN-i.R \
  --input=Experiments/ACOTSP/BH-90/STN-i-RData/BH-90-L1_stn_i.Rdata \
  --output=Experiments/ACOTSP/BH-90/STN-i-Plots \
  --layout_type=fr
```

4. Calculate metrics:
```bash
Rscript R/metrics_STN-i.R \
  --input=Experiments/ACOTSP/BH-90/STN-i-RData/BH-90-L1_stn_i.Rdata \
  --output=Experiments/ACOTSP/BH-90/STN-i-Metrics
```

5. Merge multiple networks:
```bash
Rscript R/generate_merged_STN-i_Rdata.R \
  --input=Experiments/ACOTSP/BH_BH-90/STN-i-RData \
  --output=Experiments/ACOTSP/BH_BH-90/Merged-STN-i-RData
```

## Automation Scripts

The repository includes scripts to automate common workflows:

### `run_all_generate_STN-i_file.sh`
Generates STN-i files for all instances in a specified algorithm's data folder.

### `run_all_generate_STN-i_Rdata.sh`
Creates RData objects for all STN-i files in the specified algorithm folders.

### `run_all_metrics_STN-i.sh`
Calculates metrics for all STN-i RData files in the specified algorithm folders.

### `aggregate_all_metrics_STN-i.sh`
Aggregates all metrics from individual experiments into a single CSV file per algorithm.

```bash
./Authomatize/aggregate_all_metrics_STN-i.sh
```

The script will:
- Process each configured algorithm (ACOTSP, PSO-X)
- For each experiment type (BH, BH-90, BL, BL-45, etc.)
- Combine metrics from all levels (L1-L5)
- Generate a single `All_Metrics.csv` file in the algorithm's Others directory

### `run_all_generate_STN-i_Rdata.sh`
Creates RData objects for all STN-i files in the specified algorithm folders.

These scripts should be run from the repository root and require proper folder structure as shown in the Project Structure section.
