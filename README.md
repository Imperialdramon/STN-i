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

## Location Code Generation Algorithm

The `get_location_code()` function in `R/utils.R` generates unique identifiers (location codes) for configurations by discretizing their parameter values into subranges. This is the mathematical foundation for node grouping in STN-i.

### Core Concept

Each configuration is mapped to a **location code** by processing its parameters according to their type (categorical, integer, or real). The location code uniquely identifies a "location" in the parameter space, which corresponds to a node in the STN-i graph.

### Numeric Parameter Bucketing (Integer & Real)

For numeric parameters (integer or real types), we create **discrete buckets** using subrange indices:

#### Subrange Index Calculation

Given a parameter value `v` in the range `[lower_bound, upper_bound]` with step size `step`:

$$
\text{subrange\_index} = \left\lfloor \frac{v - \text{lower\_bound}}{\text{step}} \right\rfloor
$$

This formula assigns each value to a bucket `[k \cdot step, (k+1) \cdot step)` where `k = subrange_index`.

**Example:** For `inertia` with range `[0.0, 0.9]` and `step=0.01`:
- Value `0.045` → `subrange_index = floor((0.045 - 0) / 0.01) = 4` → bucket `[0.04, 0.05)`
- Value `0.049` → `subrange_index = floor((0.049 - 0) / 0.01) = 4` → bucket `[0.04, 0.05)` (same bucket)
- Value `0.050` → `subrange_index = floor((0.050 - 0) / 0.01) = 5` → bucket `[0.05, 0.06)` (different bucket)

#### Scaled Integer Arithmetic

To avoid floating-point precision issues, we perform all calculations using **integer arithmetic**:

$$
\text{scale} = 10^{\text{significance}}
$$

$$
\text{scaled\_lower} = \text{round}(\text{lower\_bound} \times \text{scale})
$$

$$
\text{scaled\_upper} = \text{round}(\text{upper\_bound} \times \text{scale})
$$

$$
\text{scaled\_step} = \text{round}(\text{step} \times \text{scale})
$$

$$
\text{scaled\_value} = \text{round}(v \times \text{scale})
$$

#### Subrange Index in Scaled Space

$$
\text{subrange\_index} = \left\lfloor \frac{\text{scaled\_value} - \text{scaled\_lower}}{\text{scaled\_step}} \right\rfloor
$$

With clamping to ensure the index stays within valid bounds:

$$
\text{subrange\_index} = \min\left(\max(\text{subrange\_index}, 0), \left\lfloor \frac{\text{scaled\_upper} - \text{scaled\_lower}}{\text{scaled\_step}} \right\rfloor\right)
$$

#### Code Generation for Numeric Parameters

1. Calculate the **starting value of the bucket**:
$$
\text{calculated\_scaled} = \text{scaled\_lower} + \text{subrange\_index} \times \text{scaled\_step}
$$

2. Determine **digit padding** (to maintain fixed-width formatting):
$$
\text{max\_digits} = \text{len}(\text{str}(\text{scaled\_upper}))
$$
$$
\text{current\_digits} = \text{len}(\text{str}(\text{calculated\_scaled}))
$$
$$
\text{padding} = \text{max\_digits} - \text{current\_digits}
$$

3. Format as code part:
$$
\text{code\_part} = \text{"0"} \times \text{padding} + \text{str}(\text{calculated\_scaled})
$$

**Example:** For `inertia` with `lower_bound=0.0`, `upper_bound=0.9`, `step=0.01`, `significance=2`:

```
Scaled values:
  scaled_lower = round(0.0 × 10²) = 0
  scaled_upper = round(0.9 × 10²) = 90
  scaled_step = round(0.01 × 10²) = 1

For value v = 0.45:
  scaled_value = round(0.45 × 10²) = 45
  subrange_index = floor((45 - 0) / 1) = 45
  calculated_scaled = 0 + 45 × 1 = 45
  max_digits = len("90") = 2
  current_digits = len("45") = 2
  padding = 2 - 2 = 0
  code_part = "45"

For value v = 0.05:
  scaled_value = round(0.05 × 10²) = 5
  subrange_index = floor((5 - 0) / 1) = 5
  calculated_scaled = 0 + 5 × 1 = 5
  max_digits = len("90") = 2
  current_digits = len("5") = 1
  padding = 2 - 1 = 1
  code_part = "05"
```

### Categorical & Ordinal Parameters

For categorical and ordinal parameters, a **location dictionary** maps each discrete value to an integer code:

$$
\text{code} = \text{location\_dict}[\text{value}]
$$

The code is padded with leading zeros to match the maximum code width:

$$
\text{max\_code\_width} = \text{len}(\text{str}(\max(\text{location\_dict values})))
$$

**Example:** For `topology` with mapping `{0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6}`:

```
value = 5 → code_dict["5"] = 5 → max_width = 1 → code_part = "5"
value = 2 → code_dict["2"] = 2 → max_width = 1 → code_part = "2"
```

### Missing Values (NA)

When a parameter value is `NA` (e.g., a conditional parameter that's not active):

For numeric parameters:
$$
\text{code\_part} = \text{"X"} \times \text{len}(\text{str}(\text{scaled\_upper}))
$$

For categorical parameters:
$$
\text{code\_part} = \text{"X"} \times \text{max\_code\_width}
$$

### Final Location Code

The final location code is the concatenation of all parameter codes in order:

$$
\text{location\_code} = \text{code\_param\_1} + \text{code\_param\_2} + \ldots + \text{code\_param\_n}
$$

**Example:** For a configuration with `topology=1`, `particles=25`, `phi1=0.45`:

```
code_topology = "1"
code_particles = (scaled: 250) = "250"
code_phi1 = (scaled: 45) = "045"

location_code = "1" + "250" + "045" = "1250045"
```

### Granularity Levels (L0-L5)

Different granularity levels use different step sizes, controlling how many configurations map to each node:

- **L0 (finest)**: Smallest steps → More nodes, each representing fewer configurations
- **L5 (coarsest)**: Largest steps → Fewer nodes, each representing more configurations

For example, with `inertia` ∈ `[0.0, 0.9]` and `significance=2`:
- **L0**: `step=0.01` → 90 possible codes (0-89)
- **L1**: `step=0.05` → 18 possible codes (0-17)
- **L5**: `step=0.45` → 2 possible codes (0-1)

This creates a **hierarchical parameter space representation**, allowing analysis at multiple levels of abstraction.

## Aditional Data

### Experiments

For the ACOTSP results we use this locations

| Parámetro     | Condicional | Tipo | Valores           | Equivalencia L1                                   | Equivalencia L2                                   | Equivalencia L3                                   | Equivalencia L4                                   | Equivalencia L5                                   |
|---------------|-------------|------|-------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|
| algorithm     | FALSE       | c    | {as, mmas, eas, ras, acs} | {as:0, mmas:1, eas:2, ras:3, acs:4} | {as:0, mmas:1, eas:2, ras:3, acs:4} | {as:0, mmas:1, eas:2, ras:3, acs:4} | {as:0, mmas:1, eas:2, ras:3, acs:4} | {as:0, mmas:1, eas:2, ras:3, acs:4} |
| localsearch   | FALSE       | c    | {0, 1, 2, 3}      | {0:0, 1:1, 2:2, 3:3} | {0:0, 1:1, 2:2, 3:3} | {0:0, 1:1, 2:2, 3:3} | {0:0, 1:1, 2:2, 3:3} | {0:0, 1:1, 2:2, 3:3} |
| alpha         | FALSE       | r    | (0.00, 5.00)      | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| beta          | FALSE       | r    | (0.00, 10.00)     | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| rho           | FALSE       | r    | (0.01, 1.00)      | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| ants          | FALSE       | i    | (5, 100)          | step=5, significance=0 | step=15, significance=0 | step=25, significance=0 | step=35, significance=0 | step=45, significance=0 |
| q0            | TRUE        | r    | (0.0, 1.0)        | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| rasrank       | TRUE        | i    | (1, 100)          | step=5, significance=0 | step=15, significance=0 | step=25, significance=0 | step=35, significance=0 | step=45, significance=0 |
| elitistants   | TRUE        | i    | (1, 750)          | step=5, significance=0 | step=15, significance=0 | step=25, significance=0 | step=35, significance=0 | step=45, significance=0 |
| nnls          | TRUE        | i    | (5, 50)           | step=5, significance=0 | step=15, significance=0 | step=25, significance=0 | step=35, significance=0 | step=45, significance=0 |
| dlb           | TRUE        | c    | {0, 1}            | {0:0, 1:1} | {0:0, 1:1} | {0:0, 1:1} | {0:0, 1:1} | {0:0, 1:1} |

For the PSO-X results we use this locations

| Parámetro    | Condicional | Tipo | Valores           | Equivalencia L1                                   | Equivalencia L2                                   | Equivalencia L3                                   | Equivalencia L4                                   | Equivalencia L5                                   |
|---------------|-------------|------|-------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|--------------------------------------------------|
| particles     | FALSE       | i    | (2, 200)          | step=5, significance=0 | step=15, significance=0 | step=25, significance=0 | step=35, significance=0 | step=45, significance=0 |
| topology      | FALSE       | c    | {0, 1, 2, 3, 4, 5, 6} | {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6} | {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6} | {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6} | {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6} | {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6} |
| modInfluence  | FALSE       | c    | {0, 1, 2}         | {0:0, 1:1, 2:2} | {0:0, 1:1, 2:2} | {0:0, 1:1, 2:2} | {0:0, 1:1, 2:2} | {0:0, 1:1, 2:2} |
| branching     | TRUE        | i    | (4, 20)           | step=5, significance=0 | step=15, significance=0 | step=17, significance=0 | step=17, significance=0 | step=17, significance=0 |
| tSchedule     | TRUE        | i    | (2, 10)           | step=5, significance=0 | step=8, significance=0 | step=8, significance=0 | step=8, significance=0 | step=8, significance=0 |
| phi1          | FALSE       | r    | (0.00, 2.50)      | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| phi2          | FALSE       | r    | (0.00, 2.50)      | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
| inertia       | FALSE       | r    | (0.00, 0.90)      | step=0.05, significance=2 | step=0.15, significance=2 | step=0.25, significance=2 | step=0.35, significance=2 | step=0.45, significance=2 |
