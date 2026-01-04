# Search Trajectory Network for irace (STN-i)

This repository contains the implementation of Search Trajectory Networks (STN) adapted for the irace algorithm. STN-i allows visualization and analysis of the search behavior of irace through network representations.

## Table of Contents
- [Overview](#overview)
- [Project Structure](#project-structure)
- [Scripts Description](#scripts-description)
  - [Data Generation Scripts](#data-generation-scripts)
  - [Metric Calculation Scripts](#metric-calculation-scripts)
- [Automation Scripts](#automation-scripts)
- [Location Code Generation Algorithm](#location-code-generation-algorithm)
- [Additional Data](#additional-data)

## Overview

STN-i creates network representations where:
- **Nodes** represent configurations evaluated by irace
- **Edges** represent transitions between configurations
- **Node attributes** include fitness values, elite status, and configuration types
- **Edge attributes** include transition frequencies and improvement types

The project supports:
- Individual STN-i generation for single irace executions
- Network visualization with customizable layouts, colors, shapes, and sizes
- Comprehensive metric calculation for network analysis
- Elite configuration extraction and analysis
- Batch processing through automation scripts

## Project Structure

```
STN-i/
├── R/                      # R scripts for data processing and visualization
│   ├── generate_STN-i_file.R
│   ├── generate_STN-i_Rdata.R
│   ├── plot_STN-i.R
│   ├── metrics_STN-i.R
│   ├── generate_optimum_file.R
│   ├── get_elites.R
│   ├── generate_elite_STN-i_file.R
│   ├── box_plot_best_elite_STN-i.R
│   └── Functions/          # Utility functions
├── Experiments/           
│   ├── ACOTSP/             # ACOTSP algorithm data
│   │   ├── Individuals/    # Individual STN-i per execution
│   │   ├── Individuals-Elites/ # Elite-only STN-i
│   │   ├── General-Metrics/    # Aggregated metrics
│   │   ├── Locations/      # Location parameter files
│   │   └── Others/         # Parameters and optimum files
│   └── PSO-X/              # PSO-X algorithm data
├── Authomatize/            # Bash automation scripts
└── Logs/                   # Execution logs
```

## Quality Metric (QUALITY)

STN-i uses a composite quality metric called **QUALITY** to evaluate and compare configurations across different irace runs. This metric is particularly designed to handle the challenge that configurations are evaluated on different numbers of instances during the training phase.

### QUALITY Formula

```
QUALITY = MNRG × (2 - i/N)
```

Where:
- **MNRG**: Mean Normalized Ranking Gap (range: [0, 1], where 0 = best)
- **i**: Number of instances evaluated for the configuration in the current run
- **N**: Maximum number of instances evaluated across all configurations in the current run
- **(2 - i/N)**: Instance penalty factor (range: [1, 2], where 1 = best, achieved when i=N)

**Result:** QUALITY ranges from [0, 2], where **lower values indicate better configurations**.

### Components Breakdown

#### 1. MNRG (Mean Normalized Ranking Gap)

MNRG is calculated through a four-step process over the experiments matrix (rows = instances, columns = configurations):

##### Step 1: GAP Calculation (Per Instance)
For each instance-configuration pair, calculate the percentage gap from the best value:

```
GAP(instance, config) = 100 × |value(instance, config) - best(instance)| / best(instance)
```

Where `best(instance)` is the optimal value (from Optimum.csv) or the best value found across all configurations for that instance.

##### Step 2: Horizontal Ranking (Per Instance)
For each instance (row), rank all configurations by their GAP values:

```
Rank(instance, config) = rank of GAP(instance, config) among all configs in instance
```

- Best configuration (lowest GAP) gets rank = 1
- Worst configuration gets rank = total number of evaluated configs
- Tied values receive the same rank

**Handling Missing Evaluations (NA values):**

When a configuration was not evaluated on a specific instance (NA in the experiments matrix), the ranking behavior depends on the `is_na_ranking` parameter:

- **`is_na_ranking = FALSE`** (default): NA values are ignored and remain as NA in the ranking matrix. Only configurations actually evaluated on the instance are ranked. This is appropriate when you want to rank based solely on observed performance. **Important:** This approach can implicitly favor configurations eliminated early (with few evaluations), as their MNRG is calculated only from instances where they were evaluated, potentially showing artificially good performance. This is why the QUALITY metric includes the instance penalty factor `(2 - i/N)` to compensate for this effect.

- **`is_na_ranking = TRUE`**: NA values are assigned the worst possible rank: `rank = number_of_evaluated_configs + 1`. This penalizes configurations that were not evaluated (typically because they were eliminated early by irace) by treating their absence as worse than any observed performance.

##### Step 3: Normalization (Per Instance)
Normalize each rank to the range [0, 1]:

```
NormalizedRank(instance, config) = (Rank(instance, config) - 1) / (num_configs - 1)
```

- Rank 1 (best) → 0.0
- Highest rank (worst) → 1.0
- Intermediate ranks → proportional values between 0 and 1

##### Step 4: Vertical Averaging (Per Configuration)
Calculate the mean normalized rank across all instances where the configuration was evaluated:

```
MNRG(config) = mean(NormalizedRank(instance, config) for all instances where config was evaluated)
```

**Result:** MNRG ∈ [0, 1], where 0 represents consistently best performance and 1 represents consistently worst performance.

#### 2. Instance Penalty Factor: (2 - i/N)

This factor penalizes configurations evaluated on fewer instances, which are less reliable:

- **i**: Number of instances where the configuration was evaluated (varies per configuration within each run)
- **N**: Maximum number of instances any configuration was evaluated on in this run (typically the elite configurations see all instances)

**Examples:**
- Configuration evaluated on all instances (i = N): factor = 2 - N/N = 1.0 (no penalty)
- Configuration evaluated on half the instances (i = N/2): factor = 2 - 0.5 = 1.5 (moderate penalty)
- Configuration evaluated on few instances (i → 0): factor → 2.0 (maximum penalty)

**Important:** The values of i and N are **specific to each run**. Different runs may have different N values depending on how many instances irace evaluated during that particular execution.

### Why QUALITY Instead of MNRG Alone?

During irace training:
- **Elite configurations** are evaluated on many instances (high i, close to N)
- **Regular configurations** may be eliminated early, evaluated on few instances (low i)

Using MNRG alone would unfairly compare configurations with different levels of evaluation confidence. The instance penalty factor ensures that:
1. Configurations with more evaluations (more reliable) are preferred when MNRG values are similar
2. The metric remains interpretable: lower is always better
3. Elite configurations (evaluated on all instances) receive no penalty

### When MNRG is Used Directly

In **testing scenarios** (e.g., `generate_summarize_testing.R`), all elite configurations are evaluated on the **same complete set of testing instances**. Since i = N for all configurations, the instance penalty factor becomes 1.0 for everyone, making QUALITY = MNRG. In this case, using MNRG directly is appropriate and avoids unnecessary computation.

## Scripts Description

### Data Generation Scripts

#### `generate_STN-i_file.R`
Processes irace execution data (.Rdata files) and generates STN-i trace files in CSV format. This script handles parameter discretization, configuration grouping by location codes, and trajectory extraction.

**Quality Metric:** The script uses the QUALITY value from results.csv files, which combines the Mean Normalized Ranking Gap (MNRG) with the number of instances evaluated per configuration. This metric (QUALITY = MNRG × (2 - i/N)) properly accounts for configurations evaluated on different numbers of instances during training, where lower values indicate better performance.

**Usage:**
```bash
Rscript R/generate_STN-i_file.R \
  --input=<input_dir> \
  --general_parameters=<general_params_file> \
  --parameters=<params_file_or_dir> \
  --output=<output_dir> \
  [--name=<output_name>] \
  [--best_criteria=<min|max>] \
  [--quality_criteria=<min|max|mean|median|mode>] \
  [--significance=<integer>] \
  [--instances=<instances_file>] \
  [--na_ranking=<TRUE|FALSE>] \
  [--representative_criteria=<min|max|mean|median|mode>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Directory containing input .Rdata files from irace executions | - | Yes |
| -g | --general_parameters | CSV file with general parameters for data processing | - | Yes |
| -p | --parameters | Directory or CSV file with location-specific parameters | - | Yes |
| -o | --output | Output directory for STN-i trace files | - | Yes |
| -n | --name | Name of the output STN-i file | auto | No |
| -b | --best_criteria | Criterion for best value configurations before grouping ('min' or 'max') | min | No |
| -c | --quality_criteria | Quality criterion for aggregating QUALITY values across runs ('min','max','mean','median','mode') | mean | No |
| -s | --significance | Significance level (decimal places) for numerical parameters | 2 | No |
| -t | --instances | CSV file with instance optimum values | NULL | No |
| -k | --na_ranking | Consider NA as worst possible value in rankings | FALSE | No |
| -r | --representative_criteria | Criterion for representative configuration of a location ('min','max','mean','median','mode') | mean | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Complete Example:**
```bash
Rscript R/generate_STN-i_file.R \
  --input="Experiments/ACOTSP/Individuals/BH/Data" \
  --general_parameters="Experiments/ACOTSP/Others/Parameters.csv" \
  --parameters="Experiments/ACOTSP/Locations" \
  --output="Experiments/ACOTSP/Individuals/BH/STN-i-Files" \
  --name="BH" \
  --best_criteria="min" \
  --quality_criteria="mean" \
  --significance=2 \
  --instances="Experiments/ACOTSP/Others/Optimum.csv" \
  --na_ranking=FALSE \
  --representative_criteria="mean" \
  --verbose=TRUE
```

---

#### `generate_STN-i_Rdata.R`
Converts STN-i trace files (CSV) into graph objects (.Rdata) using the igraph package. The resulting graph contains all node and edge attributes needed for visualization and analysis.

**Usage:**
```bash
Rscript R/generate_STN-i_Rdata.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<output_file_name>] \
  [--problem_type=<min|max>] \
  [--best_known_solution=<numeric_value>] \
  [--number_of_runs=<integer_value>] \
  [--network_name=<name>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Path to STN-i trace file (.csv) | - | Yes |
| -o | --output | Output folder for .Rdata files | - | Yes |
| -f | --output_file | Name of the output file | input_name_stn_i.Rdata | No |
| -p | --problem_type | Optimization objective ('min' or 'max') | min | No |
| -b | --best_known_solution | Best-known solution value | computed from data | No |
| -n | --number_of_runs | Number of independent runs to include | max found | No |
| -m | --network_name | Name for the network | input file name | No |

**Complete Example:**
```bash
Rscript R/generate_STN-i_Rdata.R \
  --input="Experiments/ACOTSP/Individuals/BH/STN-i-Files/BH-L0.csv" \
  --output="Experiments/ACOTSP/Individuals/BH/STN-i-RData" \
  --output_file="BH-L0_stn_i.Rdata" \
  --problem_type="min" \
  --best_known_solution=6110 \
  --number_of_runs=20 \
  --network_name="BH-L0"
```

---

#### `plot_STN-i.R`
Generates PDF visualizations of STN-i networks with extensive customization options for layout, colors, shapes, and node sizes.

**Usage:**
```bash
Rscript R/plot_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<output_file_name>] \
  [--layout_type=<value>] \
  [--show_regular=<TRUE|FALSE>] \
  [--show_start_regular=<TRUE|FALSE>] \
  [--show_single_nodes=<TRUE|FALSE>] \
  [--size_factor=<value>] \
  [--palette=<value>] \
  [--shape_option=<value>] \
  [--size_type=<value>] \
  [--zoom_quantile=<value>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Path to STN-i .Rdata file | - | Yes |
| -o | --output | Output folder for plot PDF | - | Yes |
| -f | --output_file | Name of the output PDF file | input_name.pdf | No |
| -l | --layout_type | Layout algorithm: fr, kk, circle, grid, sphere, random, drl, graphopt | fr | No |
| -r | --show_regular | Include regular (non-elite) nodes | TRUE | No |
| -s | --show_start_regular | Include start regular nodes | FALSE | No |
| -n | --show_single_nodes | Include isolated nodes (degree = 0) | TRUE | No |
| -z | --size_factor | Scaling factor for node sizes and edge widths | 1.0 | No |
| -p | --palette | Color scheme (1-5). Palette 1: regular=black, elite=orange, best=red | 1 | No |
| -w | --shape_option | Shape scheme (1-3) for node types | 1 | No |
| -t | --size_type | Size type: equals, configurations, out_degree, in_degree, degree, elite_out_degree | equals | No |
| -q | --zoom_quantile | Focus on best configurations (0-1, where 0.25 = top 25%) | NA | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Shape Options:**
- **Option 1**: Start=square, End=triangle, Standard=diamond, Start-End=cross, Start-Standard=star, Standard-End=ellipse, Start-Standard-End=circle
- **Option 2**: Start=triangle, End=square, Standard=diamond, Start-End=ellipse, Start-Standard=cross, Standard-End=star, Start-Standard-End=circle
- **Option 3**: Start=diamond, End=cross, Standard=star, Start-End=ellipse, Start-Standard=circle, Standard-End=square, Start-Standard-End=triangle

**Size Types:**
- **equals**: All nodes have the same size
- **configurations**: Size proportional to number of times the configuration appears
- **out_degree**: Size proportional to number of outgoing transitions
- **in_degree**: Size proportional to number of incoming transitions
- **degree**: Size proportional to total connections
- **elite_out_degree**: Regular nodes = base size, elite nodes = count of connections to regular nodes

**Complete Example:**
```bash
Rscript R/plot_STN-i.R \
  --input="Experiments/ACOTSP/Individuals/BH/STN-i-RData/BH-L0_stn_i.Rdata" \
  --output="Experiments/ACOTSP/Individuals/BH/STN-i-Plots" \
  --output_file="BH-L0-P_1-L_fr-W_1-T_equals-SR_TRUE-SSR_FALSE-SN_TRUE-Z_NA.pdf" \
  --layout_type="fr" \
  --show_regular=TRUE \
  --show_start_regular=FALSE \
  --show_single_nodes=TRUE \
  --size_factor=1.0 \
  --palette=1 \
  --shape_option=1 \
  --size_type="equals" \
  --zoom_quantile=NA \
  --verbose=FALSE
```

---

### Metric Calculation Scripts

#### `metrics_STN-i.R`
Calculates comprehensive network metrics for STN-i analysis, including node counts, edge types, connectivity, and search efficiency measures.

**Usage:**
```bash
Rscript R/metrics_STN-i.R \
  --input=<input_file> \
  --output=<output_folder> \
  [--output_file=<output_file_name>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Path to STN-i .Rdata file | - | Yes |
| -o | --output | Output folder for metrics CSV | - | Yes |
| -f | --output_file | Name of the output CSV file | input_name_metrics.csv | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Complete Example:**
```bash
Rscript R/metrics_STN-i.R \
  --input="Experiments/ACOTSP/Individuals/BH/STN-i-RData/BH-L0_stn_i.Rdata" \
  --output="Experiments/ACOTSP/Individuals/BH/STN-i-Metrics" \
  --output_file="BH-L0_stn_i_metrics.csv" \
  --verbose=TRUE
```

**Output Files:**

The script generates three complementary metric files, each focusing on different aspects of the STN-i network:

1. **`*_metrics_nodes.csv`**: General network metrics analyzing grouped configurations (nodes in the graph)
   - Node and edge counts by type (regular, elite, start, standard, end)
   - Connectivity metrics (degree distributions, components)
   - Path analysis (average path length to best nodes)
   - Overall network topology characteristics

2. **`*_metrics_elite_nodes.csv`**: Elite subnetwork metrics (subgraph containing only elite nodes)
   - Elite-to-elite connections and transitions
   - Elite-to-regular outgoing edges
   - Elite node topology within the subnetwork
   - Elite-specific connectivity patterns

3. **`*_metrics_configurations.csv`**: Individual configuration metrics (ungrouped, raw configuration counts)
   - Total configurations analyzed across all runs
   - Configuration counts by type before grouping into nodes
   - Distribution statistics (max/min configurations per node)
   - Configuration-level rates and correlations

**Calculated Metrics:**

##### 1. Node Metrics (`*_metrics_nodes.csv`)

General network structure based on grouped configurations (nodes):

| Metric | Description | Type | Range |
|--------|-------------|------|--------|
| network_name | Name of the STN-i network | String | - |
| problem_type | Optimization type (min or max) | String | min/max |
| number_of_runs | Number of independent irace runs | Integer | [1, ∞) |
| best_known_solution | Best known solution value | Float | ℝ |
| components | Number of connected components | Integer | [1, nodes] |
| single_components | Components with single isolated node | Integer | [0, components] |
| multiple_components | Components with multiple connected nodes | Integer | [0, components] |
| nodes | Total number of grouped configurations (locations) | Integer | [1, ∞) |
| regular_nodes | Non-elite configuration groups | Integer | [0, nodes] |
| elite_nodes | Elite configuration groups | Integer | [0, nodes] |
| start_nodes | Initial configuration groups | Integer | [0, nodes] |
| standard_nodes | Intermediate configuration groups | Integer | [0, nodes] |
| end_nodes | Final configuration groups | Integer | [0, nodes] |
| edges | Total number of transitions between nodes | Integer | [0, ∞) |
| worsening_edges | Transitions to worse configurations | Integer | [0, edges] |
| equal_edges | Transitions to equal fitness | Integer | [0, edges] |
| improving_edges | Transitions to better configurations | Integer | [0, edges] |
| max_degree | Maximum total degree (in+out) | Integer | [0, ∞) |
| min_degree | Minimum total degree (in+out) | Integer | [0, ∞) |
| average_degree | Average total degree across all nodes | Float | [0, ∞) |
| average_regular_in_degree | Average in-degree for regular nodes | Float | [0, ∞) |
| average_elite_in_degree | Average in-degree for elite nodes | Float | [0, ∞) |
| average_elite_out_degree | Average out-degree for elite nodes | Float | [0, ∞) |
| best_nodes | Number of nodes with best performance | Integer | [1, nodes] |
| average_best_in_degree | Average in-degree for best nodes | Float | [0, ∞) |
| average_best_out_degree | Average out-degree for best nodes | Float | [0, ∞) |
| best_strength_in | Weighted in-degree for best nodes | Float | [0, ∞) |
| start_best_nodes | Best nodes that are start nodes | Integer | [0, best_nodes] |
| standard_best_nodes | Best nodes that are standard nodes | Integer | [0, best_nodes] |
| end_best_nodes | Best nodes that are end nodes | Integer | [0, best_nodes] |
| average_path_length | Average shortest path from start to best | Float | [0, ∞) |
| paths | Number of paths from start to best nodes | Integer | [0, ∞) |
| best_nodes_rate | Proportion of best nodes | Float | [0, 1] |
| regular_nodes_rate | Proportion of regular nodes | Float | [0, 1] |
| elite_nodes_rate | Proportion of elite nodes | Float | [0, 1] |
| start_nodes_rate | Proportion of start nodes | Float | [0, 1] |
| standard_nodes_rate | Proportion of standard nodes | Float | [0, 1] |
| end_nodes_rate | Proportion of end nodes | Float | [0, 1] |
| worsening_edges_rate | Proportion of worsening edges | Float | [0, 1] |
| equal_edges_rate | Proportion of equal edges | Float | [0, 1] |
| improving_edges_rate | Proportion of improving edges | Float | [0, 1] |

##### 2. Elite Node Metrics (`*_metrics_elite_nodes.csv`)

Subnetwork metrics considering only elite configurations:

| Metric | Description | Type | Range |
|--------|-------------|------|--------|
| network_name | Name of the STN-i network | String | - |
| problem_type | Optimization type (min or max) | String | min/max |
| number_of_runs | Number of independent irace runs | Integer | [1, ∞) |
| best_known_elite_solution | Best known solution among elites | Float | ℝ |
| elite_components | Number of components in elite subgraph | Integer | [0, elite_nodes] |
| single_elite_components | Elite components with single node | Integer | [0, elite_components] |
| multiple_elite_components | Elite components with multiple nodes | Integer | [0, elite_components] |
| elite_nodes | Total elite configuration groups | Integer | [0, ∞) |
| elite_best_nodes | Elite nodes with best performance | Integer | [0, elite_nodes] |
| elite_start_nodes | Elite nodes that are start nodes | Integer | [0, elite_nodes] |
| elite_standard_nodes | Elite nodes that are standard nodes | Integer | [0, elite_nodes] |
| elite_end_nodes | Elite nodes that are end nodes | Integer | [0, elite_nodes] |
| elite_edges | Total edges between elite nodes | Integer | [0, ∞) |
| elite_to_elite_worsening_edges | Worsening transitions between elites | Integer | [0, elite_edges] |
| elite_to_elite_equal_edges | Equal transitions between elites | Integer | [0, elite_edges] |
| elite_to_elite_improving_edges | Improving transitions between elites | Integer | [0, elite_edges] |
| elite_to_regular_worsening_edges | Elite to regular worsening transitions | Integer | [0, ∞) |
| elite_to_regular_equal_edges | Elite to regular equal transitions | Integer | [0, ∞) |
| elite_to_regular_improving_edges | Elite to regular improving transitions | Integer | [0, ∞) |
| max_elite_to_elite_degree | Max degree within elite subgraph | Integer | [0, ∞) |
| min_elite_to_elite_degree | Min degree within elite subgraph | Integer | [0, ∞) |
| average_elite_to_elite_degree | Average degree within elite subgraph | Float | [0, ∞) |
| max_in_elite_to_elite_degree | Max in-degree within elite subgraph | Integer | [0, ∞) |
| min_in_elite_to_elite_degree | Min in-degree within elite subgraph | Integer | [0, ∞) |
| average_in_elite_to_elite_degree | Average in-degree within elite subgraph | Float | [0, ∞) |
| max_out_elite_to_elite_degree | Max out-degree within elite subgraph | Integer | [0, ∞) |
| min_out_elite_to_elite_degree | Min out-degree within elite subgraph | Integer | [0, ∞) |
| average_out_elite_to_elite_degree | Average out-degree within elite subgraph | Float | [0, ∞) |
| max_out_elite_to_regular_degree | Max elite to regular out-degree | Integer | [0, ∞) |
| min_out_elite_to_regular_degree | Min elite to regular out-degree | Integer | [0, ∞) |
| average_out_elite_to_regular_degree | Average elite to regular out-degree | Float | [0, ∞) |
| best_elite_nodes_rate | Proportion of best nodes among elites | Float | [0, 1] |
| start_elite_nodes_rate | Proportion of start nodes among elites | Float | [0, 1] |
| standard_elite_nodes_rate | Proportion of standard nodes among elites | Float | [0, 1] |
| end_elite_nodes_rate | Proportion of end nodes among elites | Float | [0, 1] |
| elite_to_elite_worsening_edges_rate | Proportion of worsening elite-elite edges | Float | [0, 1] |
| elite_to_elite_equal_edges_rate | Proportion of equal elite-elite edges | Float | [0, 1] |
| elite_to_elite_improving_edges_rate | Proportion of improving elite-elite edges | Float | [0, 1] |

##### 3. Configuration Metrics (`*_metrics_configurations.csv`)

Individual configuration counts before grouping into nodes:

| Metric | Description | Type | Range |
|--------|-------------|------|--------|
| network_name | Name of the STN-i network | String | - |
| problem_type | Optimization type (min or max) | String | min/max |
| number_of_runs | Number of independent irace runs | Integer | [1, ∞) |
| best_known_solution | Best known solution value | Float | ℝ |
| configurations | Total individual configurations analyzed | Integer | [1, ∞) |
| regular_configurations | Total regular configurations | Integer | [0, configurations] |
| elite_configurations | Total elite configurations | Integer | [0, configurations] |
| start_configurations | Configurations used as starting points | Integer | [0, configurations] |
| standard_configurations | Configurations in intermediate stages | Integer | [0, configurations] |
| end_configurations | Configurations at final stages | Integer | [0, configurations] |
| regular_start_configurations | Regular configs that are start configs | Integer | [0, configurations] |
| regular_standard_configurations | Regular configs that are standard configs | Integer | [0, configurations] |
| regular_end_configurations | Regular configs that are end configs | Integer | [0, configurations] |
| elite_start_configurations | Elite configs that are start configs | Integer | [0, configurations] |
| elite_standard_configurations | Elite configs that are standard configs | Integer | [0, configurations] |
| elite_end_configurations | Elite configs that are end configs | Integer | [0, configurations] |
| max_configurations | Max configurations grouped into single node | Integer | [1, ∞) |
| min_configurations | Min configurations grouped into single node | Integer | [1, max_configurations] |
| max_regular_configurations | Max regular configs per node | Integer | [0, ∞) |
| min_regular_configurations | Min regular configs per node | Integer | [0, max_regular_configurations] |
| max_elite_configurations | Max elite configs per node | Integer | [0, ∞) |
| min_elite_configurations | Min elite configs per node | Integer | [0, max_elite_configurations] |
| max_start_configurations | Max start configs per node | Integer | [0, ∞) |
| min_start_configurations | Min start configs per node | Integer | [0, max_start_configurations] |
| max_standard_configurations | Max standard configs per node | Integer | [0, ∞) |
| min_standard_configurations | Min standard configs per node | Integer | [0, max_standard_configurations] |
| max_end_configurations | Max end configs per node | Integer | [0, ∞) |
| min_end_configurations | Min end configs per node | Integer | [0, max_end_configurations] |
| max_regular_start_configurations | Max regular start configs per node | Integer | [0, ∞) |
| min_regular_start_configurations | Min regular start configs per node | Integer | [0, max_regular_start_configurations] |
| max_regular_standard_configurations | Max regular standard configs per node | Integer | [0, ∞) |
| min_regular_standard_configurations | Min regular standard configs per node | Integer | [0, max_regular_standard_configurations] |
| max_regular_end_configurations | Max regular end configs per node | Integer | [0, ∞) |
| min_regular_end_configurations | Min regular end configs per node | Integer | [0, max_regular_end_configurations] |
| max_elite_start_configurations | Max elite start configs per node | Integer | [0, ∞) |
| min_elite_start_configurations | Min elite start configs per node | Integer | [0, max_elite_start_configurations] |
| max_elite_standard_configurations | Max elite standard configs per node | Integer | [0, ∞) |
| min_elite_standard_configurations | Min elite standard configs per node | Integer | [0, max_elite_standard_configurations] |
| max_elite_end_configurations | Max elite end configs per node | Integer | [0, ∞) |
| min_elite_end_configurations | Min elite end configs per node | Integer | [0, max_elite_end_configurations] |
| regular_configurations_rate | Proportion of regular configurations | Float | [0, 1] |
| elite_configurations_rate | Proportion of elite configurations | Float | [0, 1] |
| start_configurations_rate | Proportion of start configurations | Float | [0, 1] |
| standard_configurations_rate | Proportion of standard configurations | Float | [0, 1] |
| end_configurations_rate | Proportion of end configurations | Float | [0, 1] |
| regular_start_configurations_rate | Proportion of regular start configurations | Float | [0, 1] |
| regular_standard_configurations_rate | Proportion of regular standard configurations | Float | [0, 1] |
| regular_end_configurations_rate | Proportion of regular end configurations | Float | [0, 1] |
| elite_start_configurations_rate | Proportion of elite start configurations | Float | [0, 1] |
| elite_standard_configurations_rate | Proportion of elite standard configurations | Float | [0, 1] |
| elite_end_configurations_rate | Proportion of elite end configurations | Float | [0, 1] |

---

#### `generate_optimum_file.R`
Processes multiple irace .Rdata files and generates a CSV file with the best quality values for each instance across all executions.

**Usage:**
```bash
Rscript R/generate_optimum_file.R \
  [--input=<input_directory>] \
  [--directories=<dir1,dir2,...>] \
  --output=<output_directory> \
  [--name=<output_file_name>] \
  [--best=<min|max>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Single input directory containing .Rdata files | - | No* |
| -d | --directories | Comma-separated paths to multiple input directories | - | No* |
| -o | --output | Output directory for the optimum CSV file | - | Yes |
| -n | --name | Name of the output CSV file | Optimum.csv | No |
| -b | --best | Criteria for best value ('min' or 'max') | min | No |

*At least one of --input or --directories must be provided.

**Complete Example:**
```bash
Rscript R/generate_optimum_file.R \
  --directories="Experiments/ACOTSP/Individuals/BH/Data,Experiments/ACOTSP/Individuals/BH-90/Data,Experiments/ACOTSP/Individuals/BL/Data,Experiments/ACOTSP/Individuals/BL-45/Data" \
  --output="Experiments/ACOTSP/Others" \
  --name="Optimum.csv" \
  --best="min"
```

---

#### `get_elites.R`
Extracts elite configurations from multiple irace execution scenarios and creates mapping files for elite-focused analysis. Supports filtering to extract only the best elite configuration per run.

**Usage:**
```bash
Rscript R/get_elites.R \
  --directories=<dir1,dir2,...> \
  --output=<output_dir> \
  --name=<output_name> \
  [--parameters=<parameters_file>] \
  [--best_elites=<TRUE|FALSE>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -d | --directories | Comma-separated list of parent directories containing Results subdirectories | - | Yes |
| -o | --output | Directory where output files will be saved | - | Yes |
| -n | --name | Base name for output files | - | Yes |
| -p | --parameters | Path to Parameters.csv file | auto-detect | No |
| -b | --best_elites | Only keep the best elite per run (marked as IS_BEST in configurations.csv) | FALSE | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Best Elite Selection:**

When `--best_elites=TRUE`, the script extracts only the configuration marked as `IS_BEST=TRUE` in each run's `configurations.csv` file. This best configuration is determined by irace as the first element of the last iteration's elite set (stored in `allElites[[last_iteration]][1]`), representing the best performing configuration found during the tuning process.

The script generates two output files:
1. **`<name>_configs.txt`**: Tab-separated file containing unique elite configurations with parameter values
2. **`<name>_mapping.csv`**: CSV file mapping each scenario/run to its elite configuration IDs with ESCENARIO metadata

**Complete Examples:**

Extract all elite configurations (ACOTSP):
```bash
Rscript R/get_elites.R \
  -d "Experiments/ACOTSP/Individuals/BH/Results,Experiments/ACOTSP/Individuals/BH-90/Results,Experiments/ACOTSP/Individuals/BL/Results,Experiments/ACOTSP/Individuals/BL-45/Results" \
  -o "Experiments/ACOTSP/Individuals-Elites/Configurations" \
  -p "Experiments/ACOTSP/Others/Parameters.csv" \
  -n "All_Elites" \
  -v TRUE
```

Extract only the best elite per run (ACOTSP):
```bash
Rscript R/get_elites.R \
  -d "Experiments/ACOTSP/Individuals/BH/Results,Experiments/ACOTSP/Individuals/BH-90/Results,Experiments/ACOTSP/Individuals/BL/Results,Experiments/ACOTSP/Individuals/BL-45/Results" \
  -o "Experiments/ACOTSP/Individuals-Elites/Configurations" \
  -p "Experiments/ACOTSP/Others/Parameters.csv" \
  -n "Best_Elites" \
  -b TRUE \
  -v TRUE
```

Extract all elite configurations (PSO-X):
```bash
Rscript R/get_elites.R \
  -d "Experiments/PSO-X/Individuals/BH/Results,Experiments/PSO-X/Individuals/BH-65/Results,Experiments/PSO-X/Individuals/BL/Results,Experiments/PSO-X/Individuals/BL-32/Results" \
  -o "Experiments/PSO-X/Individuals-Elites/Configurations" \
  -p "Experiments/PSO-X/Others/Parameters.csv" \
  -n "All_Elites" \
  -v TRUE
```

Extract only the best elite per run (PSO-X):
```bash
Rscript R/get_elites.R \
  -d "Experiments/PSO-X/Individuals/BH/Results,Experiments/PSO-X/Individuals/BH-65/Results,Experiments/PSO-X/Individuals/BL/Results,Experiments/PSO-X/Individuals/BL-32/Results" \
  -o "Experiments/PSO-X/Individuals-Elites/Configurations" \
  -p "Experiments/PSO-X/Others/Parameters.csv" \
  -n "Best_Elites" \
  -b TRUE \
  -v TRUE
```

---

#### `generate_summarize_testing.R`
Processes elite testing results and training data to generate summary files for box plot analysis. This script calculates normalized ranking gaps (MNRG) for elite configurations across testing instances and identifies which training configurations performed best during testing.

**Purpose:**

This script bridges training and testing phases by:
1. Processing testing results from elite configurations to calculate quality metrics (MNRG)
2. Matching training configurations (from `configurations.csv`) with their testing performance
3. Identifying the best training configuration per run using `IS_BEST=TRUE` marker
4. Generating summary files that map training runs to their representative testing performance

**Usage:**
```bash
Rscript R/generate_summarize_testing.R \
  --testing_dir=<testing_dir> \
  --scenarios_dir=<scenarios_dir> \
  --output=<output_dir> \
  --parameters=<params_file> \
  [--problem_type=<min|max>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -t | --testing_dir | Directory containing elite testing .Rdata files | - | Yes |
| -s | --scenarios_dir | Directory containing scenario subdirectories with Results/ folders | - | Yes |
| -o | --output | Output directory for generated files | - | Yes |
| -p | --parameters | CSV file with parameters definition | - | Yes |
| -m | --problem_type | Optimization objective ('min' or 'max') | min | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Output Files:**

1. **`Optimum_Testing.csv`**: Best values achieved across all elite configurations for each testing instance
2. **`configurations_results.csv`**: Dictionary of all tested configurations with their parameters and MNRG values
3. **`<scenario_name>.csv`**: Per-scenario mapping of training runs to their best configuration's testing performance (MNRG)

**Processing Flow:**

1. Reads testing .Rdata files to extract performance matrices of elite configurations
2. Calculates MNRG (Mean Normalized Ranking Gap) for each configuration across testing instances
3. For each training scenario, reads `configurations.csv` from `Results/{SEED_ID}/`
4. Identifies the best configuration per run (marked as `IS_BEST=TRUE`)
5. Matches training configurations with testing configurations by parameter values
6. Outputs the testing MNRG value for each training run's best configuration

**Complete Example:**
```bash
Rscript R/generate_summarize_testing.R \
  -t "Experiments/ACOTSP/Individuals-Elites/General-Data/Best-Elites" \
  -s "Experiments/ACOTSP/Individuals" \
  -o "Experiments/ACOTSP/Individuals-Elites/Summarize" \
  -p "Experiments/ACOTSP/Others/Parameters.csv" \
  -m "min" \
  -v TRUE
```

---

#### `box_plot_best_elite_STN-i.R`
Creates box plot visualizations comparing multiple scenarios using summary CSV files. Generates publication-quality PDF plots with customizable dimensions, resolution, and statistical indicators.

**Purpose:**

This script provides visual comparison of testing performance across different scenarios through box plots. It allows visualization of:
- Distribution of MNRG values across training runs
- Median and quartile comparisons between scenarios
- Outlier detection and visualization
- Mean values for additional statistical insight
- Individual data points for transparency

**Usage:**
```bash
Rscript R/box_plot_best_elite_STN-i.R \
  --input=<file1.csv,file2.csv,...> \
  --output=<output_dir> \
  --filename=<output_name> \
  [--title=<plot_title>] \
  [--show_points=<TRUE|FALSE>] \
  [--show_outliers=<TRUE|FALSE>] \
  [--width=<width_inches>] \
  [--height=<height_inches>] \
  [--dpi=<resolution>] \
  [--verbose=<TRUE|FALSE>]
```

**Parameters:**

| Short | Long Parameter | Description | Default | Required |
|-------|---------------|-------------|---------|----------|
| -i | --input | Comma-separated list of CSV files to compare | - | Yes |
| -o | --output | Directory where output PDF will be saved | - | Yes |
| -f | --filename | Name of the output PDF file (without extension) | - | Yes |
| -t | --title | Title for the box plot | auto-generated | No |
| -p | --show_points | Show individual data points on box plot | TRUE | No |
| -l | --show_outliers | Show outliers (values beyond 1.5×IQR) in red | TRUE | No |
| -w | --width | Width of the PDF in inches | 10 | No |
| -e | --height | Height of the PDF in inches | 6 | No |
| -d | --dpi | Resolution in dots per inch | 300 | No |
| -v | --verbose | Show detailed processing information | FALSE | No |

**Visualized Metrics:**

Each box plot displays:
- **Box**: First quartile (Q1), median (Q2), and third quartile (Q3)
- **Whiskers**: Extend to 1.5 × IQR from quartiles with horizontal lines at endpoints
- **Outliers** (optional): Red points beyond whiskers
- **Individual points** (optional): Gray points for normal values, red for outliers

**Complete Examples:**

Generate box plot for ACOTSP scenarios:
```bash
Rscript R/box_plot_best_elite_STN-i.R \
  --input="Experiments/ACOTSP/Individuals-Elites/Summarize/BH.csv,Experiments/ACOTSP/Individuals-Elites/Summarize/BH-90.csv,Experiments/ACOTSP/Individuals-Elites/Summarize/BL.csv,Experiments/ACOTSP/Individuals-Elites/Summarize/BL-45.csv" \
  --output="Experiments/ACOTSP/Individuals-Elites/Summarize" \
  --filename="Box_Plot_BH_BH-90_BL_BL-45" \
  --show_points=TRUE \
  --show_outliers=TRUE \
  --verbose=TRUE
```

Generate box plot for PSO-X scenarios:
```bash
Rscript R/box_plot_best_elite_STN-i.R \
  --input="Experiments/PSO-X/Individuals-Elites/Summarize/BH.csv,Experiments/PSO-X/Individuals-Elites/Summarize/BH-65.csv,Experiments/PSO-X/Individuals-Elites/Summarize/BL.csv,Experiments/PSO-X/Individuals-Elites/Summarize/BL-32.csv" \
  --output="Experiments/PSO-X/Individuals-Elites/Summarize" \
  --filename="Box_Plot_BH_BH-65_BL_BL-32" \
  --title="Box Plots de Escenarios PSO-X: BH, BH-65, BL, BL-32" \
  --show_points=TRUE \
  --show_outliers=TRUE \
  --width=12 \
  --height=8 \
  --dpi=300 \
  --verbose=TRUE
```

---

#### `generate_elite_STN-i_file.R`
**TODO:** Processes elite configurations from irace testing results and generates STN-i files for each scenario.

---

## Automation Scripts

The repository includes bash scripts to automate common workflows. All scripts should be run from the repository root.

### `run_all_generate_STN-i_file.sh`
Generates STN-i trace files for all experiments and locations.

**Usage:**
```bash
# Using Individuals mode (default)
./Authomatize/run_all_generate_STN-i_file.sh

# Using Individuals-Elites mode
./Authomatize/run_all_generate_STN-i_file.sh --mode=Individuals-Elites
```

**Parameters:**
- `--mode=<Individuals|Individuals-Elites>`: Processing mode (default: Individuals)

**Output:**
- Log file in `Logs/run_all_generate_STN-i_file_<MODE>.log`
- Generated STN-i .csv files in `Experiments/<ALG>/<MODE>/<EXP>/STN-i-Files/`

---

### `run_all_generate_STN-i_Rdata.sh`
Converts all STN-i trace files (.csv) to graph objects (.Rdata).

**Usage:**
```bash
# Using Individuals mode (default)
./Authomatize/run_all_generate_STN-i_Rdata.sh

# Using Individuals-Elites mode
./Authomatize/run_all_generate_STN-i_Rdata.sh --mode=Individuals-Elites
```

**Parameters:**
- `--mode=<Individuals|Individuals-Elites>`: Processing mode (default: Individuals)

**Output:**
- Log file in `Logs/run_all_generate_STN-i_Rdata_<MODE>.log`
- Generated .Rdata files in `Experiments/<ALG>/<MODE>/<EXP>/STN-i-RData/`

---

### `run_all_plot_STN-i.sh`
Generates PDF visualizations for all STN-i networks.

**Usage:**
```bash
# Using Individuals mode with default settings
./Authomatize/run_all_plot_STN-i.sh

# Using Individuals-Elites mode
./Authomatize/run_all_plot_STN-i.sh --mode=Individuals-Elites

# Custom size factor and verbose output
./Authomatize/run_all_plot_STN-i.sh --size_factor=1.5 --verbose=TRUE
```

**Parameters:**
- `--mode=<Individuals|Individuals-Elites>`: Processing mode (default: Individuals)
- `--size_factor=<value>`: Scaling factor for node/edge sizes (default: 1.0)
- `--verbose=<TRUE|FALSE>`: Detailed output (default: FALSE)

**Customization:**
Edit the script to configure:
- `VALID_LAYOUTS`: Layout algorithms to use (fr, kk, circle, etc.)
- `VALID_PALETTES`: Color schemes (1-5)
- `VALID_SHAPE_OPTIONS`: Shape mappings (1-3)
- `VALID_SIZE_TYPES`: Node sizing methods (equals, configurations, out_degree, etc.)
- `VALID_SHOW_PARAMETERS`: Node visibility combinations
- `VALID_ZOOM_QUANTILES`: Focus levels

**Output:**
- Log file in `Logs/run_all_plot_STN-i_<MODE>.log`
- Generated PDF plots in `Experiments/<ALG>/<MODE>/<EXP>/STN-i-Plots/`

---

### `run_all_metrics_STN-i.sh`
Calculates metrics for all STN-i networks.

**Usage:**
```bash
# Using Individuals mode (default)
./Authomatize/run_all_metrics_STN-i.sh

# Using Individuals-Elites mode
./Authomatize/run_all_metrics_STN-i.sh --mode=Individuals-Elites
```

**Parameters:**
- `--mode=<Individuals|Individuals-Elites>`: Processing mode (default: Individuals)

**Output:**
- Log file in `Logs/run_all_metrics_STN-i_<MODE>.log`
- Generated .csv metrics in `Experiments/<ALG>/<MODE>/<EXP>/STN-i-Metrics/`

---

### `aggregate_all_metrics_STN-i.sh`
Aggregates all metrics from individual experiments into consolidated CSV files per algorithm.

**Usage:**
```bash
# Using Individuals mode (default)
./Authomatize/aggregate_all_metrics_STN-i.sh

# Using Individuals-Elites mode
./Authomatize/aggregate_all_metrics_STN-i.sh --mode=Individuals-Elites
```

**Parameters:**
- `--mode=<Individuals|Individuals-Elites>`: Processing mode (default: Individuals)

**Output:**
For each algorithm (ACOTSP, PSO-X) in `Experiments/<ALG>/General-Metrics/`:
- `All_Metrics_nodes.csv`: Node-focused metrics from all experiments
- `All_Metrics_elite_nodes.csv`: Elite-specific metrics from all experiments
- `All_Metrics_configurations.csv`: Configuration-focused metrics from all experiments

(Add `_Elites` suffix when using Individuals-Elites mode)

---

## Location Code Generation Algorithm

The `get_location_code()` function in `R/Functions/network_utils.R` generates unique identifiers (location codes) for configurations by discretizing their parameter values into subranges. This is the mathematical foundation for node grouping in STN-i.

### Core Concept

Each configuration is mapped to a **location code** by processing its parameters according to their type (categorical, integer, or real). The location code uniquely identifies a "location" in the parameter space, which corresponds to a node in the STN-i graph.

### Numeric Parameter Bucketing (Integer & Real)

For numeric parameters (integer or real types), we create **discrete buckets** using subrange indices:

#### Subrange Index Calculation

Given a parameter value `v` in the range `[lower_bound, upper_bound]` with step size `step`:

```math
\mathrm{subrange\_index} = \left\lfloor \frac{v - \mathrm{lower\_bound}}{\mathrm{step}} \right\rfloor
```

This formula assigns each value to a bucket `[k \cdot step, (k+1) \cdot step)` where `k = subrange_index`.

**Example:** For `inertia` with range `[0.0, 0.9]` and `step=0.01`:
- Value `0.045` → `subrange_index = floor((0.045 - 0) / 0.01) = 4` → bucket `[0.04, 0.05)`
- Value `0.049` → `subrange_index = floor((0.049 - 0) / 0.01) = 4` → bucket `[0.04, 0.05)` (same bucket)
- Value `0.050` → `subrange_index = floor((0.050 - 0) / 0.01) = 5` → bucket `[0.05, 0.06)` (different bucket)

#### Scaled Integer Arithmetic

To avoid floating-point precision issues, we perform all calculations using **integer arithmetic**:

```math
\mathrm{scale} = 10^{\mathrm{significance}}
```

```math
\mathrm{scaled\_lower} = \mathrm{round}(\mathrm{lower\_bound} \times \mathrm{scale})
```

```math
\mathrm{scaled\_upper} = \mathrm{round}(\mathrm{upper\_bound} \times \mathrm{scale})
```

```math
\mathrm{scaled\_step} = \mathrm{round}(\mathrm{step} \times \mathrm{scale})
```

```math
\mathrm{scaled\_value} = \mathrm{round}(v \times \mathrm{scale})
```

#### Subrange Index in Scaled Space

```math
\mathrm{subrange\_index} = \left\lfloor \frac{\mathrm{scaled\_value} - \mathrm{scaled\_lower}}{\mathrm{scaled\_step}} \right\rfloor
```

With clamping to ensure the index stays within valid bounds:

```math
\mathrm{subrange\_index} = \min\left(
  \max(\mathrm{subrange\_index}, 0),
  \left\lfloor
    \frac{\mathrm{scaled\_upper} - \mathrm{scaled\_lower}}{\mathrm{scaled\_step}}
  \right\rfloor
\right)
```

#### Code Generation for Numeric Parameters

1. Calculate the **starting value of the bucket**:
```math
\mathrm{calculated\_scaled} = \mathrm{scaled\_lower} + \mathrm{subrange\_index} \times \mathrm{scaled\_step}
```

2. Determine **digit padding** (to maintain fixed-width formatting):
```math
\mathrm{max\_digits} = \mathrm{len}(\mathrm{str}(\mathrm{scaled\_upper}))
```
```math
\mathrm{current\_digits} = \mathrm{len}(\mathrm{str}(\mathrm{calculated\_scaled}))
```
```math
\mathrm{padding} = \mathrm{max\_digits} - \mathrm{current\_digits}
```

3. Format as code part:
```math
\mathrm{code\_part} = \texttt{"0"} \times \mathrm{padding} + \mathrm{str}(\mathrm{calculated\_scaled})
```

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

```math
\mathrm{code} = \mathrm{location\_dict}[\mathrm{value}]
```

The code is padded with leading zeros to match the maximum code width:

```math
\mathrm{max\_code\_width} = \mathrm{len}(\mathrm{str}(\max(\mathrm{location\_dict\ values})))
```

**Example:** For `topology` with mapping `{0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6}`:

```
value = 5 → code_dict["5"] = 5 → max_width = 1 → code_part = "5"
value = 2 → code_dict["2"] = 2 → max_width = 1 → code_part = "2"
```

### Missing Values (NA)

When a parameter value is `NA` (e.g., a conditional parameter that's not active):

For numeric parameters:
```math
\mathrm{code\_part} = \texttt{"X"} \times \mathrm{len}(\mathrm{str}(\mathrm{scaled\_upper}))
```

For categorical parameters:
```math
\mathrm{code\_part} = \texttt{"X"} \times \mathrm{max\_code\_width}
```

### Final Location Code

The final location code is the concatenation of all parameter codes in order:

```math
\mathrm{location\_code} = \mathrm{code\_param\_1} + \mathrm{code\_param\_2} + \ldots + \mathrm{code\_param\_n}
```

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

---

## Additional Data

### Experiments

#### ACOTSP Location Parameters

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

#### PSO-X Location Parameters

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
