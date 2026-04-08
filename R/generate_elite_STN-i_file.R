# nolint start

#########################################################################
# Elite STN-i File Generation Script
# Author: Pablo Estobar
#
# Description:
# Generates STN-i files from elite configurations found in irace runs.
# Elite configurations are those marked with IS_ELITE == TRUE in each run's
# configurations.csv file. MMNRG quality metric is calculated from testing
# matrices and associated with each configuration.
#
# Process:
# 1. Load elite testing matrices and calculate MMNRG quality metric
# 2. Filter scenario run data to ONLY elite configurations
# 3. Recreate Results/ structure with filtered elites
# 4. Generate STN-i files using standard generate_stn_i() function
#
# Usage:
# Rscript generate_elite_STN-i_file.R --elites_dir=<path> --scenario_dir=<path> \
#         --general_parameters=<file> --parameters=<dir> --output=<dir> \
#         --optimum_file=<file> --name=<file> [--verbose=TRUE]
#
#########################################################################

# Validate required packages
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("irace", quietly = TRUE)) {
  stop("The irace package is not installed. Please install it with 'install.packages(\"irace\")'", call. = FALSE)
}
if (!requireNamespace("tools", quietly = TRUE)) {
  stop("The tools package is not installed. Please install it with 'install.packages(\"tools\")'", call. = FALSE)
}
if (!requireNamespace("optparse", quietly = TRUE)) {
  stop("The optparse package is not installed. Please install it with 'install.packages(\"optparse\")'", call. = FALSE)
}

library(igraph)
library(irace)
library(tools)
library(optparse)

source("R/Functions/network_utils.R")

# Define command line options
option_list <- list(
  make_option(c("-e", "--elites_dir"),
              type="character",
              help="Directory with elite testing .Rdata files"),
  
  make_option(c("-s", "--scenario_dir"),
              type="character",
              help="Scenario directory containing Results/ subdirectory"),
  
  make_option(c("-g", "--general_parameters"),
              type="character",
              help="CSV file with general parameters"),
  
  make_option(c("-p", "--parameters"),
              type="character",
              help="Directory with location parameter CSV files"),
  
  make_option(c("-o", "--output"),
              type="character",
              help="Output directory for STN-i files"),
  
  make_option(c("-m", "--optimum_file"),
              type="character",
              help="CSV file with instance optimum values for elite testing"),
  
  make_option(c("-n", "--name"),
              type="character",
              default=NULL,
              help="Output filename [default = scenario name]"),
  
  make_option(c("-c", "--quality_criteria"),
              type="character",
              default="mean",
              help="Quality criterion: 'min','max','mean','median','mode' [default = %default]"),
  
  make_option(c("-r", "--representative_criteria"),
              type="character",
              default="mean",
              help="Representative criterion [default = %default]"),
  
  make_option(c("-b", "--best_criteria"),
              type="character",
              default="min",
              help="Best criteria (ignored for elite, kept for compatibility) [default = %default]"),
  
  make_option(c("-x", "--significance"),
              type="integer",
              default=2,
              help="Significance level for parameters [default = %default]"),
  
  make_option(c("-v", "--verbose"),
              type="logical",
              default=FALSE,
              help="Show detailed processing information [default = %default]")
)

opt <- parse_args(OptionParser(option_list=option_list))

# Validate required arguments
if (is.null(opt$elites_dir)) stop("Elites directory required (-e/--elites_dir)")
if (is.null(opt$scenario_dir)) stop("Scenario directory required (-s/--scenario_dir)")
if (is.null(opt$general_parameters)) stop("General parameters required (-g/--general_parameters)")
if (is.null(opt$parameters)) stop("Parameters directory required (-p/--parameters)")
if (is.null(opt$output)) stop("Output directory required (-o/--output)")
if (is.null(opt$optimum_file)) stop("Optimum file required (-m/--optimum_file)")

# Normalize and verify paths
elites_dir <- normalizePath(opt$elites_dir)
scenario_dir <- normalizePath(opt$scenario_dir)
general_params_file <- normalizePath(opt$general_parameters)
params_dir <- normalizePath(opt$parameters)
output_dir <- normalizePath(opt$output, mustWork=FALSE)
optimum_file <- normalizePath(opt$optimum_file)

if (!dir.exists(elites_dir)) stop(sprintf("Elites directory not found: %s", elites_dir))
if (!dir.exists(scenario_dir)) stop(sprintf("Scenario directory not found: %s", scenario_dir))
if (!file.exists(general_params_file)) stop(sprintf("General parameters file not found: %s", general_params_file))
if (!dir.exists(params_dir)) stop(sprintf("Parameters directory not found: %s", params_dir))
if (!file.exists(optimum_file)) stop(sprintf("Optimum file not found: %s", optimum_file))

dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

if (opt$verbose) cat("=== Elite STN-i Generation ===\n\n")

# ========== STEP 1: Calculate MMNRG from elite testing matrices ==========

if (opt$verbose) cat("Step 1: Loading elite testing matrices and calculating MMNRG...\n")

optimum_df <- read.csv(optimum_file, sep=";", stringsAsFactors=FALSE)
rdata_files <- list.files(elites_dir, pattern="\\.Rdata$", full.names=TRUE)

if (length(rdata_files) == 0) {
  stop(sprintf("No .Rdata files in %s", elites_dir))
}

all_configs <- c()
mmnrg_values <- list()

for (i in seq_along(rdata_files)) {
  load(rdata_files[i])
  
  if (is.null(iraceResults$testing) || is.null(iraceResults$testing$experiments)) next
  
  experiments <- iraceResults$testing$experiments
  test_instances <- iraceResults$scenario$testInstances
  sense <- iraceResults$scenario$sense
  if (is.na(sense) || is.null(sense)) sense <- "min"
  sense <- as.character(sense)[1]
  
  instance_names <- basename(test_instances)
  all_configs <- unique(c(all_configs, colnames(experiments)))
  
  # Calculate MMNRG for this matrix
  gap_matrix <- matrix(NA, nrow=nrow(experiments), ncol=ncol(experiments))
  colnames(gap_matrix) <- colnames(experiments)
  rownames(gap_matrix) <- instance_names
  
  for (j in seq_len(nrow(experiments))) {
    inst <- instance_names[j]
    opt_val <- optimum_df$BEST[optimum_df$INSTANCE == inst]
    if (length(opt_val) == 0) next
    
    for (k in seq_len(ncol(experiments))) {
      val <- experiments[j, k]
      if (!is.na(val) && !is.na(opt_val)) {
        gap_matrix[j, k] <- abs(val - opt_val)
      }
    }
  }
  
  # Ranking per row (horizontal)
  rank_matrix <- matrix(NA, nrow=nrow(gap_matrix), ncol=ncol(gap_matrix))
  colnames(rank_matrix) <- colnames(gap_matrix)
  rownames(rank_matrix) <- rownames(gap_matrix)
  
  for (j in seq_len(nrow(gap_matrix))) {
    row_vals <- gap_matrix[j, !is.na(gap_matrix[j, ])]
    if (length(row_vals) > 1) {
      idx <- which(!is.na(gap_matrix[j, ]))
      rank_matrix[j, idx] <- rank(row_vals)
    }
  }
  
  # Normalization [0,1] per row
  norm_matrix <- matrix(NA, nrow=nrow(rank_matrix), ncol=ncol(rank_matrix))
  colnames(norm_matrix) <- colnames(rank_matrix)
  rownames(norm_matrix) <- rownames(rank_matrix)
  
  for (j in seq_len(nrow(rank_matrix))) {
    row_vals <- rank_matrix[j, !is.na(rank_matrix[j, ])]
    if (length(row_vals) > 1) {
      idx <- which(!is.na(rank_matrix[j, ]))
      min_r <- min(row_vals)
      max_r <- max(row_vals)
      if (max_r > min_r) {
        norm_matrix[j, idx] <- (rank_matrix[j, idx] - min_r) / (max_r - min_r)
      } else {
        norm_matrix[j, idx] <- 0
      }
    }
  }
  
  # Mean per configuration (vertical mean)
  col_means <- colMeans(norm_matrix, na.rm=TRUE)
  
  # Accumulate for final MMNRG (mean of means)
  for (cfg_name in names(col_means)) {
    if (!(cfg_name %in% names(mmnrg_values))) {
      mmnrg_values[[cfg_name]] <- c()
    }
    mmnrg_values[[cfg_name]] <- c(mmnrg_values[[cfg_name]], col_means[[cfg_name]])
  }
}

final_mmnrg <- sapply(mmnrg_values, function(vals) mean(vals, na.rm=TRUE))

if (opt$verbose) {
  cat(sprintf("  Calculated MMNRG for %d configurations\n", length(final_mmnrg)))
  cat(sprintf("  MMNRG range: [%.4f, %.4f]\n\n", min(final_mmnrg, na.rm=TRUE), max(final_mmnrg, na.rm=TRUE)))
}

# ========== STEP 2: Filter scenario data to elite configurations ==========

if (opt$verbose) cat("Step 2: Filtering scenario configurations to elite status per run...\n")

results_dir <- file.path(scenario_dir, "Results")
if (!dir.exists(results_dir)) {
  stop(sprintf("Results directory not found in %s", scenario_dir))
}

# Get all run directories
run_dirs <- list.dirs(results_dir, recursive=FALSE, full.names=TRUE)
if (length(run_dirs) == 0) {
  stop(sprintf("No run directories found in %s", results_dir))
}

# Create Results directory with filtered elite data (permanent, in scenario folder)
# Convert scenario path from Individuals/BH to Individuals-Elites/BH
scenario_base_dir <- dirname(scenario_dir)  # e.g., Experiments/ACOTSP/Individuals
scenario_elites_dir <- gsub("/Individuals/", "/Individuals-Elites/", scenario_dir)  # e.g., Experiments/ACOTSP/Individuals-Elites/BH

elite_results_dir <- file.path(scenario_elites_dir, "Results")

unlink(elite_results_dir, recursive=TRUE)
dir.create(elite_results_dir, recursive=TRUE, showWarnings=FALSE)

elite_config_count <- 0

for (run_path in run_dirs) {
  run_id <- basename(run_path)
  configs_file <- file.path(run_path, "configurations.csv")
  traj_file <- file.path(run_path, "trajectories.csv")
  results_file <- file.path(run_path, "results.csv")
  
  if (!file.exists(configs_file) || !file.exists(traj_file)) {
    if (opt$verbose) cat(sprintf("    Skipping %s: missing files\n", run_id))
    next
  }
  
  # Read original run data
  run_configs <- read.csv(configs_file, sep=";", stringsAsFactors=FALSE)
  run_trajs <- read.csv(traj_file, sep=";", stringsAsFactors=FALSE)
  run_results <- if (file.exists(results_file)) read.csv(results_file, sep=";", stringsAsFactors=FALSE) else NULL
  
  # Filter to only elite configurations
  if ("IS_ELITE" %in% colnames(run_configs)) {
    if (is.logical(run_configs$IS_ELITE)) {
      elite_mask <- run_configs$IS_ELITE == TRUE
    } else {
      elite_mask <- tolower(as.character(run_configs$IS_ELITE)) == "true"
    }
  } else {
    # No elite column: use all configs
    elite_mask <- rep(TRUE, nrow(run_configs))
  }
  
  elite_configs <- run_configs[elite_mask, ]
  elite_config_count <- elite_config_count + nrow(elite_configs)
  
  if (nrow(elite_configs) == 0) {
    if (opt$verbose) cat(sprintf("    %s: No elite configs found\n", run_id))
    next
  }
  
  # Add mapping columns to track original vs testing
  elite_configs$ORIGINAL_CONFIG_ID <- elite_configs$CONFIG_ID
  elite_configs$TESTING_CONFIG_ID <- NA
  
  # Map each config to testing elite ID using MMNRG
  for (i in seq_len(nrow(elite_configs))) {
    cfg_id <- as.character(elite_configs$CONFIG_ID[i])
    if (cfg_id %in% names(final_mmnrg)) {
      elite_configs$TESTING_CONFIG_ID[i] <- which(names(final_mmnrg) == cfg_id)
    }
  }
  
  # Filter trajectories to only those between elite configs
  elite_config_ids <- elite_configs$ORIGINAL_CONFIG_ID
  traj_mask <- (run_trajs$ORIGIN_CONFIG_ID %in% elite_config_ids) & 
               (run_trajs$DESTINY_CONFIG_ID %in% elite_config_ids)
  elite_trajs <- run_trajs[traj_mask, ]
  
  if (nrow(elite_trajs) == 0) {
    if (opt$verbose) cat(sprintf("    %s: No elite trajectories found\n", run_id))
    next
  }
  
  # Prepare results data with QUALITY metric
  if (!is.null(run_results) && "QUALITY" %in% colnames(run_results)) {
    elite_results <- run_results[run_results$CONFIG_ID %in% elite_config_ids, ]
  } else {
    # Create results with MMNRG as QUALITY
    elite_results <- data.frame(
      CONFIG_ID = elite_configs$ORIGINAL_CONFIG_ID,
      QUALITY = sapply(elite_configs$ORIGINAL_CONFIG_ID, function(x) {
        idx <- which(names(final_mmnrg) == as.character(x))
        if (length(idx) > 0) final_mmnrg[idx] else mean(final_mmnrg, na.rm=TRUE)
      }),
      stringsAsFactors = FALSE
    )
  }
  
  # Save filtered elite data to scenario Results directory
  run_dir_in_scenario <- file.path(elite_results_dir, run_id)
  
  dir.create(run_dir_in_scenario, recursive=TRUE, showWarnings=FALSE)
  
  # Save to scenario Results directory
  write.table(elite_configs, file.path(run_dir_in_scenario, "configurations.csv"),
              sep=";", row.names=FALSE, quote=FALSE)
  write.table(elite_trajs, file.path(run_dir_in_scenario, "trajectories.csv"),
              sep=";", row.names=FALSE, quote=FALSE)
  write.table(elite_results, file.path(run_dir_in_scenario, "results.csv"),
              sep=";", row.names=FALSE, quote=FALSE)
  
  if (opt$verbose) cat(sprintf("    %s: %d elite configs, %d trajectories\n", 
                                run_id, nrow(elite_configs), nrow(elite_trajs)))
}

if (opt$verbose) cat(sprintf("  Total elite configs across all runs: %d\n\n", elite_config_count))

if (elite_config_count == 0) {
  stop("No elite configurations found in any run")
}

# ========== STEP 3: Generate STN-i using filtered elite data ==========

if (opt$verbose) cat("Step 3: Generating STN-i files per location...\n\n")

scenario_name <- basename(scenario_dir)
param_files <- sort(list.files(params_dir, pattern="\\.csv$", full.names=TRUE))

for (param_file in param_files) {
  param_basename <- file_path_sans_ext(basename(param_file))
  
  # Generate output filename
  if (is.null(opt$name)) {
    output_name <- sprintf("%s_%s.csv", scenario_name, param_basename)
  } else {
    if (length(param_files) > 1) {
      base_name <- file_path_sans_ext(opt$name)
      ext <- file_ext(opt$name)
      output_name <- sprintf("%s-%s.%s", base_name, param_basename, ext)
    } else {
      output_name <- opt$name
    }
  }
  
  if (opt$verbose) cat(sprintf("  Generating: %s\n", output_name))
  
  # Call generate_stn_i with permanent Results directory
  tryCatch({
    generate_stn_i(
      input_dir = elite_results_dir,
      parameters_file = param_file,
      output_dir = output_dir,
      output_name = output_name,
      quality_criteria = opt$quality_criteria,
      representative_criteria = opt$representative_criteria,
      significance = opt$significance,
      verbose = opt$verbose
    )
  }, error = function(e) {
    cat(sprintf("Error generating %s: %s\n", output_name, conditionMessage(e)))
  })
}

if (opt$verbose) cat("\n=== Generation Complete ===\n")

# Cleanup
rm(list=ls())
gc()
quit(save="no")

# nolint end
