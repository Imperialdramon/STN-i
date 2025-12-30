
# nolint start

#########################################################################
# Box Plot Best Elite STN-i Script
# Author: Pablo Estobar
#
# Description:
# This script creates box plot visualizations comparing multiple scenarios
# using CSV data files. Each input file represents a scenario, and the
# script generates a publication-quality PDF with customizable dimensions
# and resolution.
#
# Usage:
# Rscript box_plot_best_elite_STN-i.R --input=<file1.csv,file2.csv,...> \
#                                      --output=<output_dir> \
#                                      --filename=<output_name> \
#                                      [--title=<plot_title>] \
#                                      [--show_points=<TRUE|FALSE>] \
#                                      [--show_outliers=<TRUE|FALSE>] \
#                                      [--width=<width_inches>] \
#                                      [--height=<height_inches>] \
#                                      [--dpi=<resolution>] \
#                                      [--verbose=<TRUE|FALSE>]
#
# Arguments:
# --input          : (Required) Comma-separated list of CSV files to compare
# --output         : (Required) Directory where output PDF will be saved
# --filename       : (Required) Name of the output PDF file (without extension)
# --title          : (Optional) Title for the box plot [default: auto-generated]
# --show_points    : (Optional) Show individual data points on box plot [default: TRUE]
# --show_outliers  : (Optional) Show outliers (values beyond 1.5*IQR) [default: TRUE]
# --width          : (Optional) Width of the PDF in inches [default: 10]
# --height         : (Optional) Height of the PDF in inches [default: 6]
# --dpi            : (Optional) Resolution in dots per inch [default: 300]
# --verbose        : (Optional) Show detailed processing information [default: FALSE]
#
# Requirements:
# - R with the following packages installed:
#     - optparse
#     - ggplot2
#
# Notes:
# - Input CSV files should have a semicolon (;) separator
# - File names (without extension) are used as scenario labels
# - All CSV files should have the same column structure
#########################################################################

# ---------- Validate required packages ----------
required_packages <- c("optparse", "ggplot2")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
    stop(sprintf("Error: The following packages are not installed: %s\nPlease install them with: install.packages(c(%s))",
                 paste(missing_packages, collapse = ", "),
                 paste(sprintf('"%s"', missing_packages), collapse = ", ")),
         call. = FALSE)
}

# ---------- Load required packages ----------
library(optparse)
library(ggplot2)

# Define command line options
option_list <- list(
    make_option(c("-i", "--input"),
                type = "character",
                help = "Comma-separated list of CSV files to compare"),
    
    make_option(c("-o", "--output"),
                type = "character",
                help = "Directory where output PDF will be saved"),
    
    make_option(c("-f", "--filename"),
                type = "character",
                help = "Name of the output PDF file (without extension)"),
    
    make_option(c("-t", "--title"),
                type = "character",
                default = NULL,
                help = "Title for the box plot [default: auto-generated from scenario names]"),
    
    make_option(c("-p", "--show_points"),
                type = "logical",
                default = TRUE,
                help = "Show individual data points on box plot [default: %default]"),
    
    make_option(c("-l", "--show_outliers"),
                type = "logical",
                default = TRUE,
                help = "Show outliers (values beyond 1.5*IQR) [default: %default]"),
    
    make_option(c("-w", "--width"),
                type = "numeric",
                default = 10,
                help = "Width of the PDF in inches [default: %default]"),
    
    make_option(c("-e", "--height"),
                type = "numeric",
                default = 6,
                help = "Height of the PDF in inches [default: %default]"),
    
    make_option(c("-d", "--dpi"),
                type = "numeric",
                default = 300,
                help = "Resolution in dots per inch [default: %default]"),
    
    make_option(c("-v", "--verbose"),
                type = "logical",
                default = FALSE,
                help = "Show detailed processing information [default: %default]")
)

# Parse command line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Validate required arguments
if (is.null(opt$input)) {
    stop("Input files are required (-i/--input)")
}
if (is.null(opt$output)) {
    stop("Output directory is required (-o/--output)")
}
if (is.null(opt$filename)) {
    stop("Output filename is required (-f/--filename)")
}

# Validate numeric arguments
if (opt$width <= 0) {
    stop("Width must be a positive number")
}
if (opt$height <= 0) {
    stop("Height must be a positive number")
}
if (opt$dpi <= 0) {
    stop("DPI must be a positive number")
}

# Validate logical arguments
if (!is.logical(opt$show_points)) {
    stop("The 'show_points' argument must be TRUE or FALSE")
}
if (!is.logical(opt$show_outliers)) {
    stop("The 'show_outliers' argument must be TRUE or FALSE")
}
if (!is.logical(opt$verbose)) {
    stop("The 'verbose' argument must be TRUE or FALSE")
}

# Process input files
input_files <- strsplit(opt$input, ",")[[1]]
input_files <- trimws(input_files)

# Validate that all input files exist
for (file in input_files) {
    if (!file.exists(file)) {
        stop(sprintf("Input file does not exist: %s", file))
    }
}

# Process output directory
output_dir <- normalizePath(opt$output, mustWork = FALSE)
if (!dir.exists(output_dir)) {
    if (opt$verbose) cat(sprintf("Creating output directory: %s\n", output_dir))
    dir.create(output_dir, recursive = TRUE)
}

# Prepare output file path
output_file <- file.path(output_dir, paste0(opt$filename, ".pdf"))

if (opt$verbose) {
    cat("=== Box Plot Configuration ===\n")
    cat(sprintf("Input files: %d\n", length(input_files)))
    for (i in seq_along(input_files)) {
        cat(sprintf("  [%d] %s\n", i, input_files[i]))
    }
    cat(sprintf("Output file: %s\n", output_file))
    cat(sprintf("Dimensions: %.1f x %.1f inches\n", opt$width, opt$height))
    cat(sprintf("Resolution: %d DPI\n", opt$dpi))
    cat(sprintf("Show points: %s\n", opt$show_points))
    cat(sprintf("Show outliers: %s\n", opt$show_outliers))
    cat("=============================\n\n")
}

# ---------- Read and process data ----------
if (opt$verbose) cat("Reading input CSV files...\n")

all_data <- list()
scenario_names <- character()

for (file in input_files) {
    if (opt$verbose) cat(sprintf("  Reading: %s\n", file))
    
    # Read CSV with semicolon separator
    data <- tryCatch({
        read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    }, error = function(e) {
        stop(sprintf("Error reading file %s: %s", file, e$message))
    })
    
    # Extract scenario name from filename (without extension)
    scenario_name <- tools::file_path_sans_ext(basename(file))
    scenario_names <- c(scenario_names, scenario_name)
    
    # Store data
    all_data[[scenario_name]] <- data
    
    if (opt$verbose) {
        cat(sprintf("    Rows: %d, Columns: %d\n", nrow(data), ncol(data)))
    }
}

if (opt$verbose) cat(sprintf("\nProcessed %d scenarios: %s\n\n", 
                             length(scenario_names), 
                             paste(scenario_names, collapse = ", ")))

# ---------- Prepare data for plotting ----------
if (opt$verbose) cat("Preparing data for box plot...\n")

# Determine which column to use for plotting
# We'll use the first numeric column that's not an ID column
plot_data_list <- list()

for (scenario_name in scenario_names) {
    data <- all_data[[scenario_name]]
    
    # Find suitable numeric columns (exclude columns with "ID" in name)
    numeric_cols <- sapply(data, is.numeric)
    id_cols <- grepl("ID", colnames(data), ignore.case = TRUE)
    suitable_cols <- numeric_cols & !id_cols
    
    if (sum(suitable_cols) == 0) {
        stop(sprintf("No suitable numeric columns found in %s.csv", scenario_name))
    }
    
    # Use the first suitable column
    col_name <- colnames(data)[suitable_cols][1]
    values <- data[[col_name]]
    
    if (opt$verbose) {
        cat(sprintf("  Scenario '%s': Using column '%s' (%d values)\n", 
                    scenario_name, col_name, length(values)))
    }
    
    # Create data frame for this scenario
    scenario_df <- data.frame(
        Scenario = scenario_name,
        Value = values,
        stringsAsFactors = FALSE
    )
    
    plot_data_list[[scenario_name]] <- scenario_df
}

# Combine all data
plot_data <- do.call(rbind, plot_data_list)
rownames(plot_data) <- NULL

# Convert Scenario to factor to control order
plot_data$Scenario <- factor(plot_data$Scenario, levels = scenario_names)

if (opt$verbose) {
    cat(sprintf("\nTotal data points: %d\n", nrow(plot_data)))
    cat("Summary by scenario:\n")
    print(tapply(plot_data$Value, plot_data$Scenario, summary))
    cat("\n")
}

# ---------- Generate plot title ----------
if (is.null(opt$title)) {
    opt$title <- sprintf("Box Plots de Escenarios: %s", 
                        paste(scenario_names, collapse = ", "))
}

if (opt$verbose) cat(sprintf("Plot title: %s\n\n", opt$title))

# ---------- Create box plot ----------
if (opt$verbose) cat("Creating box plot...\n")

# Define a simple color palette
n_scenarios <- length(scenario_names)
color_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                   "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

# Repeat palette if we have more scenarios than colors
if (n_scenarios > length(color_palette)) {
    color_palette <- rep(color_palette, ceiling(n_scenarios / length(color_palette)))
}
color_palette <- color_palette[1:n_scenarios]

# Identify outliers for proper visualization
if (opt$show_outliers && opt$show_points) {
    if (opt$verbose) cat("  Identifying outliers for proper layering...\n")
    
    # Calculate outliers for each scenario
    plot_data$is_outlier <- FALSE
    for (scenario in levels(plot_data$Scenario)) {
        scenario_data <- plot_data$Value[plot_data$Scenario == scenario]
        q1 <- quantile(scenario_data, 0.25, na.rm = TRUE)
        q3 <- quantile(scenario_data, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        
        plot_data$is_outlier[plot_data$Scenario == scenario] <- 
            (scenario_data < lower_bound | scenario_data > upper_bound)
    }
    
    # Separate outliers from normal points
    normal_points <- plot_data[!plot_data$is_outlier, ]
    outlier_points <- plot_data[plot_data$is_outlier, ]
    
    if (opt$verbose) {
        cat(sprintf("    Normal points: %d, Outliers: %d\n", 
                    nrow(normal_points), nrow(outlier_points)))
    }
}

# Create the plot
p <- ggplot(plot_data, aes(x = Scenario, y = Value, fill = Scenario)) +
    geom_boxplot(alpha = 0.7, 
                 outlier.shape = NA,
                 linewidth = 0.6,
                 staplewidth = 0.5) +  # staplewidth adds horizontal lines at whisker ends
    scale_fill_manual(values = color_palette) +
    labs(title = opt$title,
         x = "Escenario",
         y = "Valor") +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
    )

# Add points based on configuration
if (opt$show_points && opt$show_outliers) {
    # Show normal points and outliers separately
    if (opt$verbose) cat("  Adding normal points and outliers separately...\n")
    
    # Add normal points in gray
    if (nrow(normal_points) > 0) {
        p <- p + geom_jitter(data = normal_points, 
                           alpha = 0.3, width = 0.2, size = 1.5, 
                           color = "gray40")
    }
    
    # Add outliers in red on top
    if (nrow(outlier_points) > 0) {
        p <- p + geom_jitter(data = outlier_points, 
                           alpha = 0.7, width = 0.2, size = 2, 
                           color = "red")
    }
} else if (opt$show_points && !opt$show_outliers) {
    # Show all points uniformly (no outlier distinction)
    if (opt$verbose) cat("  Adding all data points...\n")
    p <- p + geom_jitter(alpha = 0.3, width = 0.2, size = 1.5)
} else if (!opt$show_points && opt$show_outliers) {
    # Show only outliers
    if (opt$verbose) cat("  Adding only outliers...\n")
    p <- p + geom_boxplot(alpha = 0.7, 
                         outlier.colour = "red", 
                         outlier.size = 2, 
                         outlier.alpha = 0.7)
}

# ---------- Save plot ----------
if (opt$verbose) cat(sprintf("Saving plot to: %s\n", output_file))

tryCatch({
    ggsave(
        filename = output_file,
        plot = p,
        width = opt$width,
        height = opt$height,
        dpi = opt$dpi,
        device = "pdf"
    )
    
    if (opt$verbose) {
        cat("\n=== Plot saved successfully ===\n")
        cat(sprintf("File: %s\n", output_file))
        cat(sprintf("Size: %.2f MB\n", file.info(output_file)$size / 1024^2))
        cat("===============================\n")
    } else {
        cat(sprintf("Box plot saved to: %s\n", output_file))
    }
    
}, error = function(e) {
    stop(sprintf("Error saving plot: %s", e$message))
})

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end