# nolint start

#' Get STN-i metrics focused on nodes
#'
#' This function extracts metrics related to nodes from an STN-i result.
#' It includes node counts, topologies, elite status, and path information.
#'
#' @param stn_i_result A list containing the STN-i data, typically returned by `stn_i_create()` or `get_stn_i_data()`.
#'
#' @return A list of node-focused metrics.
#'
#' @examples
#' \dontrun{
#' node_metrics <- get_stn_i_metrics_nodes(stn_i_result)
#' }
#'
#' @export
get_stn_i_metrics_nodes <- function(stn_i_result) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the STN-i and metadata
  STN_i <- stn_i_result$STN_i
  network_name <- stn_i_result$network_name
  problem_type <- stn_i_result$problem_type
  best_known_solution <- stn_i_result$best_known_solution
  number_of_runs <- stn_i_result$number_of_runs

  # Direct metrics
  metrics$network_name <- network_name
  metrics$problem_type <- problem_type
  metrics$number_of_runs <- number_of_runs
  metrics$best_known_solution <- best_known_solution

  # Compute components metric
  # Single: one node, no edges
  # Multiple: more than one node, with edges
  comp_data <- components(STN_i)
  metrics$components <- comp_data$no
  if (metrics$components > 0) {
    comp_membership <- comp_data$membership
    comp_sizes <- table(comp_membership)
    metrics$single_components <- sum(comp_sizes == 1)
    metrics$multiple_components <- sum(comp_sizes > 1)
  } else {
    metrics$single_components <- 0
    metrics$multiple_components <- 0
  }

  # Compute nodes quantity metrics
  metrics$nodes <- vcount(STN_i)
  metrics$regular_nodes <- sum(V(STN_i)$IS_ELITE == FALSE)
  metrics$elite_nodes <- sum(V(STN_i)$IS_ELITE == TRUE)
  metrics$start_nodes <- sum(V(STN_i)$STARTS > 0)
  metrics$standard_nodes <- sum(V(STN_i)$STANDARD > 0)
  metrics$end_nodes <- sum(V(STN_i)$ENDS > 0)

  # Compute edges quantity metrics
  metrics$edges <- ecount(STN_i)
  metrics$worsening_edges <- sum(E(STN_i)$Type == "WORSENING")
  metrics$equal_edges <- sum(E(STN_i)$Type == "EQUAL")
  metrics$improving_edges <- sum(E(STN_i)$Type == "IMPROVING")

  # Compute general degree metrics
  if (metrics$nodes > 0) {
    # Total degree (in + out)
    metrics$max_degree <- max(degree(STN_i, mode = "all"), na.rm = TRUE)
    metrics$min_degree <- min(degree(STN_i, mode = "all"), na.rm = TRUE)
    metrics$average_degree <- mean(degree(STN_i, mode = "all"), na.rm = TRUE)
  } else {
    metrics$max_degree <- NA
    metrics$min_degree <- NA
    metrics$average_degree <- NA
  }

  # Compute degree metrics for regular and elite nodes
  regular_nodes <- V(STN_i)[V(STN_i)$IS_ELITE == FALSE]
  if (length(regular_nodes) > 0) {
    # In-degree for regular nodes
    metrics$average_regular_in_degree <- mean(degree(STN_i, v = regular_nodes, mode = "in"), na.rm = TRUE)
  } else {
    metrics$average_regular_in_degree <- NA
  }
  elite_nodes <- V(STN_i)[V(STN_i)$IS_ELITE == TRUE]
  if (length(elite_nodes) > 0) {
    # In-degree and out-degree for elite nodes
    metrics$average_elite_in_degree <- mean(degree(STN_i, v = elite_nodes, mode = "in"), na.rm = TRUE)
    metrics$average_elite_out_degree <- mean(degree(STN_i, v = elite_nodes, mode = "out"), na.rm = TRUE)
  } else {
    metrics$average_elite_in_degree <- NA
    metrics$average_elite_out_degree <- NA
  }

  # Compute best nodes metrics
  metrics$best_nodes <- sum(V(STN_i)$IS_BEST == TRUE)
  best_ids <- which(V(STN_i)$IS_BEST == TRUE)
  if (length(best_ids) > 0) {
    metrics$average_best_in_degree <- mean(degree(STN_i, v = best_ids, mode = "in"), na.rm = TRUE)
    metrics$average_best_out_degree <- mean(degree(STN_i, v = best_ids, mode = "out"), na.rm = TRUE)
    metrics$best_strength_in <- sum(strength(STN_i, vids = best_ids,  mode="in")) / number_of_runs
    metrics$start_best_nodes <- sum(V(STN_i)$STARTS > 0 & V(STN_i)$IS_BEST == TRUE)
    metrics$standard_best_nodes <- sum(V(STN_i)$STANDARD > 0 & V(STN_i)$IS_BEST == TRUE)
    metrics$end_best_nodes <- sum(V(STN_i)$ENDS > 0 & V(STN_i)$IS_BEST == TRUE)
  } else {
    metrics$average_best_in_degree <- NA
    metrics$average_best_out_degree <- NA
    metrics$best_strength_in <- NA
    metrics$start_best_nodes <- NA
    metrics$standard_best_nodes <- NA
    metrics$end_best_nodes <- NA
  }
  start_ids <- which(V(STN_i)$STARTS > 0)
  if (length(best_ids) > 0 & length(start_ids) > 0) {
    dist_matrix <- distances(STN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
    finite_distances <- dist_matrix[is.finite(dist_matrix)]
    metrics$average_path_length <- mean(finite_distances)
    metrics$paths <- length(finite_distances)
  } else {
    metrics$average_path_length <- NA
    metrics$paths <- NA
  }

  # Compute percentage metrics for nodes
  if (metrics$nodes > 0) {
    metrics$best_nodes_rate <- metrics$best_nodes / metrics$nodes
    metrics$regular_nodes_rate <- metrics$regular_nodes / metrics$nodes
    metrics$elite_nodes_rate <- metrics$elite_nodes / metrics$nodes
    metrics$start_nodes_rate <- metrics$start_nodes / metrics$nodes
    metrics$standard_nodes_rate <- metrics$standard_nodes / metrics$nodes
    metrics$end_nodes_rate <- metrics$end_nodes / metrics$nodes
  } else {
    metrics$best_nodes_rate <- NA
    metrics$regular_nodes_rate <- NA
    metrics$elite_nodes_rate <- NA
    metrics$start_nodes_rate <- NA
    metrics$standard_nodes_rate <- NA
    metrics$end_nodes_rate <- NA
  }

  # Compute percentage metrics for edges
  if (metrics$edges > 0) {
    metrics$worsening_edges_rate <- metrics$worsening_edges / metrics$edges
    metrics$equal_edges_rate <- metrics$equal_edges / metrics$edges
    metrics$improving_edges_rate <- metrics$improving_edges / metrics$edges
  } else {
    metrics$worsening_edges_rate <- NA
    metrics$equal_edges_rate <- NA
    metrics$improving_edges_rate <- NA
  }

  return(metrics)
}

#' Get STN-i metrics focused on elite nodes
#'
#' This function extracts metrics related to elite nodes from an STN-i result.
#' It creates a subgraph containing only elite nodes and calculates metrics
#' similar to `get_stn_i_metrics_nodes()` but specific to the elite subgraph.
#'
#' @param stn_i_result A list containing the STN-i data, typically returned by `stn_i_create()` or `get_stn_i_data()`.
#'
#' @return A list of elite node-focused metrics.
#'
#' @examples
#' \dontrun{
#' elite_node_metrics <- get_stn_i_metrics_elite_nodes(stn_i_result)
#' }
#'
#' @export
get_stn_i_metrics_elite_nodes <- function(stn_i_result) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the STN-i and metadata
  STN_i <- stn_i_result$STN_i
  network_name <- stn_i_result$network_name
  problem_type <- stn_i_result$problem_type
  number_of_runs <- stn_i_result$number_of_runs

  # Filter vertices according to settings - create elite subgraph
  to_remove <- V(STN_i)[IS_ELITE == FALSE]$name
  ESTN_i <- delete_vertices(STN_i, to_remove)

  # Determine BEST nodes and best known solution if not provided
  fitness_vals <- V(ESTN_i)$FITNESS
  V(ESTN_i)$IS_BEST <- FALSE
  best_known_solution <- NA
  if (problem_type == "min") {
    if (!is.na(best_known_solution)) {
      best_known_solution <- as.numeric(best_known_solution)
    } else {
      best_known_solution <- min(fitness_vals, na.rm = TRUE)
    }
    V(ESTN_i)$IS_BEST <- fitness_vals <= best_known_solution
  } else {
    if (!is.na(best_known_solution)) {
      best_known_solution <- as.numeric(best_known_solution)
    } else {
      best_known_solution <- max(fitness_vals, na.rm = TRUE)
    }
    V(ESTN_i)$IS_BEST <- fitness_vals >= best_known_solution
  }

  # Count outgoing regular nodes for each elite node
  V(ESTN_i)$outgoing_regular_nodes <- 0
  V(ESTN_i)$outgoing_improving_regular_nodes <- 0
  V(ESTN_i)$outgoing_equal_regular_nodes <- 0
  V(ESTN_i)$outgoing_worsening_regular_nodes <- 0
  for (i in 1:vcount(ESTN_i)) {
    elite_node_name <- V(ESTN_i)[i]$name
    elite_node_id <- which(V(STN_i)$name == elite_node_name)
    if (length(elite_node_id) > 0) {
      out_edges <- incident(STN_i, elite_node_id, mode = "out")
      regular_count <- 0
      improving_regular <- 0
      equal_regular <- 0
      worsening_regular <- 0
      # Check each outgoing edge
      for (edge_id in out_edges) {
        edge_target <- head_of(STN_i, edge_id)
        # If target is a regular node (not elite)
        if (V(STN_i)[edge_target]$IS_ELITE == FALSE) {
          regular_count <- regular_count + 1
          edge_type <- E(STN_i)[edge_id]$Type
          if (edge_type == "IMPROVING") {
            improving_regular <- improving_regular + 1
          } else if (edge_type == "EQUAL") {
            equal_regular <- equal_regular + 1
          } else if (edge_type == "WORSENING") {
            worsening_regular <- worsening_regular + 1
          }
        }
      }
      V(ESTN_i)[i]$outgoing_regular_nodes <- regular_count
      V(ESTN_i)[i]$outgoing_improving_regular_nodes <- improving_regular
      V(ESTN_i)[i]$outgoing_equal_regular_nodes <- equal_regular
      V(ESTN_i)[i]$outgoing_worsening_regular_nodes <- worsening_regular
    }
  }

  # Direct metrics
  metrics$network_name <- network_name
  metrics$problem_type <- problem_type
  metrics$number_of_runs <- number_of_runs
  metrics$best_known_elite_solution <- best_known_solution
  
  # Compute component metrics
  # Single: components with 1 node
  # Multiple: components with more than 1 node
  comp_data <- components(ESTN_i)
  metrics$elite_components <- comp_data$no
  if (vcount(ESTN_i) > 0) {
    comp_membership <- comp_data$membership
    comp_sizes <- table(comp_membership)
    metrics$single_elite_components <- sum(comp_sizes == 1)
    metrics$multiple_elite_components <- sum(comp_sizes > 1)
  } else {
    metrics$single_elite_components <- 0
    metrics$multiple_elite_components <- 0
  }

  # Compute nodes quantity metrics
  metrics$elite_nodes <- vcount(ESTN_i)
  metrics$elite_best_nodes <- sum(V(ESTN_i)$IS_BEST == TRUE)
  metrics$elite_start_nodes <- sum(V(ESTN_i)$STARTS > 0)
  metrics$elite_standard_nodes <- sum(V(ESTN_i)$STANDARD > 0)
  metrics$elite_end_nodes <- sum(V(ESTN_i)$ENDS > 0)

  # Compute edges quantity metrics
  metrics$elite_edges <- ecount(ESTN_i)
  metrics$elite_to_elite_worsening_edges <- sum(E(ESTN_i)$Type == "WORSENING")
  metrics$elite_to_elite_equal_edges <- sum(E(ESTN_i)$Type == "EQUAL")
  metrics$elite_to_elite_improving_edges <- sum(E(ESTN_i)$Type == "IMPROVING")
  metrics$elite_to_regular_worsening_edges <- sum(V(ESTN_i)$outgoing_worsening_regular_nodes)
  metrics$elite_to_regular_equal_edges <- sum(V(ESTN_i)$outgoing_equal_regular_nodes)
  metrics$elite_to_regular_improving_edges <- sum(V(ESTN_i)$outgoing_improving_regular_nodes)

  # Compute degree metrics for elite nodes
  if (vcount(ESTN_i) > 0) {
    # Total degree (in + out)
    metrics$max_elite_to_elite_degree <- max(degree(ESTN_i, mode = "all"), na.rm = TRUE)
    metrics$min_elite_to_elite_degree <- min(degree(ESTN_i, mode = "all"), na.rm = TRUE)
    metrics$average_elite_to_elite_degree <- mean(degree(ESTN_i, mode = "all"), na.rm = TRUE)

    # In-degree (arcs that enter elite nodes)
    metrics$max_in_elite_to_elite_degree <- max(degree(ESTN_i, mode = "in"), na.rm = TRUE)
    metrics$min_in_elite_to_elite_degree <- min(degree(ESTN_i, mode = "in"), na.rm = TRUE)
    metrics$average_in_elite_to_elite_degree <- mean(degree(ESTN_i, mode = "in"), na.rm = TRUE)
    
    # Out-degree (arcs that leave elite nodes)
    metrics$max_out_elite_to_elite_degree <- max(degree(ESTN_i, mode = "out"), na.rm = TRUE)
    metrics$min_out_elite_to_elite_degree <- min(degree(ESTN_i, mode = "out"), na.rm = TRUE)
    metrics$average_out_elite_to_elite_degree <- mean(degree(ESTN_i, mode = "out"), na.rm = TRUE)
    
    # Outgoing arcs to regular nodes (connections from elite to non-elite): max, min, and average
    metrics$max_out_elite_to_regular_degree <- max(V(ESTN_i)$outgoing_regular_nodes, na.rm = TRUE)
    metrics$min_out_elite_to_regular_degree <- min(V(ESTN_i)$outgoing_regular_nodes, na.rm = TRUE)
    metrics$average_out_elite_to_regular_degree <- mean(V(ESTN_i)$outgoing_regular_nodes, na.rm = TRUE)
  } else {
    metrics$max_elite_to_elite_degree <- NA
    metrics$min_elite_to_elite_degree <- NA
    metrics$average_elite_to_elite_degree <- NA
    metrics$max_in_elite_to_elite_degree <- NA
    metrics$min_in_elite_to_elite_degree <- NA
    metrics$average_in_elite_to_elite_degree <- NA
    metrics$max_out_elite_to_elite_degree <- NA
    metrics$min_out_elite_to_elite_degree <- NA
    metrics$average_out_elite_to_elite_degree <- NA
    metrics$max_out_elite_to_regular_degree <- NA
    metrics$min_out_elite_to_regular_degree <- NA
    metrics$average_out_elite_to_regular_degree <- NA
  }

  # Compute best nodes metrics within elite subgraph
  # best_ids <- which(V(ESTN_i)$IS_BEST == TRUE)
  # if (length(best_ids) > 0) {
  #   metrics$average_best_elite_in_degree <- mean(degree(ESTN_i, v = best_ids, mode = "in"), na.rm = TRUE)
  #   metrics$average_best_elite_out_degree <- mean(degree(ESTN_i, v = best_ids, mode = "out"), na.rm = TRUE)
  #   metrics$best_elite_strength_in <- sum(strength(ESTN_i, vids = best_ids, mode="in")) / number_of_runs
  # } else {
  #   metrics$average_best_elite_in_degree <- NA
  #   metrics$average_best_elite_out_degree <- NA
  #   metrics$best_elite_strength_in <- NA
  # }

  # Compute path length from start nodes to best nodes within elite subgraph
  # start_ids <- which(V(ESTN_i)$STARTS > 0)
  # if (length(best_ids) > 0 & length(start_ids) > 0) {
  #   dist_matrix <- distances(ESTN_i, v = start_ids, to = best_ids, mode = "out", weights = NULL)
  #   finite_distances <- dist_matrix[is.finite(dist_matrix)]
  #   metrics$average_path_length <- mean(finite_distances)
  #   metrics$paths <- length(finite_distances)
  # } else {
  #   metrics$average_path_length <- NA
  #   metrics$paths <- NA
  # }

  # Compute percentage metrics for nodes
  if (metrics$elite_nodes > 0) {
    metrics$best_elite_nodes_rate <- metrics$elite_best_nodes / metrics$elite_nodes
    metrics$start_elite_nodes_rate <- metrics$elite_start_nodes / metrics$elite_nodes
    metrics$standard_elite_nodes_rate <- metrics$elite_standard_nodes / metrics$elite_nodes
    metrics$end_elite_nodes_rate <- metrics$elite_end_nodes / metrics$elite_nodes
  } else {
    metrics$best_elite_nodes_rate <- NA
    metrics$start_elite_nodes_rate <- NA
    metrics$standard_elite_nodes_rate <- NA
    metrics$end_elite_nodes_rate <- NA
  }

  # Compute percentage metrics for edges
  if (metrics$elite_edges > 0) {
    metrics$elite_to_elite_worsening_edges_rate <- metrics$elite_to_elite_worsening_edges / metrics$elite_edges
    metrics$elite_to_elite_equal_edges_rate <- metrics$elite_to_elite_equal_edges / metrics$elite_edges
    metrics$elite_to_elite_improving_edges_rate <- metrics$elite_to_elite_improving_edges / metrics$elite_edges
  } else {
    metrics$elite_to_elite_worsening_edges_rate <- NA
    metrics$elite_to_elite_equal_edges_rate <- NA
    metrics$elite_to_elite_improving_edges_rate <- NA
  }

  return(metrics)
}

#' Get STN-i metrics focused on configurations
#'
#' This function extracts metrics related to configurations from an STN-i result.
#' It includes configuration counts, elite/regular splits, topologies, and correlation metrics.
#'
#' @param stn_i_result A list containing the STN-i data, typically returned by `stn_i_create()` or `get_stn_i_data()`.
#'
#' @return A list of configuration-focused metrics.
#'
#' @examples
#' \dontrun{
#' config_metrics <- get_stn_i_metrics_configurations(stn_i_result)
#' }
#'
#' @export
get_stn_i_metrics_configurations <- function(stn_i_result) {

  # Initialize an empty list to store metrics
  metrics <- list()

  # Obtain the STN-i and metadata
  STN_i <- stn_i_result$STN_i
  network_name <- stn_i_result$network_name
  problem_type <- stn_i_result$problem_type
  best_known_solution <- stn_i_result$best_known_solution
  number_of_runs <- stn_i_result$number_of_runs

  # Direct metrics
  metrics$network_name <- network_name
  metrics$problem_type <- problem_type
  metrics$number_of_runs <- number_of_runs
  metrics$best_known_solution <- best_known_solution

  # Compute configurations metrics
  metrics$configurations <- sum(V(STN_i)$CONFIGURATIONS)
  metrics$regular_configurations <- sum(V(STN_i)$REGULARS)
  metrics$elite_configurations <- sum(V(STN_i)$ELITES)
  metrics$start_configurations <- sum(V(STN_i)$STARTS)
  metrics$standard_configurations <- sum(V(STN_i)$STANDARDS)
  metrics$end_configurations <- sum(V(STN_i)$ENDS)

  # Compute correlation for configurations metrics
  metrics$regular_start_configurations <- sum(V(STN_i)$REGULAR_STARTS)
  metrics$regular_standard_configurations <- sum(V(STN_i)$REGULAR_STANDARDS)
  metrics$regular_end_configurations <- sum(V(STN_i)$REGULAR_ENDS)
  metrics$elite_start_configurations <- sum(V(STN_i)$ELITE_STARTS)
  metrics$elite_standard_configurations <- sum(V(STN_i)$ELITE_STANDARDS)
  metrics$elite_end_configurations <- sum(V(STN_i)$ELITE_ENDS)

  # Compute max/min configurations metrics
  metrics$max_configurations <- max(V(STN_i)$CONFIGURATIONS)
  metrics$min_configurations <- min(V(STN_i)$CONFIGURATIONS)
  metrics$max_regular_configurations <- max(V(STN_i)$REGULARS)
  metrics$min_regular_configurations <- min(V(STN_i)$REGULARS)
  metrics$max_elite_configurations <- max(V(STN_i)$ELITES)
  metrics$min_elite_configurations <- min(V(STN_i)$ELITES)
  metrics$max_start_configurations <- max(V(STN_i)$STARTS)
  metrics$min_start_configurations <- min(V(STN_i)$STARTS)
  metrics$max_standard_configurations <- max(V(STN_i)$STANDARDS)
  metrics$min_standard_configurations <- min(V(STN_i)$STANDARDS)
  metrics$max_end_configurations <- max(V(STN_i)$ENDS)
  metrics$min_end_configurations <- min(V(STN_i)$ENDS)

  # Compute max/min correlation for configurations metrics
  metrics$max_regular_start_configurations <- max(V(STN_i)$REGULAR_STARTS)
  metrics$min_regular_start_configurations <- min(V(STN_i)$REGULAR_STARTS)
  metrics$max_regular_standard_configurations <- max(V(STN_i)$REGULAR_STANDARDS)
  metrics$min_regular_standard_configurations <- min(V(STN_i)$REGULAR_STANDARDS)
  metrics$max_regular_end_configurations <- max(V(STN_i)$REGULAR_ENDS)
  metrics$min_regular_end_configurations <- min(V(STN_i)$REGULAR_ENDS)
  metrics$max_elite_start_configurations <- max(V(STN_i)$ELITE_STARTS)
  metrics$min_elite_start_configurations <- min(V(STN_i)$ELITE_STARTS)
  metrics$max_elite_standard_configurations <- max(V(STN_i)$ELITE_STANDARDS)
  metrics$min_elite_standard_configurations <- min(V(STN_i)$ELITE_STANDARDS)
  metrics$max_elite_end_configurations <- max(V(STN_i)$ELITE_ENDS)
  metrics$min_elite_end_configurations <- min(V(STN_i)$ELITE_ENDS)

  # Compute percentage metrics for configurations and correlation metrics
  if (metrics$configurations > 0) {
    metrics$regular_configurations_rate <- metrics$regular_configurations / metrics$configurations
    metrics$elite_configurations_rate <- metrics$elite_configurations / metrics$configurations
    metrics$start_configurations_rate <- metrics$start_configurations / metrics$configurations
    metrics$standard_configurations_rate <- metrics$standard_configurations / metrics$configurations
    metrics$end_configurations_rate <- metrics$end_configurations / metrics$configurations
    metrics$regular_start_configurations_rate <- metrics$regular_start_configurations / metrics$configurations
    metrics$regular_standard_configurations_rate <- metrics$regular_standard_configurations / metrics$configurations
    metrics$regular_end_configurations_rate <- metrics$regular_end_configurations / metrics$configurations
    metrics$elite_start_configurations_rate <- metrics$elite_start_configurations / metrics$configurations
    metrics$elite_standard_configurations_rate <- metrics$elite_standard_configurations / metrics$configurations
    metrics$elite_end_configurations_rate <- metrics$elite_end_configurations / metrics$configurations
  } else {
    metrics$regular_configurations_rate <- NA
    metrics$elite_configurations_rate <- NA
    metrics$start_configurations_rate <- NA
    metrics$standard_configurations_rate <- NA
    metrics$end_configurations_rate <- NA
    metrics$regular_start_configurations_rate <- NA
    metrics$regular_standard_configurations_rate <- NA
    metrics$regular_end_configurations_rate <- NA
    metrics$elite_start_configurations_rate <- NA
    metrics$elite_standard_configurations_rate <- NA
    metrics$elite_end_configurations_rate <- NA
  }

  return(metrics)
}

#' Save STN-i metrics to a CSV file
#'
#' This function saves a list of STN-i metrics to a CSV file.
#'
#' @param stn_i_metrics A list of metrics extracted from STN-i results, typically returned by `get_stn_i_metrics_nodes()` or `get_stn_i_metrics_configurations()`.
#' @param output_file_path A string specifying the path to the output CSV file where the metrics will be saved.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_stn_i_metrics(stn_i_metrics, "path/to/stn_i_metrics.csv")
#' }
save_stn_i_metrics <- function(stn_i_metrics, output_file_path) {
  # Ensure the file name ends in .csv
  if (!grepl("\\.csv$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".csv")
  }

  # Convert the named list to a data frame: names as columns, values as first row
  metrics_df <- as.data.frame(stn_i_metrics, stringsAsFactors = FALSE)
  # Ensure it's a single row
  if (is.list(metrics_df) && nrow(metrics_df) != 1) {
    metrics_df <- as.data.frame(t(unlist(stn_i_metrics)), stringsAsFactors = FALSE)
  }

  # Save the data frame to a CSV file with semicolon separator and header
  write.table(
    metrics_df,
    file = output_file_path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    dec = "."
  )

  message(paste("STN-i metrics saved to:", output_file_path))
}

# nolint end
