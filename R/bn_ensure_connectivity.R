#' Ensure All Nodes Are Connected in a Bayesian Network
#'
#' @description
#' Checks whether the undirected skeleton of a Bayesian network is fully connected.
#' If disconnected components exist, bridges them by adding the strongest bootstrapped
#' arc between each pair of adjacent components.
#'
#' @param bn A `bnlearn::bn` object.
#' @param df A data frame containing the variables in the network.
#' @param algorithm Character. Structure learning algorithm for bootstrap: `"tabu"` or `"hc"`.
#' @param score Character. Scoring function: `"bic"` or `"aic"`.
#' @param black_list Optional data frame of arcs to blacklist.
#' @param white_list Optional data frame of arcs to whitelist.
#' @param bootstrap_reps Integer. Number of bootstrap replicates for bridge arc selection.
#' @param seed Optional integer for reproducibility.
#'
#' @return A `bnlearn::bn` object with bridge arcs added to ensure full connectivity.
#'
#' @details
#' The function:
#' 1. Converts the BN to an undirected igraph and checks connectivity.
#' 2. If connected, returns the BN unchanged.
#' 3. If disconnected, identifies components and bootstraps arc strengths across all
#'    variables to find the strongest candidate arc between each pair of disconnected
#'    components.
#' 4. Adds the best bridge arc per component gap and verifies the result is connected.
#'
#' This is the unsupervised analog to [bn_ensure_reachability()], which enforces
#' directed reachability to a dependent variable.
#'
#' @export
bn_ensure_connectivity <- function(
    bn,
    df,
    algorithm = c("tabu", "hc"),
    score = c("bic", "aic"),
    black_list = NULL,
    white_list = NULL,
    bootstrap_reps = 50,
    seed = 1
) {

  algorithm <- match.arg(algorithm)
  score <- match.arg(score)


  # --- check connectivity on undirected skeleton ---
  g <- bnlearn::as.igraph(bn)
  g_undir <- igraph::as.undirected(g)

  if (igraph::is_connected(g_undir)) {
    message("Network is already fully connected.")
    return(bn)
  }


  # --- identify components ---
  comp <- igraph::components(g_undir)
  n_components <- comp$no
  membership <- comp$membership

  message(
    glue::glue("Network has {n_components} disconnected components. Bridging...")
  )


  # --- bootstrap arc strengths across all variables ---
  if (!is.null(seed)) set.seed(seed)
  boot_arcs <- work::bn_boot_strength(
    df = df,
    bootstrap_reps = bootstrap_reps,
    algorithm = algorithm,
    score = score,
    black_list = black_list,
    white_list = white_list,
    align_direction = TRUE,
    strength_min = 0.01,
    seed = seed
  )


  # --- find best bridge arc between each pair of components ---
  current_arcs <- bnlearn::arcs(bn) %>% as.data.frame()

  for (i in seq_len(n_components - 1)) {
    for (j in seq(from = i + 1, to = n_components)) {

      nodes_i <- names(membership[membership == i])
      nodes_j <- names(membership[membership == j])

      # find candidate arcs that cross these two components
      candidates <- boot_arcs %>%
        dplyr::filter(
          (from %in% nodes_i & to %in% nodes_j) |
          (from %in% nodes_j & to %in% nodes_i)
        ) %>%
        dplyr::arrange(dplyr::desc(strength))

      if (nrow(candidates) == 0) {
        warning(
          glue::glue(
            "No bootstrap candidate found to bridge components {i} and {j}. ",
            "Adding arc between first nodes of each component."
          )
        )

        candidates <- data.frame(
          from = nodes_i[1],
          to = nodes_j[1],
          stringsAsFactors = FALSE
        )
      }

      # take the strongest candidate
      bridge <- candidates[1, c("from", "to")] %>% as.data.frame()
      current_arcs <- dplyr::bind_rows(current_arcs, bridge) %>%
        dplyr::distinct()
    }
  }


  # --- rebuild the bn with bridge arcs ---
  all_nodes <- bnlearn::nodes(bn)
  bn_new <- bnlearn::empty.graph(all_nodes)
  bnlearn::arcs(bn_new) <- as.matrix(current_arcs[, c("from", "to")])


  # --- verify connectivity ---
  g_new <- bnlearn::as.igraph(bn_new)
  g_new_undir <- igraph::as.undirected(g_new)

  if (igraph::is_connected(g_new_undir)) {
    message("Network is now fully connected after bridging.")
  } else {
    warning("Network is still not fully connected after bridging. Manual inspection recommended.")
  }


  return(bn_new)
}
