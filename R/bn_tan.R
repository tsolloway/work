#' Fit a Tree-Augmented Naive (TAN) Bayesian Network
#'
#' @description
#' Fits a Tree-Augmented Naive (TAN) Bayesian network using `bnlearn::tree.bayes()`,
#' with optional cross-battery whitelist generation, custom dictionaries,
#' manual community assignments, and visualization preparation.
#'
#' @details
#' The function:
#' 1. Optionally builds a cross-battery whitelist among independent variables (IVs).
#' 2. Fits a TAN model using `bnlearn::tree.bayes()`.
#' 3. Generates arc-level statistics, model summaries, and visualization-ready data.
#'
#' @param df Data frame containing the dependent and independent variables.
#' @param dv Character string naming the dependent variable.
#' @param ivs Character vector naming the independent variables.
#' @param white_list Optional data frame of arcs to force inclusion (`from`, `to`).
#' @param black_list Optional data frame of arcs to exclude (`from`, `to`).
#' @param cross_battery_priority Logical; if `TRUE` (default), constructs a cross-battery whitelist before model fitting.
#' @param compare_to_naive Logical; if `TRUE` (default), includes naive Bayes comparison in the summary.
#' @param dictionary Optional dictionary (data frame, list, or named vector) for node labeling.
#' @param manual_groups Optional vector or mapping for manually assigning node communities.
#' @param node_label_type How to label nodes: `"both"`, `"variable"`, or `"label"`. Default `"both"`.
#' @param n_groups Optional integer specifying number of community clusters for visualization.
#' @param node_size Numeric value controlling relative node size in visualization. Default = 1.
#' @param tool_tip_edge_prefix Optional character string to prepend to edge tooltips (e.g., "MI =").
#' @param suppress_bn_warning Logical; if `TRUE`, suppresses model-fit warnings. Default `FALSE`.
#' @param on_exit_detach_igraph Logical; if `TRUE` (default), detaches igraph-related packages at exit.
#' @param seed Integer random seed for reproducibility. Default = 1.
#'
#' @return A named list with components:
#' \describe{
#'   \item{bn}{Fitted TAN Bayesian network object.}
#'   \item{fit}{Parameterized Bayesian network (`bn.fit`).}
#'   \item{arcs}{Arc-level chi-square statistics from `bn_arc_chisq()`.}
#'   \item{summary}{Model summary statistics and naive comparison.}
#'   \item{viz_prep}{Node and edge data prepared for visualization.}
#'   \item{meta}{Metadata including analysis type.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- bn_tan(
#'   df = iris,
#'   dv = "Species",
#'   ivs = names(iris)[1:4],
#'   seed = 42
#' )
#' }
#'
#' @export
bn_tan <- function(
    df,
    dv,
    ivs,
    white_list = NULL,
    black_list = NULL,
    cross_battery_priority = TRUE,
    compare_to_naive = TRUE,
    dictionary = NULL,
    manual_groups = NULL,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1,
    tool_tip_edge_prefix = NULL,
    suppress_bn_warning = FALSE,
    on_exit_detach_igraph = TRUE,
    seed = 1
){


  # white_list = NULL
  # black_list = NULL
  # cross_battery_priority = TRUE
  # compare_to_naive = TRUE
  # dictionary = NULL
  # manual_groups = NULL
  # node_label_type = "both"
  # n_groups = NULL
  # node_size = 1
  # tool_tip_edge_prefix = NULL
  # suppress_bn_warning = FALSE
  # on_exit_detach_igraph = TRUE
  # seed = 1


  work::start()

  ##############################
  # Setup
  ##############################

  if(!is.null(seed)) set.seed(seed)
  suppressMessages(require(bnlearn, quietly = TRUE))

  node_label_type <- match.arg(node_label_type)

  dictionary <- dictionary %>% dictionary_from_named_object()

  vars <- c(dv, ivs) %>% unlist() %>% setNames(NULL)

  if (!is.null(white_list)) white_list <- as.data.frame(white_list)
  if (!is.null(black_list)) black_list <- as.data.frame(black_list)

  dfx <- df %>%
    dplyr::select(all_of(vars)) %>%
    as.data.frame()



  ##############################
  # model
  ##############################

  if (cross_battery_priority) {

    cb_black_list <- ivs %>%
      map_dfr(make_arcs) %>%
      dplyr::bind_rows(black_list) %>%
      distinct() %>%
      as.data.frame()


    if(!is.null(seed)) set.seed(seed)
    bn <- bnlearn::tree.bayes(
      dfx,
      dv,
      blacklist = cb_black_list,
      whitelist = white_list
    )

  }else if (!cross_battery_priority) {

    if(!is.null(seed)) set.seed(seed)
    bn <- bnlearn::tree.bayes(
      dfx,
      training = dv,
      blacklist = black_list,
      whitelist = white_list
    )
  }


  ##############################
  # results
  ##############################

  results <- list()

  results[["bn"]] <- bn
  results[["fit"]] <- bn %>% bnlearn::bn.fit(dfx, method = "bayes")
  results[["arcs"]] <- bn %>% bn_arc_chisq(dfx, dv = dv)
  results[["meta"]] <- list(analysis = "bn_model_single")


  results[["summary"]] <- bn %>% bn_summary_statistics(
    df = dfx,
    dv = dv,
    fit = results[["fit"]],
    compare_to_naive = compare_to_naive,
    suppress_bn_warning = suppress_bn_warning,
    seed = seed
  )


  results[["viz_prep"]] <- bn_to_netviz_prep(
    obj = results,
    dictionary = dictionary,
    node_label_type = node_label_type,
    manual_groups = manual_groups,
    n_groups = n_groups,
    node_size = node_size,
    on_exit_detach_igraph = FALSE,
    tool_tip_edge_prefix = tool_tip_edge_prefix
  )


  results[["meta"]] <- list(analysis = "bn_model_single", method = "tan")


  # --- cleanup ---
  if(on_exit_detach_igraph){
    detach_igraph()
  }

  return(results)
}
