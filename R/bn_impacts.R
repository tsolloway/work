#' bn_impacts
#'
#' @description Convenience wrapper around \code{bn_impact()} that runs all
#'   four variants (attribute, attribute weighted, community, community weighted)
#'   and returns them in a single list. Shares all parameters with
#'   \code{bn_impact()}.
#'
#' @param obj A named list of BN subgroup objects, as produced by
#'   \code{bn_finalize_network()$bn_subgroups}.
#' @param df Data frame used to fit the network.
#' @param do_community Logical. If TRUE (default), includes community-level
#'   results. If FALSE, only attribute-level results are returned.
#' @param weight Character scalar or NULL. Column name for weights. When NULL,
#'   weighted variants are omitted from the output.
#' @param community_assignment Optional. Community assignment object for
#'   community-level analysis.
#' @param mi_boot Integer or NULL. Bootstrap replicates for community MI.
#'   Only applied to community variants.
#' @param brand_names Character vector or NULL. When provided, only compute
#'   brand-specific lift for these brand levels. Brands not in this vector are
#'   skipped. Market-level lift is always computed. Default NULL (all brands).
#' @param ... Additional arguments passed to \code{bn_impact()} (e.g.,
#'   \code{dv}, \code{type}, \code{n_boot}, \code{lift}, \code{brand},
#'   \code{dictionary}, \code{impact_metric_type}, \code{dv_metric}, etc.).
#'
#' @return A list with:
#' \describe{
#'   \item{table_attribute}{Attribute-level results (unweighted).}
#'   \item{table_attribute_weighted}{Attribute-level results (weighted). NULL if
#'     \code{weight} is NULL.}
#'   \item{table_community}{Community-level results (unweighted).}
#'   \item{table_community_weighted}{Community-level results (weighted). NULL if
#'     \code{weight} is NULL.}
#'   \item{meta}{Shared metadata from the attribute-level run.}
#' }
#'
#' @seealso [bn_impact()], [bn_impact_engine()], [bn_impact_write()]
#'
#' @export
bn_impacts <- function(
    obj,
    df,
    do_community = TRUE,
    weight = NULL,
    community_assignment = NULL,
    mi_boot = NULL,
    brand_names = NULL,
    ...
) {

  # Attribute (unweighted)
  cli::cli_alert_info("Running attribute impact (unweighted)")
  attr_result <- bn_impact(
    obj = obj, df = df,
    do_community = FALSE,
    community_assignment = community_assignment,
    weight = NULL,
    brand_names = brand_names,
    ...
  )

  # Attribute (weighted)
  attr_weighted <- NULL
  if (!is.null(weight)) {
    cli::cli_alert_info("Running attribute impact (weighted)")
    attr_weighted <- bn_impact(
      obj = obj, df = df,
      do_community = FALSE,
      community_assignment = community_assignment,
      weight = weight,
      brand_names = brand_names,
      ...
    )
  }

  # Community (unweighted)
  comm_result <- NULL
  comm_weighted <- NULL

  if (do_community) {
    cli::cli_alert_info("Running community impact (unweighted)")
    comm_result <- bn_impact(
      obj = obj, df = df,
      do_community = TRUE,
      community_assignment = community_assignment,
      weight = NULL,
      brand_names = brand_names,
      mi_boot = mi_boot,
      ...
    )

    # Community (weighted)
    if (!is.null(weight)) {
      cli::cli_alert_info("Running community impact (weighted)")
      comm_weighted <- bn_impact(
        obj = obj, df = df,
        do_community = TRUE,
        community_assignment = community_assignment,
        weight = weight,
        brand_names = brand_names,
        mi_boot = mi_boot,
        ...
      )
    }
  }

  list(
    table_attribute          = attr_result[["table"]],
    table_attribute_weighted = if (!is.null(attr_weighted)) attr_weighted[["table"]] else NULL,
    table_community          = if (!is.null(comm_result)) comm_result[["table"]] else NULL,
    table_community_weighted = if (!is.null(comm_weighted)) comm_weighted[["table"]] else NULL,
    meta                     = attr_result[["meta"]]
  )
}
