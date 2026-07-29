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
#' @param dictionary Optional dictionary for label lookup.
#' @param dv Named character vector. DV column(s) in \code{df}.
#' @param ivs Character vector or NULL. IV columns. Default NULL (all).
#' @param process_subgroups Logical. Process subgroups. Default TRUE.
#' @param do_community Logical. If TRUE (default), includes community-level
#'   results. If FALSE, only attribute-level results are returned.
#' @param community_assignment Optional. Community assignment object for
#'   community-level analysis.
#' @param community_impact_attributes Character vector or NULL. Battery names
#'   (variable-name prefixes, e.g. \code{"q14a"}) whose attributes are included
#'   when computing community-level impacts — applied to both the unweighted
#'   and weighted community variants and every community metric within them
#'   (lift, maxVmin, MI, base) across all subgroups and shift types. Default
#'   NULL includes all attributes. Errors if a declared battery matches no IV.
#'   Attribute-level tables are never affected.
#' @param lift Numeric vector. Lift fractions. Default \code{c(0, 0.1)}.
#' @param min_base_for_lift Integer. Minimum sample size for brand lift.
#'   Default 75.
#' @param type Character. Impact type: \code{"gr"}, \code{"cp"}, or
#'   \code{"mi"}. Default \code{"gr"}.
#' @param dv_metric Character. \code{"mean"} or \code{"top_box"}.
#'   Default \code{"mean"}.
#' @section Outcome-display variants:
#'   Both proportional and absolute outcome-display variants of every
#'   DV-change metric are always precomputed and stored. The dashboard's
#'   Outcome Display dropdown switches between them at runtime.
#' @param include_base Logical. Include base sizes. Default TRUE.
#' @param index_by Character. Indexing method. Default \code{"lift_first"}.
#' @param n_boot Integer. Bootstrap replicates. Default 1.
#' @param n_querry Integer. Query sample size. Default 1e4.
#' @param brand Character or NULL. Column name in \code{df} containing a brand
#'   variable. When provided, lift is computed separately for each brand level.
#'   Default NULL.
#' @param brand_names Character vector or NULL. When provided, only compute
#'   brand-specific lift for these brand levels. Default NULL (all brands).
#' @param weight Character scalar or NULL. Column name for weights. When NULL,
#'   weighted variants are omitted from the output.
#' @param mi_boot Integer or NULL. Bootstrap replicates for community MI.
#'   Only applied to community variants. Default 100.
#' @param use_parallel Logical. Use parallel plan if available. Default TRUE.
#' @param boot_nonzero Logical. Default \code{FALSE}: classical bootstrap
#'   inference (replicate SD used directly as the SE, p-values invariant to
#'   \code{n_boot}). \code{TRUE} restores the legacy
#'   \code{se = sd/sqrt(n_boot)} behavior.
#' @param seed Integer. Random seed. Default 1.
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
    dictionary = NULL,
    dv = NULL,
    ivs = NULL,
    process_subgroups = TRUE,
    do_community = TRUE,
    community_assignment = NULL,
    community_impact_attributes = NULL,
    lift = c(0, 0.1),
    min_base_for_lift = 75,
    type = c("gr", "cp", "mi"),
    dv_metric = c("mean", "top_box"),
    include_base = TRUE,
    index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    n_boot = 1,
    n_querry = 1e4,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    mi_boot = 100,
    use_parallel = TRUE,
    scale_ranges = NULL,
    boot_nonzero = FALSE,
    seed = 1
) {

  # Validate community_impact_attributes up front so a bad battery name
  # errors before the (potentially long) attribute runs, not after them.
  # The engine re-validates per call; this is purely a fail-fast check and
  # is skipped when the assignment can't be resolved here (the engine's
  # own resolution then governs).
  if (do_community && !is.null(community_impact_attributes)) {
    ca_check <- community_assignment
    if (is.null(ca_check)) {
      obj_first <- if (process_subgroups) obj[[1]] else obj
      ca_check <- obj_first[["viz_prep"]][["attribute_viz_prep"]][["nodes"]]
    }
    if (!is.null(ca_check)) {
      check_ids <- ca_check[["id"]]
      if (!is.null(ivs)) check_ids <- intersect(check_ids, unlist(ivs, use.names = FALSE))
      invisible(.bn_community_impact_ids(check_ids, community_impact_attributes))
    }
  }

  # Attribute (unweighted)
  if (n_boot == 1) cli::cli_alert_info("Running attribute impact (unweighted)")
  attr_result <- bn_impact(
    obj = obj, df = df, dv = dv, ivs = ivs,
    do_community = FALSE,
    community_assignment = community_assignment,
    type = type, index_by = index_by,
    process_subgroups = process_subgroups,
    dictionary = dictionary,
    n_boot = n_boot, n_querry = n_querry,
    lift = lift,
    brand = brand, brand_names = brand_names,
    min_base_for_lift = min_base_for_lift,
    include_base = include_base,
    dv_metric = dv_metric,
    weight = NULL,
    mi_boot = NULL,
    verbose = FALSE,
    use_parallel = use_parallel,
    scale_ranges = scale_ranges,
    boot_nonzero = boot_nonzero,
    seed = seed
  )

  # Attribute (weighted)
  attr_weighted <- NULL
  if (!is.null(weight)) {
    if (n_boot == 1) cli::cli_alert_info("Running attribute impact (weighted)")
    attr_weighted <- bn_impact(
      obj = obj, df = df, dv = dv, ivs = ivs,
      do_community = FALSE,
      community_assignment = community_assignment,
      type = type, index_by = index_by,
      process_subgroups = process_subgroups,
      dictionary = dictionary,
      n_boot = n_boot, n_querry = n_querry,
      lift = lift,
      brand = brand, brand_names = brand_names,
      min_base_for_lift = min_base_for_lift,
      include_base = include_base,
      dv_metric = dv_metric,
      weight = weight,
      mi_boot = NULL,
      verbose = FALSE,
      use_parallel = use_parallel,
      scale_ranges = scale_ranges,
      boot_nonzero = boot_nonzero,
      seed = seed
    )
  }

  # Community (unweighted)
  comm_result <- NULL
  comm_weighted <- NULL

  if (do_community) {
    if (n_boot == 1) cli::cli_alert_info("Running community impact (unweighted)")
    comm_result <- bn_impact(
      obj = obj, df = df, dv = dv, ivs = ivs,
      do_community = TRUE,
      community_assignment = community_assignment,
      community_impact_attributes = community_impact_attributes,
      type = type, index_by = index_by,
      process_subgroups = process_subgroups,
      dictionary = dictionary,
      n_boot = n_boot, n_querry = n_querry,
      lift = lift,
      brand = brand, brand_names = brand_names,
      min_base_for_lift = min_base_for_lift,
      include_base = include_base,
      dv_metric = dv_metric,
      weight = NULL,
      mi_boot = mi_boot,
      verbose = FALSE,
      use_parallel = use_parallel,
      scale_ranges = scale_ranges,
      boot_nonzero = boot_nonzero,
      seed = seed
    )

    # Community (weighted)
    if (!is.null(weight)) {
      if (n_boot == 1) cli::cli_alert_info("Running community impact (weighted)")
      comm_weighted <- bn_impact(
        obj = obj, df = df, dv = dv, ivs = ivs,
        do_community = TRUE,
        community_assignment = community_assignment,
        community_impact_attributes = community_impact_attributes,
        type = type, index_by = index_by,
        process_subgroups = process_subgroups,
        dictionary = dictionary,
        n_boot = n_boot, n_querry = n_querry,
        lift = lift,
        brand = brand, brand_names = brand_names,
        min_base_for_lift = min_base_for_lift,
        include_base = include_base,
        dv_metric = dv_metric,
        weight = weight,
        mi_boot = mi_boot,
        verbose = FALSE,
        use_parallel = use_parallel,
        scale_ranges = scale_ranges,
        boot_nonzero = boot_nonzero,
        seed = seed
      )
    }
  }

  # The unweighted attribute run is what we borrow meta from, so its
  # meta$weight is always NULL even if the user supplied one. Stamp the
  # caller's weight onto the merged meta so downstream writers (e.g.
  # append_bn_simulator) can see it.
  merged_meta <- attr_result[["meta"]]
  merged_meta[["weight"]] <- weight

  list(
    table_attribute          = attr_result[["table"]],
    table_attribute_weighted = if (!is.null(attr_weighted)) attr_weighted[["table"]] else NULL,
    table_community          = if (!is.null(comm_result)) comm_result[["table"]] else NULL,
    table_community_weighted = if (!is.null(comm_weighted)) comm_weighted[["table"]] else NULL,
    meta                     = merged_meta
  )
}
