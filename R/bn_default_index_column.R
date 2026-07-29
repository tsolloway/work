#' Resolve the Raw Metric Column a Drivers Dashboard Opens On
#'
#' @description
#' Internal helper. Given the column names of an impact table, returns the one
#' raw metric column the Attribute / Community Drivers dashboard displays on
#' first open for `subgroup`.
#'
#' @details
#' The workbook opens on the first Assess preset (normally "Current Impact" =
#' Average Effect + range shift), and a preset **overrides** `shift_type` --
#' the Analysis and Shift Type cells are driven by Assess and greyed out. Only
#' when no preset can be built does the raw `shift_type` argument decide.
#'
#' That override is why the network-map PNGs cannot reuse
#' \code{bn_impact_write()}'s \code{.resolve_metric_suffix()}: it models
#' neither the preset override nor the \code{headroom} / \code{range} shift
#' tags (it collapses both to \code{propshift}).
#'
#' Kept as a single function so \code{append_bn_impact_dynamic()}'s row
#' pre-sort and \code{append_bn_network_maps()}'s dot sizing resolve the
#' opening view identically and cannot drift apart.
#'
#' @param col_names Character vector. Column names of the impact table.
#' @param subgroup Character or NULL. Subgroup whose columns to resolve, e.g.
#'   \code{"Total"}. NULL treats `col_names` as already unprefixed.
#' @param outcome_display Character. \code{"absolute"} or
#'   \code{"proportional"}.
#' @param shift_type Character. One of \code{"absolute"},
#'   \code{"proportional"}, \code{"headroom"}, \code{"range"}. Ignored
#'   whenever an Assess preset is available.
#'
#' @return A length-1 character column name, or \code{NA_character_} when no
#'   candidate exists.
#'
#' @keywords internal
bn_default_index_column <- function(
    col_names,
    subgroup = NULL,
    outcome_display = "absolute",
    shift_type = "absolute"
) {

  col_names <- as.character(col_names)

  ##############################
  # Metric inventory for this subgroup
  ##############################

  # Bootstrap-stat siblings are statistics *about* a metric, not metrics, and
  # would otherwise bloat the inventory with entries like
  # "lift_0_propdisplay_ci_low".
  if (!is.null(subgroup) && nzchar(subgroup)) {
    sg_cols <- col_names[startsWith(col_names, paste0(subgroup, "_"))]
    sg_cols <- sg_cols[!grepl("_(sd|se|t|ci_low|ci_high|p_value)$", sg_cols)]
    metric_suffixes <- gsub(paste0("^", subgroup, "_"), "", sg_cols)
    sg_prefix <- paste0(subgroup, "_")
  } else {
    metric_suffixes <- setdiff(col_names, c("Variable", "Label", "Community"))
    sg_prefix <- ""
  }

  if (length(metric_suffixes) == 0) return(NA_character_)

  # Collapse every variant tag so each metric appears once. The display tag
  # sits mid-string on brand lift columns ("lift_0_propdisplay_Bing") and at
  # the end on market ones, so strip in both positions.
  .strip_display <- function(x) gsub("_(propdisplay|absdisplay)(_|$)", "\\2", x)
  .strip_shift   <- function(x) gsub("_(propshift|absshift|headshift|rangeshift)(_|$)", "\\2", x)
  base_suffixes  <- unique(.strip_display(.strip_shift(metric_suffixes)))

  all_lift_base_suffixes <- grep("^lift", base_suffixes, value = TRUE)
  market_lift_suffixes   <- grep("^lift$|^lift_\\d+$", all_lift_base_suffixes, value = TRUE)

  has_shift_type  <- any(grepl("_(propshift|absshift|headshift|rangeshift)_", metric_suffixes))
  shift_type_keys <- c("propshift", "absshift", "headshift", "rangeshift")

  ##############################
  # Assess presets (same order append_bn_impact_dynamic builds them)
  ##############################

  avg_key <- {
    hit <- intersect(c("lift_0", "lift"), market_lift_suffixes)
    if (length(hit) > 0) hit[1] else NA_character_
  }

  interv_key <- {
    nz   <- setdiff(market_lift_suffixes, c("lift", "lift_0"))
    pcts <- suppressWarnings(as.numeric(sub("^lift_", "", nz)))
    nz   <- nz[!is.na(pcts)]
    pcts <- pcts[!is.na(pcts)]
    if (length(nz) == 0) NA_character_ else nz[which.min(abs(pcts - 10))]
  }

  preset_metric_keys <- character(0)
  preset_shift_keys  <- character(0)

  if (!is.na(avg_key)) {
    preset_metric_keys <- c(preset_metric_keys, avg_key)
    preset_shift_keys  <- c(preset_shift_keys, "rangeshift")
  }
  if (!is.na(interv_key)) {
    preset_metric_keys <- c(preset_metric_keys, interv_key)
    preset_shift_keys  <- c(preset_shift_keys, "headshift")
  }
  if ("maxVmin" %in% base_suffixes) {
    preset_metric_keys <- c(preset_metric_keys, "maxVmin")
    preset_shift_keys  <- c(preset_shift_keys, "rangeshift")
  }

  # Assess only exists when the engine emitted shift-type variants -- without
  # them a preset's shift key has nothing to bind to.
  has_assess <- length(preset_metric_keys) > 0 && has_shift_type
  if (has_assess) {
    keep <- preset_shift_keys %in% shift_type_keys
    preset_metric_keys <- preset_metric_keys[keep]
    preset_shift_keys  <- preset_shift_keys[keep]
    has_assess <- length(preset_metric_keys) > 0
  }

  ##############################
  # Resolve the opening column
  ##############################

  # Metric dropdown order, consulted only when Assess is suppressed.
  metric_keys <- c(
    market_lift_suffixes,
    if ("maxVmin" %in% base_suffixes) "maxVmin",
    if ("mi" %in% base_suffixes) "mi"
  )
  if (length(metric_keys) == 0) return(NA_character_)

  base_key <- if (has_assess) preset_metric_keys[1] else metric_keys[1]

  shift_key <- if (has_assess) preset_shift_keys[1] else {
    switch(
      shift_type,
      "absolute"     = "absshift",
      "proportional" = "propshift",
      "headroom"     = "headshift",
      "range"        = "rangeshift",
      "propshift"
    )
  }

  disp_key <- if (outcome_display == "absolute") "absdisplay" else "propdisplay"

  # Decreasing tag specificity: lift carries _<shift>_<display>, maxVmin
  # carries _<display> only, mi carries neither. First existing column wins.
  candidates <- c(
    paste0(sg_prefix, base_key, "_", shift_key, "_", disp_key),
    paste0(sg_prefix, base_key, "_", disp_key),
    paste0(sg_prefix, base_key)
  )

  hit <- intersect(candidates, col_names)
  if (length(hit) == 0) return(NA_character_)

  hit[1]
}
