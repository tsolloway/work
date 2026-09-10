#' correspondence_analysis
#'
#' @description Run correspondence analysis on a brand-by-attribute matrix of
#'   means derived from a [stack_data()] result. Means are computed with
#'   [means_summary()] (weighted when `weight` is supplied). Brands or attributes
#'   with thin bases are handled two ways: those below `supplemental_base` are
#'   projected as *supplementary* points (drawn on the map but excluded from the
#'   construction of the axes), and those below `exclude_base` are dropped
#'   entirely before the analysis. Bases are always unweighted respondent counts.
#'
#' @param stack_result List returned by [stack_data()]. Must contain `df_stack`,
#'   `dictionary_stack`, and `var_types`.
#' @param vars Character vector of stacked variable names to use as the
#'   attributes (map columns). Default `NULL` uses every variable tagged as an
#'   IV in `dictionary_stack`.
#' @param weight Character scalar or `NULL`. Name of a weight column in
#'   `df_stack`. When `NULL` (default) means are unweighted; otherwise weighted
#'   means are used. The base thresholds always use unweighted counts.
#' @param normalize Logical. If `TRUE`, each attribute column of the means
#'   matrix is divided by its column total before the analysis, so every
#'   attribute carries equal mass regardless of its scale. Use when mixing
#'   attributes measured on different scales (e.g. 0-10 means alongside 0/1
#'   incidences). Default `FALSE`.
#' @param supplemental_base Numeric. Brands or attributes whose base is below
#'   this value are projected as supplementary points. Default `75`.
#' @param exclude_base Numeric. Brands or attributes whose base is below this
#'   value are excluded before the analysis. Default `40`.
#' @param assigned_only Logical. If `TRUE` (default) and the stack has an
#'   assigner variable, restrict to assigned rows before computing means.
#'
#' @return A named list:
#'   \item{coordinates}{Tibble of principal coordinates (`dim_1`, `dim_2`, ...)
#'     with `point`, `type` (`"brand"`/`"attribute"`), `base`, and
#'     `supplementary`.}
#'   \item{inertia}{Tibble with `dimension`, `singular_value`, `inertia_pct`,
#'     and `inertia_cumulative`.}
#'   \item{matrix}{The brand-by-attribute matrix that was analysed (normalized
#'     when `normalize = TRUE`).}
#'   \item{bases}{Tibble of `point`, `type`, `base`, and `status`
#'     (`"active"`/`"supplementary"`/`"excluded"`).}
#'   \item{ca}{The raw [ca::ca()] object.}
#'   \item{meta}{List of the call settings (`weight`, `supplemental_base`,
#'     `exclude_base`, `assigned_only`).}
#'
#' @export
correspondence_analysis <- function(
    stack_result,
    vars = NULL,
    weight = NULL,
    normalize = FALSE,
    supplemental_base = 75,
    exclude_base = 40,
    assigned_only = TRUE
){

  if (!requireNamespace("ca", quietly = TRUE)) {
    stop("Package 'ca' is required for 'correspondence_analysis'. Please install it.")
  }

  df_stack   <- stack_result[["df_stack"]]
  dictionary <- stack_result[["dictionary_stack"]]
  var_types  <- stack_result[["var_types"]]

  # Attributes default to the IV-tagged variables, in dictionary order.
  if (is.null(vars)) {
    vars <- dictionary %>%
      dplyr::filter(ivs) %>%
      dplyr::pull(var)
  }

  stack_labels <- dictionary %>%
    dplyr::filter(var %in% vars) %>%
    dplyr::arrange(match(var, vars))
  stack_labels <- rlang::set_names(stack_labels[["label"]], stack_labels[["var"]])

  # Optionally restrict to assigned rows.
  assigner_var <- var_types[["assigner"]]
  if (assigned_only && length(assigner_var) > 0) {
    df_stack <- df_stack %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(assigner_var), ~ . == 1))
  }

  # Means + per-cell bases from the means-summary engine (weighted if requested).
  ms <- means_summary(
    df_stack     = df_stack,
    stack_labels = stack_labels,
    weight       = weight
  )

  # Split the wide summary into brand mean columns and their " - N" base columns.
  base_cols <- setdiff(grep(" - N$", names(ms), value = TRUE), "Total - N")
  mean_cols <- sub(" - N$", "", base_cols)
  brand     <- sub("^\\s*\\d+\\s*-\\s*", "", mean_cols)   # strip "{n} - " prefix

  attribute <- ms[["Label"]]

  mean_mat <- t(as.matrix(ms[, mean_cols, drop = FALSE]))  # brands x attributes
  base_mat <- t(as.matrix(ms[, base_cols, drop = FALSE]))
  rownames(mean_mat) <- brand;     colnames(mean_mat) <- attribute
  rownames(base_mat) <- brand;     colnames(base_mat) <- attribute

  # Marginal bases: brand = achieved sample; attribute = stacked respondents.
  brand_base <- apply(base_mat, 1, max, na.rm = TRUE)

  # Exclude brands below the floor, then compute attribute bases on what remains.
  keep_brand      <- brand_base >= exclude_base
  excluded_brands <- names(brand_base)[!keep_brand]

  attr_base      <- colSums(base_mat[keep_brand, , drop = FALSE], na.rm = TRUE)
  keep_attr      <- attr_base >= exclude_base
  excluded_attrs <- names(attr_base)[!keep_attr]

  M <- mean_mat[keep_brand, keep_attr, drop = FALSE]
  brand_base <- brand_base[keep_brand]
  attr_base  <- attr_base[keep_attr]

  if (any(M < 0, na.rm = TRUE)) {
    stop("Correspondence analysis requires non-negative values; the means matrix has negatives.")
  }
  if (nrow(M) < 2 || ncol(M) < 2) {
    stop("Too few brands or attributes remain after applying 'exclude_base'.")
  }

  # Equalize column masses: each attribute column sums to 1, so attributes on
  # different scales (0-10 means vs 0/1 incidences) carry equal weight.
  if (isTRUE(normalize)) {
    col_totals <- colSums(M)
    if (any(col_totals == 0)) {
      stop(paste0("Cannot normalize: attribute(s) with an all-zero column: ",
                  paste(colnames(M)[col_totals == 0], collapse = ", ")))
    }
    M <- sweep(M, 2, col_totals, "/")
  }

  # Supplementary points: retained but below the supplemental threshold.
  supp_brand <- names(brand_base)[brand_base < supplemental_base]
  supp_attr  <- names(attr_base)[attr_base  < supplemental_base]

  suprow <- which(rownames(M) %in% supp_brand)
  supcol <- which(colnames(M) %in% supp_attr)

  # Report what was excluded / made supplementary.
  fmt <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"

  if (length(excluded_brands) || length(excluded_attrs)) {
    cli::cli_alert_warning(
      "Excluded (base < {exclude_base}) — brands: {fmt(excluded_brands)}; attributes: {fmt(excluded_attrs)}"
    )
  }
  if (length(supp_brand) || length(supp_attr)) {
    cli::cli_alert_info(
      "Supplementary (base < {supplemental_base}) — brands: {fmt(supp_brand)}; attributes: {fmt(supp_attr)}"
    )
  }

  # Correspondence analysis (supplementary points projected, not fitted).
  res <- ca::ca(
    M,
    suprow = if (length(suprow)) suprow else NA,
    supcol = if (length(supcol)) supcol else NA
  )

  sv <- res[["sv"]]
  nd <- min(2, length(sv))
  dim_names <- paste0("dim_", seq_len(nd))

  row_pc <- sweep(res[["rowcoord"]][, seq_len(nd), drop = FALSE], 2, sv[seq_len(nd)], "*")
  col_pc <- sweep(res[["colcoord"]][, seq_len(nd), drop = FALSE], 2, sv[seq_len(nd)], "*")
  colnames(row_pc) <- dim_names
  colnames(col_pc) <- dim_names

  rows <- tibble::as_tibble(row_pc) %>%
    dplyr::mutate(
      point         = res[["rownames"]],
      type          = "brand",
      base          = brand_base[res[["rownames"]]],
      supplementary = dplyr::row_number() %in% res[["rowsup"]],
      .before = 1
    )
  cols <- tibble::as_tibble(col_pc) %>%
    dplyr::mutate(
      point         = res[["colnames"]],
      type          = "attribute",
      base          = attr_base[res[["colnames"]]],
      supplementary = dplyr::row_number() %in% res[["colsup"]],
      .before = 1
    )
  coordinates <- dplyr::bind_rows(rows, cols)

  inertia <- tibble::tibble(
    dimension          = seq_along(sv),
    singular_value     = sv,
    inertia_pct        = 100 * sv^2 / sum(sv^2),
    inertia_cumulative = cumsum(100 * sv^2 / sum(sv^2))
  )

  bases <- dplyr::bind_rows(
    tibble::tibble(point = names(brand_base), type = "brand",     base = brand_base),
    tibble::tibble(point = names(attr_base),  type = "attribute", base = attr_base),
    tibble::tibble(point = excluded_brands,   type = "brand",     base = NA_real_),
    tibble::tibble(point = excluded_attrs,    type = "attribute", base = NA_real_)
  ) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        point %in% c(excluded_brands, excluded_attrs) ~ "excluded",
        point %in% c(supp_brand, supp_attr)           ~ "supplementary",
        .default = "active"
      )
    )

  list(
    coordinates = coordinates,
    inertia     = inertia,
    matrix      = M,
    bases       = bases,
    ca          = res,
    meta        = list(
      weight            = weight,
      normalize         = normalize,
      supplemental_base = supplemental_base,
      exclude_base      = exclude_base,
      assigned_only     = assigned_only
    )
  )
}
