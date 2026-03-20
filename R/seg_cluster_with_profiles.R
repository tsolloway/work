#' seg_cluster_with_profiles
#'
#' @description Runs k-means clustering using a mix of polar and profile
#'   variables as inputs. Unlike [seg_cluster_input_sheet()] which clusters on
#'   polar variables only and evaluates profiles post-hoc, this function feeds
#'   profile variables directly into the clustering algorithm so they influence
#'   segment formation.
#'
#'   Typical use case: you have a set of polar batteries plus behavioural or
#'   attitudinal profile items (e.g., usage, territory, demographics) that you
#'   want to actively shape the segments — not just describe them after the fact.
#'
#' @param seg A seg object with data and spec loaded.
#' @param solution_previous Character. Letter of an existing solution family
#'   (e.g., `"A"`) whose polar inputs should be reused. If provided, overrides
#'   `inputs_polars` with the RS variables from that family.
#' @param solution_name Character. Name for this solution (e.g., `"H"`). Used as
#'   the key in `seg$solutions$analysis`.
#' @param inputs_polars Character vector. Specific polar variables (RS or source)
#'   to cluster on. If `NULL` and `use_greedy = TRUE`, greedy LDA selects them
#'   automatically. If `NULL` and `use_greedy = FALSE`, all RS polars are used.
#' @param inputs_profiles Character vector. Profile shell variables to include
#'   as clustering inputs (e.g., `c("BT14", "BT15")`).
#' @param include_polars_lda Logical. Include polar variables in the LDA
#'   discriminant analysis (default: `TRUE`).
#' @param include_profiles_lda Logical. Include profile variables in the LDA
#'   discriminant analysis (default: `TRUE`).
#' @param resp_id_name Character. Respondent ID column name. If `NULL`, auto-
#'   detected via [get_resp_id_name()].
#' @param filter_logical_vector Logical vector or character column name used to
#'   subset respondents before clustering.
#' @param ok_filter Logical. If `TRUE` (default), applies the variability filter
#'   ([seg_cluster_variability()]) to exclude flat-liners.
#' @param use_greedy Logical. If `TRUE` (default), uses greedy LDA
#'   ([get_greedy_vars()]) to select the best polar inputs when
#'   `inputs_polars = NULL` and `solution_previous = NULL`.
#' @param n_min Integer. Minimum number of clusters (default: 4).
#' @param n_max Integer. Maximum number of clusters (default: 7).
#' @param reduced_inputs_max Integer. Maximum variables for greedy LDA reduced
#'   input set (default: 16).
#' @param vary_percent Numeric. Minimum proportion of non-modal responses for
#'   the variability filter (default: 0.1).
#' @param side_bias_percent Numeric. Maximum allowable side-bias proportion for
#'   the variability filter (default: 0.1).
#' @param priors Character. LDA prior method: `"size"` (default) or `"equal"`.
#' @param iter_max Integer. Maximum k-means iterations (default: 1000).
#' @param nstart Integer. Number of random starts for k-means (default: 10).
#' @param remove_nzv Logical. If `TRUE` (default), removes near-zero variance
#'   profile variables before clustering via [caret::nearZeroVar()].
#' @param cor_threshold Numeric or `NULL`. If numeric, removes collinear profile
#'   variables where `|r|` exceeds this threshold via [caret::findCorrelation()].
#'   Default: `0.90`. Set to `NULL` to skip.
#'
#' @return The seg object with updated `seg$solutions$analysis[[solution_name]]`,
#'   `seg$solutions$summary_table`, `seg$solutions$df_segment_append`, and
#'   `seg$data$with_solutions`.
#'
#' @export
seg_cluster_with_profiles <- function(
    seg,
    solution_previous = NULL,
    solution_name = NULL,
    inputs_polars = NULL,
    inputs_profiles = NULL,
    include_polars_lda = TRUE,
    include_profiles_lda = TRUE,
    resp_id_name = NULL,
    filter_logical_vector = NULL,
    ok_filter = TRUE,
    use_greedy = TRUE,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = 16,
    vary_percent = .1,
    side_bias_percent = .1,
    priors = c("size", "equal"),
    iter_max = 1000,
    nstart = 10,
    remove_nzv = TRUE,
    cor_threshold = 0.90
){

  # solution_previous = NULL
  # solution_name = "H"
  # inputs_polars = NULL
  # # inputs_profiles = c("BT14", "BT15", "BT16", seg_get_vars_profiles(seg, c("TER", "USE")))
  # resp_id_name = NULL
  # filter_logical_vector = NULL
  # ok_filter = TRUE
  # use_greedy = TRUE
  # n_min = 4
  # n_max = 7
  # reduced_inputs_max = 16
  # vary_percent = .1
  # side_bias_percent = .1
  # priors = "size"
  # iter_max = 1000
  # nstart = 10
  # include_polars_lda = T
  # include_profiles_lda = T


  if (is.null(solution_name)) stop("solution_name is required.", call. = FALSE)

  priors <- match.arg(priors)

  cli::cli_h2("seg_cluster_with_profiles")
  cli::cli_alert_info("Solution: {.val {solution_name}} | K range: {n_min}\u2013{n_max} | Priors: {.val {priors}}")

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  if(!is.null(solution_previous)){
    if(!solution_previous %in% names(seg[["solutions"]][["inputs"]])){
      stop("solution_previous not in seg object")
    }
    inputs_polars <- seg[["solutions"]][["inputs"]][[solution_previous]][["RS"]]
  }


  df <- seg[["data"]][["with_shell"]]


  # filter data if available
  if(!is.null(filter_logical_vector)){

    if(is.character(filter_logical_vector)){

      df <- df %>% dplyr::filter(.data[[filter_logical_vector]])

    }else if(is.logical(filter_logical_vector)){

      df <- df %>% dplyr::filter(filter_logical_vector)
    }
  }


  if(ok_filter){
    filter_name <- "okay_filter"

    if(!filter_name %in% names(df)){
      cli::cli_alert("Computing variability & side-bias filters\u2026")

      df <- df %>%
        seg_cluster_variability(
          vars = seg_get_vars_polars(seg, .return="rs"),
          vary_percent = vary_percent,
          side_bias_percent = side_bias_percent
        )

      df <- df[["df"]]
    }

  }else{
    filter_name <- NULL
  }


  if(use_greedy && is.null(solution_previous) && is.null(inputs_polars)){
    cli::cli_alert("Selecting polar inputs via greedy Wilks\u2019 Lambda\u2026")
    inputs_polars <- seg %>% get_greedy_vars(df=df, filter_name = filter_name)
  }


  if(is.null(inputs_polars)){
    all_inputs_polars <- seg %>% seg_get_vars_polars(.return = "rs")
  }else{
    all_inputs_polars <- inputs_polars
  }


  all_inputs_polars_profiles <- seg_get_vars_polars(seg, .return = "all") %>%
    dplyr::filter(
      rs_var %in% all_inputs_polars | source_var %in% all_inputs_polars
    ) %>%
    dplyr::select(profile_var) %>%
    unlist() %>%
    setNames(NULL)


  # ---- clean inputs_profiles: remove near-zero variance & collinear ----
  if (!is.null(inputs_profiles) && length(inputs_profiles) > 0) {
    n_orig <- length(inputs_profiles)

    # Build complete-case profile matrix (matches what k-means will see)
    profile_mat <- df %>%
      dplyr::select(dplyr::all_of(inputs_profiles)) %>%
      as.data.frame() %>%
      stats::na.omit()

    if (remove_nzv) {
      nzv_idx <- caret::nearZeroVar(profile_mat)
      if (length(nzv_idx) > 0) {
        nzv_names <- inputs_profiles[nzv_idx]
        cli::cli_alert_warning("Removing {length(nzv_names)} near-zero variance profile var{?s}: {.val {nzv_names}}")
        inputs_profiles <- inputs_profiles[-nzv_idx]
        profile_mat <- profile_mat[, -nzv_idx, drop = FALSE]
      }
    }

    if (!is.null(cor_threshold) && length(inputs_profiles) > 1) {
      # Drop any zero-variance columns
      zv_mask <- vapply(profile_mat, function(x) stats::var(x) > 0, logical(1))
      if (any(!zv_mask)) {
        zv_names <- names(profile_mat)[!zv_mask]
        cli::cli_alert_warning("Removing {length(zv_names)} zero-variance profile var{?s}: {.val {zv_names}}")
        inputs_profiles <- setdiff(inputs_profiles, zv_names)
        profile_mat <- profile_mat[, zv_mask, drop = FALSE]
      }
      if (ncol(profile_mat) > 1) {
        cor_mat <- stats::cor(profile_mat)
        cor_idx <- caret::findCorrelation(cor_mat, cutoff = cor_threshold)
        if (length(cor_idx) > 0) {
          cor_names <- names(profile_mat)[cor_idx]
          cli::cli_alert_warning("Removing {length(cor_names)} collinear profile var{?s} (|r| > {cor_threshold}): {.val {cor_names}}")
          inputs_profiles <- setdiff(inputs_profiles, cor_names)
        }
      }
    }

    n_kept <- length(inputs_profiles)
    if (n_kept < n_orig) {
      cli::cli_alert_info("Profile inputs: {n_orig} \u2192 {n_kept}")
    }

    if (length(inputs_profiles) == 0) {
      cli::cli_alert_warning("All profile inputs removed \u2014 clustering on polars only")
      inputs_profiles <- NULL
    }
  }

  cluster_vars <- c(all_inputs_polars, inputs_profiles)
  cluster_profiles <- c(all_inputs_polars_profiles, inputs_profiles)



  if(include_polars_lda && include_profiles_lda){

    lda_vars <- c(all_inputs_polars, inputs_profiles)
    lda_profiles <- c(all_inputs_polars_profiles, inputs_profiles)

  }else if(!include_polars_lda && include_profiles_lda){

    lda_vars <- inputs_profiles
    lda_profiles <- inputs_profiles

  }else if(include_polars_lda && !include_profiles_lda){

    lda_vars <- all_inputs_polars
    lda_profiles <- all_inputs_polars_profiles

  }else if(!include_polars_lda && !include_profiles_lda){

    stop("Both include_polars_lda and include_profiles_lda cannot be false")

  }


  n_cluster_vars <- length(cluster_vars)
  n_lda_vars <- length(lda_vars)
  cli::cli_alert_info("Clustering on {n_cluster_vars} var{?s} | LDA on {n_lda_vars} var{?s}")
  cli::cli_alert("Running k-means for K = {n_min}\u2013{n_max}\u2026")

  results <- withCallingHandlers(
    cluster_kmeans(
      df = df,
      vars = cluster_vars,
      vars_profiles = cluster_profiles,
      solution_name = solution_name,
      lda_vars = lda_vars,
      lda_vars_profiles = lda_profiles,
      reduced_inputs_max = reduced_inputs_max,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      n_min = n_min,
      n_max = n_max,
      priors = priors,
      iter_max = iter_max,
      nstart = nstart
    ),
    warning = function(w) {
      message("cluster_kmeans warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )


  cli::cli_alert("Building solution family & merging results\u2026")
  # ---- build new solution family ----
  new_result <- list(kmeans = results)

  solution_table <- new_result %>%
    purrr::flatten() %>%
    purrr::map(
      ~dplyr::select(
        .x, solution_name, n, cluster_name,
        lda_name, lda_inputs, lda_profiles,
        lda_coefficient_function, lda_predict,
        confusion, accuracy, kappa, cv, collinear, split_half, df_append
      )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(df_append))

  df_segment_append <- solution_table %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    purrr::reduce(function(x, y) {
      x %>%
        dplyr::select(!dplyr::any_of(setdiff(names(y), "id"))) %>%
        dplyr::left_join(y, by = "id")
    })

  solution_table <- solution_table %>% dplyr::select(-df_append)

  solutions_new <- list(
    result = new_result,
    solution_table = solution_table,
    df_segment_append = df_segment_append
  )


  # ---- merge into existing solutions ----
  existing_family <- seg[["solutions"]][["analysis"]][[solution_name]]

  if (!is.null(existing_family)) {
    # result: overwrite matching keys, keep the rest
    merged_result <- existing_family[["result"]]
    for (rn in names(solutions_new[["result"]])) {
      merged_result[[rn]] <- solutions_new[["result"]][[rn]]
    }

    # solution_table: drop old rows whose lda_name appears in new, then bind
    new_lda_names <- solutions_new[["solution_table"]][["lda_name"]]
    merged_st <- existing_family[["solution_table"]] %>%
      dplyr::filter(!lda_name %in% new_lda_names) %>%
      dplyr::bind_rows(solutions_new[["solution_table"]])

    # df_segment_append: drop old columns that appear in new, then bind
    new_seg_cols <- setdiff(names(solutions_new[["df_segment_append"]]), "id")
    merged_df <- existing_family[["df_segment_append"]] %>%
      dplyr::select(!dplyr::any_of(new_seg_cols)) %>%
      dplyr::left_join(solutions_new[["df_segment_append"]], by = "id")

    seg[["solutions"]][["analysis"]][[solution_name]] <- list(
      result = merged_result,
      solution_table = merged_st,
      df_segment_append = merged_df
    )
  } else {
    seg[["solutions"]][["analysis"]][[solution_name]] <- solutions_new
  }


  # ---- rebuild global summary_table and df_segment_append ----
  global_solution_table <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows()

  global_df_segment_append <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "df_segment_append") %>%
    purrr::reduce(dplyr::full_join, by = "id")


  # ---- rebuild with_solutions from with_shell + all segment columns ----
  df_return <- dplyr::left_join(
    seg[["data"]][["with_shell"]],
    global_df_segment_append,
    by = dplyr::join_by(!!resp_id_name == id)
  )

  # preserve extra columns already on with_solutions (e.g., from other functions)
  if (!is.null(seg[["data"]][["with_solutions"]])) {
    existing_cols <- names(seg[["data"]][["with_solutions"]])
    new_cols <- names(df_return)
    extra_cols <- setdiff(existing_cols, new_cols)

    if (length(extra_cols) > 0) {
      df_return <- dplyr::left_join(
        df_return,
        seg[["data"]][["with_solutions"]] %>%
          dplyr::select(dplyr::all_of(c(resp_id_name, extra_cols))),
        by = dplyr::join_by(!!resp_id_name)
      )
    }
  }

  if ("okay_filter" %in% names(df) && !"okay_filter" %in% names(df_return)) {
    df_return <- dplyr::left_join(
      df_return,
      df %>% dplyr::select(dplyr::all_of(c(resp_id_name, "okay_filter"))),
      by = dplyr::join_by(!!resp_id_name)
    )
  }


  seg[["solutions"]][["summary_table"]] <- global_solution_table
  seg[["solutions"]][["df_segment_append"]] <- global_df_segment_append
  seg[["data"]][["with_solutions"]] <- df_return

  n_solutions <- nrow(solution_table)
  cli::cli_alert_success("Done \u2014 {n_solutions} solution{?s} for family {.val {solution_name}}")

  return(seg)

}
