#' seg_cluster_prototype_seed
#'
#' @description Fits a segmentation solution from a pre-defined prototype seed.
#'   Instead of discovering clusters from scratch (like [seg_cluster_input_sheet()]
#'   or [seg_cluster_with_profiles()]), this function takes an existing set of
#'   segment assignments — the "seed" — and builds the full LDA typing tool
#'   around it.
#'
#'   Typical use case: you have a hand-crafted or modified prototype solution
#'   (e.g., from [seg_prototype_split_segments()]) where segments were manually
#'   merged, split, or reassigned. This function takes those assignments as
#'   ground truth and produces the discriminant analysis, confusion matrix,
#'   accuracy metrics, and segment append columns — everything needed to
#'   evaluate and export the solution.
#'
#'   **Workflow:**
#'   1. Extracts the seed assignments (from `seg$data$with_solutions` or a
#'      provided tibble).
#'   2. Optionally selects polar inputs via greedy Wilks' Lambda
#'      ([get_greedy_vars()]).
#'   3. Reduces inputs via [cluster_reduce_vars()] (greedy_step, falling back
#'      to stepwise if greedy_step fails).
#'   4. Fits LDA twice via [cluster_add_lda()] — once with all inputs, once
#'      with the reduced set.
#'   5. Merges results into the solution family in `seg$solutions$analysis`,
#'      rebuilds the global summary table and segment append columns.
#'
#' @param seg A seg object with data and solutions loaded.
#' @param solution_family_name Character. The solution family key (e.g.
#'   `"D7to5"`). Results are stored under
#'   `seg$solutions$analysis[[solution_family_name]]`.
#' @param seed_name Character. Column name in the seed tibble (and in
#'   `seg$data$with_solutions`) containing the segment assignments
#'   (e.g. `"proto_D7to5"`).
#' @param seed Data frame or `NULL`. Two-column tibble with respondent IDs and
#'   segment assignments. If `NULL`, extracted from `seg$data$with_solutions`
#'   using `resp_id_name` and `seed_name`.
#' @param vars Character vector or `NULL`. Polar input variables for LDA. If
#'   `NULL` and `use_greedy = TRUE`, selected automatically via
#'   [get_greedy_vars()]. If `NULL` and `use_greedy = FALSE`, uses all RS
#'   polars from the spec.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter (e.g. `"okay_filter"`). `NULL` uses all rows.
#' @param use_greedy Logical. If `TRUE` (default), select polar inputs via
#'   greedy Wilks' Lambda when `vars = NULL`.
#' @param use_top_n_polars Integer. Number of top polars to consider during
#'   greedy variable selection (default: `20`).
#' @param reduced_inputs_max Integer or character vector or `NULL`. Cap on the
#'   number of reduced input variables. If numeric, truncates the greedy
#'   selection to this many. If character, uses exactly those variables. `NULL`
#'   uses all selected variables. Default: `14`.
#' @param profile_lda_ratio Numeric between 0 and 1, or `NULL`. Target proportion
#'   of reduced LDA inputs that should come from profile (non-polar) variables.
#'   Rounding favours the polar count. Default: `NULL` (no balancing — all vars
#'   compete equally in the ranked list).
#' @param force_inputs Character vector or `NULL`. Variable names to always
#'   include in the optimised (reduced) LDA input set, even if they are not
#'   ranked in the top N by `cluster_reduce_vars`. Appended after
#'   budget/cap selection. Default: `NULL`.
#' @param priors Character or numeric vector. LDA prior method: `"size"` (default),
#'   `"equal"`, or a numeric vector of length k (one value per segment, will be
#'   normalised to sum to 1). If `NULL`, defaults to `"equal"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column. If `NULL`,
#'   auto-detected via [get_resp_id_name()].
#'
#' @return The seg object with updated `seg$solutions$analysis`,
#'   `seg$solutions$summary_table`, `seg$solutions$df_segment_append`, and
#'   `seg$data$with_solutions`.
#'
#' @export
seg_cluster_prototype_seed <- function(
    seg,
    solution_family_name,
    seed_name,
    seed = NULL,
    vars = NULL,
    filter_name = NULL,
    use_greedy = TRUE,
    use_top_n_polars = 20,
    reduced_inputs_max = 14,
    profile_lda_ratio = NULL,
    force_inputs = NULL,
    priors = NULL,
    resp_id_name = NULL,
    keep_raw = FALSE
){


  # solution_family_name = "D7to5"
  # seed_name = "proto_D7to5"
  # seed = NULL
  # vars = NULL
  # filter_name = NULL
  # reduced_inputs_max = 14
  # priors = "size"
  # resp_id_name = NULL


  if (is.null(priors)) {
    priors <- "equal"
  } else if (is.character(priors)) {
    priors <- match.arg(priors, c("size", "equal"))
  } else if (is.numeric(priors)) {
    priors <- priors / sum(priors)  # normalise
  } else {
    stop("priors must be NULL, 'size', 'equal', or a numeric vector.", call. = FALSE)
  }

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  df <- seg[["data"]][["with_solutions"]]


  if(is.null(seed)){
    seed <- df %>% dplyr::select(dplyr::all_of(c(resp_id_name, seed_name)))
  }
  seed <- seed %>% dplyr::filter(!is.na(.data[[seed_name]]))



  if(!is.null(filter_name)){
    df_temp <- df %>% dplyr::filter(.data[[filter_name]])
  }else if(is.null(filter_name)){
    df_temp <- df
  }


  if( !all(df_temp[[resp_id_name]] %in% seed[[resp_id_name]]) ){
    df_temp <- df_temp %>%
      dplyr::filter(
        df_temp[[resp_id_name]] %in% seed[[resp_id_name]]
      )
  }


  if(use_greedy && is.null(vars)){
    vars <- seg %>% get_greedy_vars(df = df_temp, top = use_top_n_polars, grp = unlist(seed[[seed_name]]))
  }


  if(is.null(vars)){
    vars <- seg %>% seg_get_vars_polars(.return = "rs")
  }

  # derive vars_profiles and polar_var_names from vars via spec lookup
  polar_lookup <- seg_get_vars_polars(seg)
  bad_vars <- intersect(vars, polar_lookup[["profile_var"]])
  if (length(bad_vars) > 0) {
    stop(
      "vars contains polar profile vars — use rs or source vars instead: ",
      paste(bad_vars, collapse = ", "),
      call. = FALSE
    )
  }
  vars_profiles <- purrr::map_chr(vars, function(v) {
    idx <- match(v, polar_lookup[["rs_var"]])
    if (is.na(idx)) idx <- match(v, polar_lookup[["source_var"]])
    if (!is.na(idx)) polar_lookup[["profile_var"]][idx] else v
  })
  polar_var_names <- vars[vars %in% c(polar_lookup[["rs_var"]], polar_lookup[["source_var"]])]



  results <- tibble::tibble(
    n = length(unique(na.omit(seed[[seed_name]]))),
    solution_name = solution_family_name,
    cluster_name = seed_name,
    inputs = list(vars),
    profiles = list(vars_profiles),
    cluster_fit = NA,
    cluster_seed = list(seed %>% setNames(c("id", seed_name))),
    cluster_glance = NA,
    priors_equal = purrr::map(n, ~rep(1/.x, .x)),
    priors_size = ifelse(
      all(priors == "size"),
      purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
      purrr::map(n, ~priors)
    ),
    reduced_inputs = purrr::map2(
      cluster_seed, cluster_name,
      function(x,y)purrr::possibly(
        function(x,y)cluster_reduce_vars(df_temp, vars, x[[y]], type = "greedy_step", return_only_var = TRUE),
        otherwise = cluster_reduce_vars(df_temp, vars, x[[y]], type = "step", return_only_var = TRUE)
      )()) %>% suppressWarnings(),
    reduced_profiles = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
  )


  if(!is.null(reduced_inputs_max)){

    if(is.numeric(reduced_inputs_max)){

      # ---- budget-aware selection when profile_lda_ratio is set ----
      if (!is.null(profile_lda_ratio) && profile_lda_ratio > 0 && profile_lda_ratio < 1 &&
          length(polar_var_names) > 0) {
        # identify which vars are polars vs profiles
        profile_var_names <- setdiff(vars, polar_var_names)

        if (length(profile_var_names) > 0) {
          n_profile_budget <- floor(reduced_inputs_max * profile_lda_ratio)
          n_polar_budget <- reduced_inputs_max - n_profile_budget

          # rank polars and profiles separately so each pool fills its budget
          results <- results %>%
            dplyr::mutate(
              "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, function(cs, cn) {
                grp <- cs[[cn]]
                polar_ranked <- purrr::possibly(
                  ~cluster_reduce_vars(df_temp, polar_var_names[polar_var_names %in% vars], grp,
                                       type = "greedy_step", return_only_var = TRUE),
                  otherwise = polar_var_names[polar_var_names %in% vars]
                )()
                profile_ranked <- purrr::possibly(
                  ~cluster_reduce_vars(df_temp, profile_var_names, grp,
                                       type = "greedy_step", return_only_var = TRUE),
                  otherwise = profile_var_names
                )()
                c(utils::head(polar_ranked, n_polar_budget),
                  utils::head(profile_ranked, n_profile_budget))
              }),
              "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
            )

          n_actual_polar <- sum(results$reduced_inputs[[1]] %in% polar_var_names)
          n_actual_profile <- length(results$reduced_inputs[[1]]) - n_actual_polar
          cli::cli_alert_info(
            "LDA budget split (ratio={profile_lda_ratio}): {n_actual_polar} polar + {n_actual_profile} profile = {n_actual_polar + n_actual_profile}"
          )
        } else {
          results <- results %>%
            dplyr::mutate(
              "reduced_inputs" = purrr::map(reduced_inputs, ~utils::head(.x, reduced_inputs_max)),
              "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
            )
        }
      } else {
        results <- results %>%
          dplyr::mutate(
            "reduced_inputs" = purrr::map(reduced_inputs, ~utils::head(.x, reduced_inputs_max)),
            "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
          )
      }

    }else if(is.character(reduced_inputs_max)){
      results <- results %>%
        dplyr::mutate(
          "reduced_inputs" = list(reduced_inputs_max),
          "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
        )
    }

  }

  # ---- force_inputs: guaranteed slots within the budget ----
  if (!is.null(force_inputs) && length(force_inputs) > 0 && is.numeric(reduced_inputs_max)) {
    results <- results %>%
      dplyr::mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, function(ri) {
          forced   <- intersect(force_inputs, c(ri, force_inputs))
          n_free   <- max(reduced_inputs_max - length(forced), 0)
          ranked   <- utils::head(setdiff(ri, forced), n_free)
          unique(c(ranked, forced))
        }),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
    cli::cli_alert_info("Forced {length(force_inputs)} var{?s} into LDA opt (within budget of {reduced_inputs_max}): {.val {force_inputs}}")
  } else if (!is.null(force_inputs) && length(force_inputs) > 0) {
    # character reduced_inputs_max — just ensure forced vars are present
    results <- results %>%
      dplyr::mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, function(ri) {
          unique(c(ri, setdiff(force_inputs, ri)))
        }),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }


  result_all <- results %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = FALSE
    )


  result_reduced <- results %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = TRUE
    )


  if(is_truthy(seg[["solutions"]][["analysis"]][[solution_family_name]])){
    solution_family_results <- seg[["solutions"]][["analysis"]][[solution_family_name]]
  }else{
    solution_family_results <- list()
  }


  solution_family_results[["result"]][["prototype"]] <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced
  )


  solution_family_results[["solution_table"]] <- solution_family_results[["result"]] %>%
    purrr::discard_at("hierarchical_fit") %>%
    purrr::flatten() %>%
    purrr::map(~dplyr::select(.x, solution_name, n, cluster_name, lda_name, lda_inputs, lda_profiles, lda_fit, lda_coefficient_function, lda_predict, n_segments, accuracy, kappa, cv, collinear, split_half, df_solution)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(df_solution))



  if (!keep_raw) solution_family_results[["result"]] <- NULL

  seg[["solutions"]][["analysis"]][[solution_family_name]] <- solution_family_results



  seg[["solutions"]][["summary_table"]] <- seg_bind_summary_tables(seg)


  seg <- seg_build_with_solutions(seg, resp_id_name = resp_id_name)


  df_return <- seg[["data"]][["with_solutions"]]
  if ("okay_filter" %in% names(df) && !"okay_filter" %in% names(df_return)) {
    seg[["data"]][["with_solutions"]] <- df_return %>%
      dplyr::left_join(
        df %>% dplyr::select(dplyr::all_of(c(resp_id_name, "okay_filter"))),
        by = dplyr::join_by(!!resp_id_name)
      )
  }


  return(seg)

}
