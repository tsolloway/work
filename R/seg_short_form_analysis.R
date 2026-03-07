#' seg_short_form_analysis
#'
#' @description Performs short-form typing tool analysis on a segmentation
#'   solution. Iteratively reduces the number of LDA input variables from the
#'   full set down to the number of segments, re-fitting the LDA at each step.
#'   Returns accuracy metrics (overall + per-segment precision/recall) at each
#'   item count so you can identify the minimum number of items needed for an
#'   acceptable typing tool.
#'
#'   Uses [cluster_reduce_vars()] with greedy stepwise selection to rank
#'   variables by discriminant importance, then refits LDA at each item count
#'   from full down to k (number of segments).
#'
#' @param seg A seg object with solutions in `seg$solutions$summary_table`.
#' @param solution Character. Name of the solution to analyze. Matched first
#'   against `solution_name`, then against `lda_name`.
#' @param reorder Optional numeric vector to reorder segments before analysis.
#'   Maps old segment numbers to new (e.g., `c(6,2,3,4,5,1)` means old
#'   segment 1 becomes new segment 6, old 6 becomes new 1). If provided, calls
#'   [seg_reorder_solution()] first.
#' @param solution_new Character. Name for the reordered solution. Required if
#'   `reorder` is not NULL. Defaults to `NULL`.
#' @param id_name Character. Name of the respondent ID column. Defaults to
#'   `seg$meta$id_variable`.
#' @param priors Character. Prior probabilities for LDA. One of `"equal"` or
#'   `"size"`. Defaults to `"equal"`.
#' @param reduce_type Character. Variable reduction strategy passed to
#'   [cluster_reduce_vars()]. One of `"greedy_step"`, `"greedy"`, or `"step"`.
#'   Defaults to `"greedy_step"`.
#' @param remove_first Character vector. Variables to remove first (before
#'   the greedy ranking kicks in). Listed in removal order — first element
#'   is removed first. Defaults to `NULL`.
#' @param min_items Integer. Minimum number of items to evaluate. Stops the
#'   analysis at this item count or at failure, whichever comes first.
#'   Defaults to `NULL` (go as low as possible).
#' @param seed Numeric. Random seed for reproducibility. Defaults to `1`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{results}{Full results tibble with columns: `n` (item count),
#'       `inputs`, `lda_inputs`, `lda_fit`, `lda_coefficient_function`,
#'       `lda_seg`, `confusion`, `accuracy_overall`, `accuracy_seg`.}
#'     \item{analysis}{Summary tibble with columns: `Items`,
#'       `Overall_Accuracy`, and per-segment `Precision_Seg_*` / `Recall_Seg_*`.}
#'   }
#'
#' @export
seg_short_form_analysis <- function(
    seg,
    solution,
    reorder = NULL,
    solution_new = NULL,
    id_name = NULL,
    priors = c("equal", "size"),
    reduce_type = c("greedy_step", "greedy", "step"),
    remove_first = NULL,
    min_items = NULL,
    seed = 1
) {

  # seg = seg
  # solution = "LDA_db_final_classification"
  # reorder = NULL
  # solution_new = NULL
  # id_name = NULL
  # priors = "equal"
  # reduce_type = "greedy_step"
  # seed = 1

  if(is.character(priors))priors <- match.arg(priors)

  reduce_type <- match.arg(reduce_type)

  if (!is.null(reorder) && is.null(solution_new)) {
    stop("`solution_new` is required when `reorder` is provided.")
  }

  id_name <- id_name %||% seg[["meta"]][["id_variable"]]

  if (is.null(id_name)) {
    stop("Could not determine ID variable. Set `id_name` or ensure `seg$meta$id_variable` exists.")
  }


  # -- reorder if requested --
  if (!is.null(reorder)) {
    seg <- seg_reorder_solution(
      seg = seg,
      solution_old = solution,
      solution_new = solution_new,
      new_order = reorder
    )
    solution <- solution_new
  }


  df <- seg[["data"]][["with_solutions"]]
  summary_table <- seg[["solutions"]][["summary_table"]]

  if (is.null(summary_table)) {
    stop("seg$solutions$summary_table is NULL.")
  }

  # -- extract the target solution row --
  # match on solution_name first, then lda_name
  target <- summary_table %>% dplyr::filter(solution_name == !!solution)

  if (nrow(target) == 0) {
    target <- summary_table %>% dplyr::filter(lda_name == !!solution)
  }

  if (nrow(target) == 0) {
    stop(glue::glue("Solution '{solution}' not found in summary_table (checked solution_name and lda_name)."))
  }

  if (nrow(target) > 1) {
    cli::cli_warn("Multiple rows matched solution '{solution}'. Using the first.")
    target <- target[1, ]
  }


  # -- pull what we need from the target row --
  lda_input_vars <- target[["lda_inputs"]] %>% unlist()
  cluster_name <- target[["cluster_name"]]
  lda_name <- target[["lda_name"]]

  # train on raw cluster assignments (matches how cluster_add_lda works)
  df_complete <- df %>% dplyr::filter(!is.na(.data[[cluster_name]]))
  cluster_grp <- df_complete[[cluster_name]] %>% unlist()

  cli::cli_alert_info("Reducing {length(lda_input_vars)} variables with '{reduce_type}' strategy...")

  ranked_vars <- cluster_reduce_vars(
    df = df_complete,
    vars = lda_input_vars,
    grp = cluster_grp,
    type = reduce_type,
    return_only_var = TRUE,
    seed = seed
  )

  # append any input vars not selected by the reduction (ranked ones first)
  vars <- c(ranked_vars, setdiff(lda_input_vars, ranked_vars))

  # move remove_first vars to the end so they're dropped first
  if (!is.null(remove_first)) {
    vars <- c(setdiff(vars, remove_first), rev(remove_first))
  }

  n_segments <- length(unique(cluster_grp))
  n_min <- max(min_items %||% 1, 1)
  n_seq <- seq(length(vars), n_min)

  cli::cli_alert_info("Fitting LDA at {length(n_seq)} item counts ({max(n_seq)} down to {min(n_seq)})...")

  # -- set up priors --
  if (is.numeric(priors)) {
    if (length(priors) != n_segments) {
      stop(glue::glue("Length of `priors` ({length(priors)}) must match number of segments ({n_segments})."))
    }
    lda_priors <- priors / sum(priors)
  } else if (priors == "equal") {
    lda_priors <- rep(1 / n_segments, n_segments)
  } else {
    seg_counts <- table(cluster_grp)
    lda_priors <- as.numeric(seg_counts) / sum(seg_counts)
  }

  # -- filter to rows with non-NA input vars + non-NA lda classification --
  df_predict <- df %>%
    dplyr::filter(
      !is.na(.data[[lda_name]]),
      dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.x))
    )

  # -- iterative LDA refitting --
  results_list <- purrr::map(n_seq, function(items) {
    input_vars <- vars %>% head(items)

    res <- tryCatch({
      set.seed(seed)
      fit <- MASS::lda(
        x = df_complete %>% dplyr::select(dplyr::all_of(input_vars)),
        grouping = cluster_grp,
        prior = lda_priors
      )
      coef <- coefficient_lda(
        fit = fit,
        input = df_complete %>% dplyr::select(dplyr::all_of(input_vars)),
        grp = cluster_grp
      )
      pred <- predict(fit, df_predict %>% dplyr::select(dplyr::all_of(input_vars)))
      list(lda_fit = fit, lda_coefficient_function = coef, lda_predict = pred)
    }, error = function(e) {
      cli::cli_warn("LDA failed at {items} items: {e$message}")
      NULL
    })
    if (is.null(res)) return(NULL)

    col_name <- glue::glue("{lda_name}_items{items}")
    seg_col <- tibble::tibble(
      id = df_predict[[id_name]],
      seg_col = as.numeric(as.character(res$lda_predict[["class"]]))
    )
    names(seg_col)[2] <- col_name

    ref_levels <- levels(as.factor(df_predict[[lda_name]]))
    conf <- tryCatch(
      caret::confusionMatrix(
        factor(seg_col[[2]], levels = ref_levels),
        factor(df_predict[[lda_name]], levels = ref_levels)
      ),
      error = function(e) {
        cli::cli_warn("confusionMatrix failed at {items} items: {e$message}")
        NULL
      }
    )
    if (is.null(conf)) return(NULL)

    tibble::tibble(
      n = items,
      inputs = list(input_vars),
      lda_fit = list(res$lda_fit),
      lda_inputs = list(input_vars),
      lda_coefficient_function = list(res$lda_coefficient_function),
      lda_seg = list(seg_col),
      confusion = list(conf),
      accuracy_overall = conf %>% purrr::pluck("overall") %>% purrr::pluck("Accuracy"),
      accuracy_seg = list(
        conf %>%
          purrr::pluck("byClass") %>%
          tibble::as_tibble() %>%
          dplyr::mutate(Segment = glue::glue("Seg_{seq(dplyr::n())}")) %>%
          dplyr::select(Segment, Precision, Recall)
      )
    )
  })

  results <- purrr::compact(results_list) %>% purrr::list_rbind()

  if (nrow(results) == 0) {
    stop("All LDA fits failed or produced invalid results. Check your solution and input data.")
  }

  # drop rows where any metric is NA or 0
  keep_rows <- purrr::map_lgl(results$accuracy_seg, function(x) {
    all_vals <- c(x$Precision, x$Recall)
    !any(is.na(all_vals)) && !any(all_vals == 0)
  }) & !is.na(results$accuracy_overall) & results$accuracy_overall > 0

  # keep everything above the first failure
  first_bad <- which(!keep_rows)[1]
  if (!is.na(first_bad)) {
    keep_rows[first_bad:length(keep_rows)] <- FALSE
  }

  results <- results[keep_rows, ]

  if (nrow(results) == 0) {
    stop("All item counts had NA or zero accuracy. Check your solution and input data.")
  }

  # -- build summary analysis table --
  analysis <- results %>%
    dplyr::select(accuracy_seg) %>%
    purrr::flatten() %>%
    purrr::map(~ tidyr::pivot_wider(
      .x,
      names_from = Segment,
      values_from = c(Precision, Recall)
    )) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      Items = results[["n"]],
      Overall_Accuracy = results[["accuracy_overall"]]
    ) %>%
    dplyr::relocate(Items, Overall_Accuracy, .before = 1)

  # -- build removal order (last in vars = removed first) --
  removed_steps <- tibble::tibble(
    step = seq_along(vars),
    removed = rev(vars),
    remaining = length(vars) - seq_along(vars)
  )

  cli::cli_alert_success("Short-form analysis complete. {nrow(analysis)} item counts evaluated.")

  list(
    results = results,
    analysis = analysis,
    ranked_inputs = vars,
    removed_steps = removed_steps,
    project_name = seg[["meta"]][["project_name"]],
    project_number = seg[["meta"]][["project_number"]]
  )
}
