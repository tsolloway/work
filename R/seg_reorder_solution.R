#' seg_reorder_solution
#' @description Relabel an existing LDA solution's segments into a new order and
#'   refit a clean LDA against the relabeled segments. Rather than permuting the
#'   stored coefficient columns / posterior columns (error-prone — the
#'   coefficient matrix, posteriors, and segment labels each have to be permuted
#'   in exactly the right direction), this refits `MASS::lda` on the reordered
#'   k-means cluster labels and derives a fresh coefficient function, posterior
#'   table, and segment assignment from that single fit. The three are therefore
#'   guaranteed mutually consistent — the typing tool's coefficient scoring, its
#'   Bulk QC posteriors (`lda_predict`), and the shell's segment means all agree.
#'
#'   The original solution is left untouched. The reordered solution is added as
#'   a new row under the `reorder` analysis family, named `solution_new`
#'   (default `paste0(solution_old, "_reordered")`), so both coexist.
#'
#' @param seg A seg object with solutions.
#' @param solution_old Character. The `lda_name` of the solution to reorder.
#' @param solution_new Character. The `lda_name` for the reordered solution.
#'   Defaults to `paste0(solution_old, "_reordered")`.
#' @param new_order Integer vector mapping old segment `i` to new label
#'   `new_order[i]` (e.g. `c(3,1,2)` sends old seg 1 -> 3, 2 -> 1, 3 -> 2). Must
#'   be a permutation of `1:n_segments`.
#' @param resp_id_name Character or `NULL`. Respondent id column; auto-detected
#'   via [get_resp_id_name()] when `NULL`.
#' @return The seg object with the reordered solution added under
#'   `seg$solutions$analysis$reorder`, and `summary_table` / `with_solutions`
#'   rebuilt.
#' @export
seg_reorder_solution <- function(
    seg,
    solution_old,
    solution_new = NULL,
    new_order,
    resp_id_name = NULL
){

  if (is.null(solution_new)) solution_new <- paste0(solution_old, "_reordered")
  if (is.null(resp_id_name)) resp_id_name <- get_resp_id_name(seg)


  # ---- locate the original solution row (full, with df_solution / lda_fit) ----
  # The global summary_table is built by seg_bind_summary_tables, which strips
  # df_solution / lda_fit / lda_predict, so look up the full row from the
  # per-family analysis tables. Include the "reorder" family so an
  # already-reordered solution can itself be reordered.
  summary_table_old <- purrr::keep(seg[["solutions"]][["analysis"]], is.list) %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    purrr::compact() %>%
    purrr::map(~ dplyr::filter(.x, lda_name == !!solution_old)) %>%
    purrr::keep(~ nrow(.x) > 0) %>%
    purrr::pluck(1, .default = NULL)

  if (is.null(summary_table_old) || nrow(summary_table_old) == 0) {
    stop(glue::glue(
      "Solution '{solution_old}' not found in any seg$solutions$analysis ",
      "family's solution_table."
    ))
  }

  orig <- dplyr::slice(summary_table_old, 1)

  K <- orig[["n"]]
  if (!all(seq_len(K) == sort(new_order))) {
    stop("new_order must be a permutation of 1:n_segments.")
  }


  inputs       <- unlist(orig[["lda_inputs"]][[1]])
  cluster_name <- orig[["cluster_name"]]
  d_old        <- orig[["df_solution"]][[1]]


  # ---- training target: the original k-means clusters, relabeled by new_order ----
  # df_solution holds id + <cluster_name> (k-means assignment) + <lda_name>.
  # The original LDA was trained to predict the k-means clusters, so we relabel
  # those (old cluster i -> new_order[i]) and refit on the relabeled target.
  train   <- d_old %>% dplyr::filter(!is.na(.data[[cluster_name]]))
  old_lab <- as.integer(train[[cluster_name]])
  new_lab <- factor(new_order[old_lab], levels = sort(new_order))


  df_all <- seg[["data"]][["with_solutions"]]
  if (is.null(df_all) || (length(df_all) == 1 && all(is.na(df_all)))) {
    df_all <- seg[["data"]][["with_shell"]]
  }

  X_train <- df_all %>%
    dplyr::filter(.data[[resp_id_name]] %in% train[["id"]]) %>%
    dplyr::arrange(match(.data[[resp_id_name]], train[["id"]])) %>%
    dplyr::select(dplyr::all_of(inputs))

  X_all <- df_all %>% dplyr::select(dplyr::all_of(inputs))


  # ---- prior: permute the original fit's prior onto the new labels ----
  # new label L corresponds to old cluster order(new_order)[L], so the new
  # prior vector (in level order 1..K) is the old prior reindexed by that.
  prior <- NULL
  orig_fit <- tryCatch(orig[["lda_fit"]][[1]], error = function(e) NULL)
  if (inherits(orig_fit, "lda") && !is.null(orig_fit[["prior"]])) {
    op    <- as.numeric(orig_fit[["prior"]])
    prior <- op[order(new_order)]
    prior <- prior / sum(prior)
  }


  # ---- refit a clean LDA on the reordered labels ----
  set.seed(1)
  .collinear <- FALSE
  fit_new <- withCallingHandlers(
    if (is.null(prior)) {
      MASS::lda(x = X_train, grouping = new_lab)
    } else {
      MASS::lda(x = X_train, grouping = new_lab, prior = prior)
    },
    warning = function(w) {
      if (grepl("collinear", conditionMessage(w))) {
        .collinear <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  attr(fit_new, "collinear") <- .collinear


  # ---- clean coefficient function from the refit ----
  coef_new <- if (.collinear) {
    coefficient_lda_colinear(fit_new)
  } else {
    coefficient_lda(fit = fit_new, input = X_train, grp = new_lab)
  }


  # ---- clean posteriors (seg_1..seg_K + argmax `seg`) on all respondents ----
  .lda_score <- function(fit, newdata) {
    pred <- stats::predict(fit, newdata)
    out <- dplyr::bind_cols(
      tibble::as_tibble(pred[["posterior"]]),
      tibble::tibble(class = pred[["class"]])
    ) %>% suppressMessages()
    out <- rlang::set_names(out, glue::glue("seg_{seq_len(ncol(out))}"))
    names(out)[ncol(out)] <- "seg"
    out
  }

  lda_predict_new <- dplyr::bind_cols(
    df_all %>%
      dplyr::select(dplyr::all_of(resp_id_name)) %>%
      rlang::set_names("seg_uuid"),
    .lda_score(fit_new, X_all)
  )


  # ---- safety check: the refit must reproduce the original solution exactly,
  #      only relabeled. LDA is equivariant under a class relabel (with the
  #      prior permuted to match), so the refit's segment frequencies must equal
  #      the original assignment relabeled by new_order. Any drift means the
  #      refit diverged (bad prior, collinearity change, numerical instability)
  #      — halt rather than ship a typing tool that disagrees with the shell. ----
  orig_predict <- orig[["lda_predict"]][[1]]
  if (is.data.frame(orig_predict) && "seg" %in% names(orig_predict)) {
    cmp <- dplyr::inner_join(
      orig_predict   %>% dplyr::transmute(seg_uuid, old_seg = as.integer(as.character(seg))),
      lda_predict_new %>% dplyr::transmute(seg_uuid, new_seg = as.integer(as.character(seg))),
      by = "seg_uuid"
    )
    lv       <- sort(new_order)
    old_freq <- table(factor(new_order[cmp[["old_seg"]]], levels = lv))
    new_freq <- table(factor(cmp[["new_seg"]],            levels = lv))

    if (!identical(unname(as.integer(old_freq)), unname(as.integer(new_freq)))) {
      stop(glue::glue(
        "seg_reorder_solution: the refit LDA did not reproduce the original ",
        "solution after relabeling — segment frequency tables differ.\n",
        "  reordered original: ",
        "{paste(sprintf('%s=%d', names(old_freq), as.integer(old_freq)), collapse = ', ')}\n",
        "  refit:              ",
        "{paste(sprintf('%s=%d', names(new_freq), as.integer(new_freq)), collapse = ', ')}"
      ), call. = FALSE)
    }
  }


  # ---- df_solution: id + reordered segment (the refit's argmax) ----
  df_solution_new <- lda_predict_new %>%
    dplyr::transmute(
      id = .data[["seg_uuid"]],
      !!solution_new := as.numeric(as.character(.data[["seg"]]))
    )


  # ---- assemble reordered row (original row preserved verbatim) ----
  reorder_row <- orig %>% dplyr::mutate(
    solution_name            = "reorder",
    cluster_name             = paste0(cluster_name, "_reordered"),
    lda_name                 = solution_new,
    lda_fit                  = list(fit_new),
    lda_coefficient_function = list(coef_new),
    lda_predict              = list(lda_predict_new),
    n_segments               = list(sort(new_order)),
    collinear                = .collinear,
    df_solution              = list(df_solution_new)
  )

  seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]] <- dplyr::bind_rows(
    seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]],
    reorder_row
  )


  seg[["solutions"]][["summary_table"]] <- seg_bind_summary_tables(seg)

  seg <- seg_build_with_solutions(seg)

  return(seg)
}
