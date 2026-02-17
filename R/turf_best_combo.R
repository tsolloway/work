#' turf_best_combo
#' @description Computes Total Unduplicated Reach and Frequency (TURF) for every
#'   n-item combination from a binary (0/1) data frame. Automatically selects the
#'   optimal strategy based on the number of combinations:
#'   \itemize{
#'     \item <= \code{max_brute} combos: brute-force (single core)
#'     \item \code{max_brute} - \code{max_parallel} combos: parallel brute-force via \code{future}/\code{furrr}
#'     \item > \code{max_parallel} combos: hybrid of greedy, random sampling, and shortlist brute-force
#'   }
#'   If weights are provided, both unweighted and weighted results are returned.
#'   If subgroups are provided, TURF runs separately on each subgroup.
#'
#' @details
#' \strong{Why hybrid > Monte Carlo for large combo spaces}
#'
#' When the number of combinations exceeds \code{max_parallel}, packages like
#' \code{turfR} fall back to pure Monte Carlo (MC) sampling -- drawing random
#' combos and hoping to land near the optimum. This is wasteful because
#' the best combos cluster around a small set of high-reach items, and
#' uniform random draws are unlikely to find them.
#'
#' The hybrid strategy used here is more targeted:
#' \enumerate{
#'   \item \strong{Greedy} identifies the single best combo by stepwise
#'     selection (near-optimal in practice). This anchors the search.
#'   \item \strong{Shortlist} takes the union of greedy-path items and
#'     top individual-reach items, then brute-forces every combo within
#'     that reduced item set. This exhaustively covers the most promising
#'     region of the combo space.
#'   \item \strong{Random} samples broadly across the full space as a
#'     hedge against the greedy/shortlist missing an unexpected combo.
#' }
#'
#' In benchmarks (40 items, k=5, 658k combos, 10k samples each), the hybrid
#' found a top reach of 89.4\% vs MC's 89.0\% -- because shortlist
#' brute-force systematically covers the high-value region that random
#' sampling only hits by chance. The advantage grows with the size of the
#' combo space, where random draws become increasingly sparse relative to
#' the number of possible combos.
#'
#' @param df A data frame containing item columns, and optionally weight and
#'   subgroup columns.
#' @param vars Character vector of column names in \code{df} that are the
#'   binary (0/1) items to analyze.
#' @param n Integer or integer vector. Combination size(s) to evaluate.
#'   A single value (e.g., \code{2}) returns a tibble of all combos.
#'   A vector (e.g., \code{2:10}) returns a named list of tibbles
#'   (\code{$n_2}, \code{$n_3}, etc.).
#' @param subgroups Optional character vector of column names in \code{df}.
#'   Each should be a binary (0/1) indicator. TURF runs separately on
#'   respondents where each column == 1. Use a column of all 1s for the
#'   total sample (e.g., \code{"Total"}). Returns a named list of results.
#' @param labels Optional dictionary data frame (e.g., from \code{work::read}).
#'   If provided, label columns are interleaved next to each item column.
#'   Expects columns \code{"variable"} and \code{"label"}.
#' @param weight Optional string. Column name in \code{df} containing
#'   respondent weights. If provided, weighted reach and frequency columns
#'   are appended alongside the unweighted columns.
#' @param parallel Logical. If \code{TRUE} (default), uses parallel processing.
#'   When subgroups are provided and the combo space is small (below
#'   \code{max_brute}), subgroups run in parallel with sequential inner engines.
#'   When the combo space is large, subgroups run sequentially so the inner
#'   engine can use all cores.
#' @param max_brute Integer. Maximum number of combinations for single-core
#'   brute-force. Above this threshold, switches to parallel (if enabled) or
#'   hybrid. Default \code{50000}.
#' @param max_parallel Integer. Maximum number of combinations for parallel
#'   brute-force. Above this threshold, switches to hybrid strategy.
#'   Default \code{10000000}.
#' @param n_random Integer. Number of random combos to sample in the hybrid
#'   strategy (> \code{max_parallel} combos). Default \code{100000}.
#' @param n_shortlist Integer. Number of top items to shortlist from greedy for
#'   the brute-force pass in the hybrid strategy. Default \code{20}.
#' @param seed Integer. Seed for reproducibility of random sampling. Default \code{1}.
#' @param estimate_time Logical. If \code{TRUE}, prints an estimated runtime
#'   table for each combo size (and subgroup if applicable) and returns the
#'   estimate invisibly without running the analysis. Uses a quick local
#'   benchmark to calibrate combos/sec on the current machine. Default \code{FALSE}.
#'
#' @return Depends on inputs:
#' \itemize{
#'   \item Single \code{n}, no subgroups: a tibble.
#'   \item Vector \code{n}, no subgroups: a named list of tibbles (\code{$n_2}, \code{$n_3}, etc.).
#'   \item With subgroups: a named list by subgroup name, where each element is
#'     the result structure above (tibble or named list of tibbles).
#' }
#' Each tibble is sorted by reach (descending) with columns:
#' \describe{
#'   \item{item_1 ... item_n}{Variable name for each item slot in the combo.}
#'   \item{label_1 ... label_n}{(If labels provided) Label for each item.}
#'   \item{reach_n}{Unweighted count of respondents reached.}
#'   \item{reach_pct}{Unweighted percent of respondents reached.}
#'   \item{freq_avg}{Unweighted average frequency among reached respondents.}
#'   \item{w_reach_n}{(If weight provided) Weighted count of respondents reached.}
#'   \item{w_reach_pct}{(If weight provided) Weighted percent of respondents reached.}
#'   \item{w_freq_avg}{(If weight provided) Weighted average frequency among reached respondents.}
#' }
#'
#' @examples
#' \dontrun{
#' turf_results <- turf_best_combo(
#'   df        = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   n         = 1:3,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   labels    = example_data_ice_cream_dictionary,
#'   weight    = "weight"
#' )
#' }
#'
#' @export
turf_best_combo <- function(
    df, vars, n = 1:5, subgroups = NULL, labels = NULL, weight = NULL,
    parallel = TRUE, max_brute = 150000, max_parallel = 150001,
    n_random = 100000, n_shortlist = 20, seed = 1,
    estimate_time = FALSE
) {

  # df = df_turf
  # vars = df_turf %>% select(starts_with("PAExp_")) %>% names()
  # n = 2:10
  # subgroups = c("Total", "Male", "Female")
  # labels = dict
  # weight = "weight"
  # parallel = TRUE
  # max_brute = 50000
  # max_parallel = 200000
  # n_random = 100000
  # n_shortlist = 20
  # seed = 1
  # estimate_time = FALSE


  # ---- Validate inputs ----
  if(!is.data.frame(df)) stop("`df` must be a data.frame.")
  if(!all(vars %in% names(df))) stop("Some `vars` not found in `df`.")

  # Extract weight vector from column name
  wt_vec <- NULL
  if(!is.null(weight)){
    if(!weight %in% names(df)) stop(glue::glue("Weight column '{weight}' not found in `df`."))
    wt_vec <- df[[weight]]
    if(!is.numeric(wt_vec)) stop(glue::glue("Weight column '{weight}' must be numeric."))
  }

  # Extract binary item data
  item_df <- df[, vars, drop = FALSE]


  # ---- Time estimation ----
  if(estimate_time){
    estimate <- .turf_estimate(
      item_df = item_df, n = n, weight = wt_vec,
      subgroups = subgroups, df = df,
      parallel = parallel, max_brute = max_brute, max_parallel = max_parallel,
      n_random = n_random, n_shortlist = n_shortlist
    )
    return(invisible(estimate))
  }


  # ---- Subgroup dispatch ----
  if(!is.null(subgroups)){

    if(!all(subgroups %in% names(df))) stop("Some `subgroups` columns not found in `df`.")

    cli::cli_h2("TURF: {length(subgroups)} subgroups")

    out <- purrr::map(subgroups, function(sg_name){

      mask <- df[[sg_name]] == 1
      sg_n <- sum(mask, na.rm = TRUE)
      cli::cli_h3("{sg_name} (n = {sg_n})")

      sg_weight <- if(!is.null(wt_vec)) wt_vec[mask] else NULL

      .turf_core(
        df = item_df[mask, , drop = FALSE], n = n, labels = labels, weight = sg_weight,
        parallel = parallel, max_brute = max_brute, max_parallel = max_parallel,
        n_random = n_random, n_shortlist = n_shortlist, seed = seed
      )
    }) %>% purrr::set_names(subgroups)

    return(out)
  }


  # ---- No subgroups: run on full data ----
  .turf_core(
    df = item_df, n = n, labels = labels, weight = wt_vec,
    parallel = parallel, max_brute = max_brute, max_parallel = max_parallel,
    n_random = n_random, n_shortlist = n_shortlist, seed = seed
  )
}


# =============================================================================
# .turf_estimate — quick time estimate without running the full analysis
# =============================================================================

.turf_estimate <- function(
    item_df, n, weight = NULL, subgroups = NULL, df = NULL,
    parallel = TRUE, max_brute = 50000, max_parallel = 200000,
    n_random = 100000, n_shortlist = 20
) {

  n_items <- ncol(item_df)
  n_cores <- parallel::detectCores(logical = FALSE)

  # ---- Micro-benchmark using the REAL .build_row logic ----
  mat <- as.matrix(item_df)
  n_resp <- nrow(mat)
  item_names <- colnames(mat)
  w_total <- if(!is.null(weight)) sum(weight) else NULL

  # Full .build_row replica (same work as the real function)
  .bench_row <- function(idx){
    sub <- mat[, idx, drop = FALSE]
    hits <- rowSums(sub, na.rm = TRUE)
    reached <- hits > 0
    reach_n <- sum(reached, na.rm = TRUE)

    item_cols <- as.list(setNames(item_names[idx], paste0("item_", seq_along(idx))))
    item_df_local <- as.data.frame(item_cols, stringsAsFactors = FALSE)

    metrics <- data.frame(
      reach_n   = reach_n,
      reach_pct = round(reach_n / n_resp * 100, 1),
      freq_avg  = if(isTRUE(reach_n > 0)) round(mean(hits[reached], na.rm = TRUE), 2) else 0
    )

    if(!is.null(weight)){
      w_reach_n <- sum(weight[reached])
      metrics <- cbind(metrics, data.frame(
        w_reach_n   = round(w_reach_n, 2),
        w_reach_pct = round(w_reach_n / w_total * 100, 1),
        w_freq_avg  = if(isTRUE(w_reach_n > 0)) round(stats::weighted.mean(hits[reached], weight[reached], na.rm = TRUE), 2) else 0
      ))
    }

    cbind(item_df_local, metrics)
  }

  # Benchmark at the median n value — enough reps to get a stable rate
  bench_n <- as.integer(stats::median(n))
  bench_reps <- 500
  bench_combos <- lapply(seq_len(bench_reps), function(i) sort(sample.int(n_items, bench_n)))
  bench_time <- system.time({
    for(i in seq_len(bench_reps)) .bench_row(bench_combos[[i]])
  })[["elapsed"]]

  combos_per_sec <- bench_reps / max(bench_time, 0.001)

  cli::cli_alert_info(
    "Benchmark: {format(round(combos_per_sec), big.mark = ',')} combos/sec (n={bench_n}, {format(n_resp, big.mark = ',')} respondents)"
  )

  # ---- Estimate per combo size ----
  # Use the sequential brute rate for all strategies. The parallel path
  # (furrr) adds significant overhead that makes it slower than sequential
  # for most combo counts, so the sequential rate is the most accurate
  # baseline for wall-clock estimation.

  .estimate_n <- function(ni, n_items_local){
    n_combos <- choose(n_items_local, ni)

    if(n_combos <= max_brute){
      strategy <- "brute"
      effective_combos <- n_combos
    } else if(n_combos <= max_parallel){
      strategy <- if(parallel) "parallel" else "brute"
      effective_combos <- n_combos
    } else {
      strategy <- "hybrid"
      # Hybrid = greedy (negligible) + random (n_random) + shortlist brute
      sl_items <- min(n_shortlist, n_items_local)
      sl_combos <- if(sl_items >= ni) choose(sl_items, ni) else 0
      effective_combos <- n_random + sl_combos
    }

    est_sec <- effective_combos / combos_per_sec

    data.frame(
      n = ni,
      combos = n_combos,
      strategy = strategy,
      est_sec = round(est_sec, 1),
      est_min = round(est_sec / 60, 1),
      stringsAsFactors = FALSE
    )
  }

  # ---- Build estimate table(s) ----
  if(!is.null(subgroups)){

    cli::cli_h2("Time Estimate: {length(subgroups)} subgroups x {length(n)} combo sizes")

    sg_estimates <- purrr::map(subgroups, function(sg_name){
      mask <- df[[sg_name]] == 1
      sg_n_resp <- sum(mask, na.rm = TRUE)

      est <- purrr::map_dfr(n, ~.estimate_n(.x, n_items))
      est$subgroup <- sg_name
      est$respondents <- sg_n_resp
      est
    }) %>% dplyr::bind_rows()

    # Reorder columns
    sg_estimates <- sg_estimates %>%
      dplyr::select(subgroup, respondents, n, combos, strategy, est_sec, est_min) %>%
      tibble::as_tibble()

    # Determine parallel subgroup behavior
    worst_combos <- choose(n_items, max(n))
    parallel_sg <- parallel && worst_combos <= max_brute && length(subgroups) > 1

    # Total time depends on sequential vs parallel subgroups
    if(parallel_sg){
      # Parallel subgroups: total ≈ slowest subgroup + startup overhead
      sg_totals <- sg_estimates %>%
        dplyr::group_by(subgroup) %>%
        dplyr::summarise(sg_total = sum(est_sec), .groups = "drop")
      total_sec <- max(sg_totals$sg_total) + 2  # furrr startup overhead
      sg_mode <- "parallel"
    } else {
      total_sec <- sum(sg_estimates$est_sec)
      sg_mode <- "sequential"
    }

    total_min <- round(total_sec / 60, 1)

    # Print
    print(sg_estimates, n = nrow(sg_estimates))
    cli::cli_text("")
    cli::cli_alert_info("Subgroup mode: {sg_mode}")
    cli::cli_alert_info("Estimated total: {round(total_sec, 1)} sec ({total_min} min)")

    return(invisible(sg_estimates))

  } else {

    cli::cli_h2("Time Estimate: {length(n)} combo sizes")

    estimates <- purrr::map_dfr(n, ~.estimate_n(.x, n_items)) %>%
      tibble::as_tibble()

    total_sec <- sum(estimates$est_sec)
    total_min <- round(total_sec / 60, 1)

    print(estimates, n = nrow(estimates))
    cli::cli_text("")
    cli::cli_alert_info("Estimated total: {round(total_sec, 1)} sec ({total_min} min)")

    return(invisible(estimates))
  }
}


# =============================================================================
# .turf_core — internal engine (binary-only df + weight vector)
# =============================================================================

.turf_core <- function(
    df, n, labels = NULL, weight = NULL,
    parallel = TRUE, max_brute = 50000, max_parallel = 200000,
    n_random = 100000, n_shortlist = 20, seed = 1
) {

  # ---- Validation ----
  df <- as.data.frame(df)
  if(ncol(df) < max(n)) stop(glue::glue("`n` ({max(n)}) cannot exceed the number of items ({ncol(df)})."))
  if(min(n) < 1) stop("`n` must be >= 1.")

  if(!is.null(weight)){
    if(length(weight) != nrow(df)) stop(glue::glue(
      "`weight` length ({length(weight)}) must equal nrow(df) ({nrow(df)})."
    ))
  }


  mat <- as.matrix(df)
  n_resp <- nrow(mat)
  n_items <- ncol(mat)
  item_names <- colnames(mat)
  w_total <- if(!is.null(weight)) sum(weight) else NULL


  # ---- Build label lookup ----
  label_lookup <- NULL
  if(!is.null(labels)){

    if(is.character(labels)){
      labels <- labels %>% work::dictionary_from_named_object()
    }

    for(i in c("var", "vars", "variables")){
      if(i %in% tolower(names(labels))) names(labels)[which(tolower(names(labels)) %in% i)] <- "variable"
    }

    for(i in c("lab", "labs")){
      if(i %in% tolower(names(labels))) names(labels)[which(tolower(names(labels)) %in% i)] <- "label"
    }

    if(!all(c("variable", "label") %in% names(labels))){
      stop(glue::glue('labels object must have columns c("variable", "label")'))
    }

    label_lookup <- setNames(labels$label, labels$variable)
  }


  # ===========================================================================
  # Internal helpers (closures over mat, n_resp, item_names, weight, etc.)
  # ===========================================================================

  .build_row <- function(idx){
    sub <- mat[, idx, drop = FALSE]
    hits <- rowSums(sub, na.rm = TRUE)
    reached <- hits > 0
    reach_n <- sum(reached, na.rm = TRUE)

    # Item columns
    item_cols <- as.list(setNames(item_names[idx], paste0("item_", seq_along(idx))))
    item_df <- as.data.frame(item_cols, stringsAsFactors = FALSE)

    # Interleave labels
    if(!is.null(label_lookup)){
      interleaved <- item_df[, integer(0)]
      for(j in seq_along(idx)){
        interleaved[[paste0("item_", j)]]  <- item_names[idx[j]]
        interleaved[[paste0("label_", j)]] <- unname(label_lookup[item_names[idx[j]]])
      }
      item_df <- interleaved
    }

    # Unweighted metrics
    metrics <- data.frame(
      reach_n   = reach_n,
      reach_pct = round(reach_n / n_resp * 100, 1),
      freq_avg  = if(isTRUE(reach_n > 0)) round(mean(hits[reached], na.rm = TRUE), 2) else 0
    )

    # Weighted metrics
    if(!is.null(weight)){
      w_reach_n <- sum(weight[reached])
      w_freq_avg <- if(isTRUE(w_reach_n > 0)) round(stats::weighted.mean(hits[reached], weight[reached], na.rm = TRUE), 2) else 0
      metrics <- cbind(metrics, data.frame(
        w_reach_n   = round(w_reach_n, 2),
        w_reach_pct = round(w_reach_n / w_total * 100, 1),
        w_freq_avg  = w_freq_avg
      ))
    }

    cbind(item_df, metrics)
  }


  .turf_greedy_items <- function(n){
    selected <- integer(0)
    remaining <- seq_len(n_items)

    for(s in seq_len(min(n, n_items))){
      incremental <- sapply(remaining, function(j){
        if(length(selected) == 0){
          sum(mat[, j] == 1)
        } else {
          already_reached <- rowSums(mat[, selected, drop = FALSE]) > 0
          sum(!already_reached & mat[, j] == 1)
        }
      })
      best <- which.max(incremental)
      best_col <- remaining[best]
      selected <- c(selected, best_col)
      remaining <- setdiff(remaining, best_col)
    }

    selected
  }


  # ---- Brute-force (single core) ----
  .turf_brute <- function(n){
    combos <- combn(n_items, n)
    nc <- ncol(combos)

    cli::cli_alert_info("TURF n={n}: {format(nc, big.mark = ',')} combos -- brute-force")

    pb <- cli::cli_progress_bar(
      format = "Brute n={n} [{cli::pb_bar}] {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = nc
    )

    results <- vector("list", nc)
    for(i in seq_len(nc)){
      results[[i]] <- .build_row(combos[, i])
      cli::cli_progress_update(id = pb)
    }
    cli::cli_progress_done(id = pb)

    dplyr::bind_rows(results) %>% tibble::as_tibble()
  }


  # ---- Parallel brute-force ----
  .turf_parallel <- function(n){
    combos <- combn(n_items, n)
    nc <- ncol(combos)

    cli::cli_alert_info("TURF n={n}: {format(nc, big.mark = ',')} combos -- parallel brute-force")

    plan_was_sequential <- inherits(future::plan(), "sequential")
    if(plan_was_sequential && parallel){
      work::future_plan("multisession")
      on.exit(future::plan("sequential"), add = TRUE)
    }

    # Chunk combos for progress updates (one tick per chunk)
    chunk_size <- max(1, floor(nc / 100))
    chunks <- split(seq_len(nc), ceiling(seq_len(nc) / chunk_size))

    pb <- cli::cli_progress_bar(
      format = "Parallel n={n} [{cli::pb_bar}] {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = length(chunks)
    )

    all_results <- list()
    for(ch in chunks){
      chunk_combos <- lapply(ch, function(i) combos[, i])
      chunk_res <- furrr::future_map(chunk_combos, function(idx){
        .build_row(idx)
      }, .options = furrr::furrr_options(
        packages = c("dplyr", "glue"), seed = TRUE
      ))
      all_results <- c(all_results, chunk_res)
      cli::cli_progress_update(id = pb)
    }
    cli::cli_progress_done(id = pb)

    dplyr::bind_rows(all_results) %>% tibble::as_tibble()
  }


  # ---- Greedy stepwise ----
  .turf_greedy <- function(n){
    cli::cli_alert_info("Greedy: building {n}-item combo stepwise...")

    selected <- integer(0)
    remaining <- seq_len(n_items)

    for(s in seq_len(n)){
      incremental <- sapply(remaining, function(j){
        if(length(selected) == 0){
          sum(mat[, j] == 1)
        } else {
          already_reached <- rowSums(mat[, selected, drop = FALSE]) > 0
          sum(!already_reached & mat[, j] == 1)
        }
      })

      best <- which.max(incremental)
      best_col <- remaining[best]
      selected <- c(selected, best_col)
      remaining <- setdiff(remaining, best_col)

      cli::cli_alert_success("Step {s}/{n}: +{item_names[best_col]} (+{incremental[best]})")
    }

    reached_total <- sum(rowSums(mat[, selected, drop = FALSE]) > 0)
    cli::cli_alert_success("Greedy done (reach {round(reached_total / n_resp * 100, 1)}%)")

    .build_row(selected)
  }


  # ---- Random sampling ----
  .turf_random <- function(n, n_random, seed){
    cli::cli_alert_info("Random: sampling {format(n_random, big.mark = ',')} combos...")

    if(!is.null(seed)) set.seed(seed)

    random_combos <- lapply(seq_len(n_random), function(i) sort(sample.int(n_items, n)))

    pb <- cli::cli_progress_bar(
      format = "Random n={n} [{cli::pb_bar}] {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n_random
    )

    results <- vector("list", n_random)
    for(i in seq_len(n_random)){
      results[[i]] <- .build_row(random_combos[[i]])
      cli::cli_progress_update(id = pb)
    }
    cli::cli_progress_done(id = pb)

    cli::cli_alert_success("Random: done")

    dplyr::bind_rows(results) %>% tibble::as_tibble()
  }


  # ---- Shortlist: greedy top items -> brute-force ----
  .turf_shortlist <- function(n, n_shortlist){

    greedy_items <- .turf_greedy_items(n)

    item_reach <- colSums(mat)
    top_by_reach <- order(item_reach, decreasing = TRUE)[1:min(n_shortlist, n_items)]

    shortlisted <- sort(unique(c(greedy_items, top_by_reach)))
    if(length(shortlisted) > n_shortlist){
      shortlisted <- sort(unique(c(
        greedy_items,
        top_by_reach[1:max(1, n_shortlist - length(greedy_items))]
      )))[1:n_shortlist]
    }

    if(length(shortlisted) < n){
      shortlisted <- sort(unique(c(shortlisted, order(item_reach, decreasing = TRUE)[1:n])))
    }

    nc <- choose(length(shortlisted), n)

    cli::cli_alert_info(
      "Shortlist: brute-forcing {format(nc, big.mark = ',')} combos from top {length(shortlisted)} items..."
    )

    combos <- combn(shortlisted, n)

    pb <- cli::cli_progress_bar(
      format = "Shortlist n={n} [{cli::pb_bar}] {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = nc
    )

    results <- vector("list", nc)
    for(i in seq_len(nc)){
      results[[i]] <- .build_row(combos[, i])
      cli::cli_progress_update(id = pb)
    }
    cli::cli_progress_done(id = pb)

    cli::cli_alert_success("Shortlist: done")

    dplyr::bind_rows(results) %>% tibble::as_tibble()
  }


  # ===========================================================================
  # Single-n worker
  # ===========================================================================

  .turf_single <- function(ni){
    n_combos <- choose(n_items, ni)

    if(n_combos <= max_brute){

      .turf_brute(ni)

    } else if(n_combos <= max_parallel){

      if(parallel){
        .turf_parallel(ni)
      } else {
        .turf_brute(ni)
      }

    } else {

      cli::cli_alert_info(
        "TURF n={ni}: {format(n_combos, big.mark = ',')} combos > {format(max_parallel, big.mark = ',')} -- hybrid (greedy + random + shortlist)"
      )

      greedy_result    <- .turf_greedy(ni)
      random_result    <- .turf_random(ni, n_random, seed)
      shortlist_result <- .turf_shortlist(ni, n_shortlist)

      out <- dplyr::bind_rows(greedy_result, shortlist_result, random_result) %>%
        tibble::as_tibble()

      item_cols <- grep("^item_", names(out), value = TRUE)
      out <- out %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(item_cols)), .keep_all = TRUE)

      out
    }
  }


  # ===========================================================================
  # Dispatch
  # ===========================================================================

  if(length(n) > 1){

    cli::cli_h2("TURF: combo sizes {min(n)} to {max(n)}")

    out <- purrr::map(n, function(ni){
      cli::cli_h3("n = {ni} ({which(n == ni)}/{length(n)})")
      res <- .turf_single(ni)
      dplyr::arrange(res, -reach_n, -freq_avg)
    }) %>%
      purrr::set_names(paste0("n_", n))

    cli::cli_alert_success("TURF complete ({length(n)} combo sizes)")

    return(out)

  } else {

    out <- .turf_single(n)
    out <- out %>% dplyr::arrange(-reach_n, -freq_avg)
    return(out)
  }
}
