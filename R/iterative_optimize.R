#' iterative_optimize
#'
#' @description Greedy swap optimizer that directly maximizes threshold hits.
#'   Unlike k-means (which minimizes within-cluster variance) or PAM (which
#'   minimizes distance to medoids), this optimizer targets the metric that
#'   actually matters for segmentation: how many variables show meaningful
#'   differentiation between each segment and everyone else.
#'
#'   **What it optimizes.** For each segment, the algorithm computes the
#'   weighted mean of every variable within the segment and the weighted mean
#'   of everyone *outside* the segment. A variable is a "hit" when the absolute
#'   difference between those two means exceeds a threshold (e.g. 0.20 for
#'   polars, 0.15 for profiles). The objective is the total number of hits
#'   summed across all segments. More hits = more differentiation = a more
#'   useful segmentation.
#'
#'   **How it works.** Starting from an initial assignment (typically k-means),
#'   the optimizer considers every possible single-respondent swap: moving one
#'   person from their current segment to a different segment. For each
#'   candidate swap, it computes how the total hit count would change. It picks
#'   the swap that produces the largest improvement (steepest ascent) and
#'   applies it. This repeats until no swap improves the objective.
#'
#'   **Vectorized delta computation.** Naively checking every swap would be
#'   prohibitively slow. Instead, for each (source_seg, target_seg) pair, the
#'   algorithm computes the change in objective for ALL respondents in source_seg
#'   simultaneously using matrix operations. This turns an O(n * k^2 * p)
#'   per-respondent loop into O(k^2) matrix multiplications.
#'
#'   **Incremental state updates.** Rather than recomputing all segment sums
#'   from scratch after each swap, the algorithm maintains running weighted
#'   sums and counts per segment. Each swap updates only O(p) values — the
#'   single respondent's contribution is subtracted from the old segment and
#'   added to the new one.
#'
#'   **Minimum segment size.** Segments below `min_seg_pct` of total n are
#'   protected from losing respondents. This prevents the optimizer from
#'   draining small segments to boost the objective.
#'
#'   This function is the raw engine — it operates on pre-built matrices and
#'   index vectors. For the pipeline wrapper that handles data prep, k-means
#'   seeding, variable reduction, and LDA, see [cluster_iterative()].
#'
#' @param data_mat Numeric matrix (n x p). Survey data with NAs replaced by 0.
#'   Columns are polar profile variables (binary 0/1 indicators) and shell
#'   profile variables (any scale). Rows are respondents.
#' @param non_na_mat Logical matrix (n x p). `TRUE` where the original data is
#'   non-missing. Used alongside `data_mat` to compute correct weighted means
#'   that respect missing data — the weighted sum (`data_mat * w`) divided by
#'   the weighted non-NA count (`non_na_mat * w`) gives the weighted mean
#'   excluding missing values.
#' @param w Numeric vector length n. Respondent weights. Use `rep(1, n)` for
#'   unweighted analysis.
#' @param init_assign Integer vector length n. Initial segment assignments
#'   (1-based, e.g. from `kmeans()$cluster`). The optimizer refines these
#'   assignments — it does not generate them from scratch.
#' @param polar_idx Integer vector. Column indices in `data_mat` that are polar
#'   profile variables. These are evaluated against `polar_threshold`. Note:
#'   this contains ALL polar columns in the matrix; `group_col_idx` and
#'   `target_groups` determine which subset actually counts toward the
#'   objective.
#' @param prof_idx Integer vector. Column indices in `data_mat` that are shell
#'   profile variables (demographics, attitudes, etc.). These are always
#'   evaluated against `profile_threshold` regardless of `target_groups`.
#' @param group_col_idx Named list of integer vectors. Partitions polar columns
#'   into groups by battery (e.g. `list(kg = 1:10, pr_tt = 11:25)`). Each
#'   element gives the column indices for one polar group.
#' @param target_groups Character vector. Controls which polar groups count
#'   toward the optimizer's objective function. The optimizer always evaluates
#'   ALL variables in the matrix, but only polar columns belonging to groups
#'   listed here contribute to the hit count that drives swap decisions.
#'   Profile hits always count regardless of this setting.
#'
#'   This lets you focus optimization on specific batteries. For example, with
#'   `group_col_idx = list(kg = 1:10, pr_tt = 11:25)`:
#'   - `target_groups = c("kg", "pr_tt")` — optimizer maximizes hits across
#'     both batteries (total differentiation).
#'   - `target_groups = "kg"` — optimizer only counts KG polar hits. It will
#'     ignore PR/TT polar differentiation when deciding which swaps to make,
#'     so the resulting segments may show strong KG separation but weaker
#'     PR/TT separation.
#'
#'   In `cluster_iterative()`, this is derived from `target_defs[[target]]`
#'   — e.g. if `target_defs = list(total = c("kg", "pr_tt"), kg_only = "kg")`
#'   and `target = "kg_only"`, then `target_groups = "kg"`.
#' @param polar_threshold Numeric. Minimum `abs(seg_mean - others_mean)` for a
#'   polar variable to count as a hit (default: `0.20`).
#' @param profile_threshold Numeric. Minimum `abs(seg_mean - others_mean)` for
#'   a profile variable to count as a hit (default: `0.15`).
#' @param min_seg_pct Numeric (0–1). Minimum segment size as a fraction of n.
#'   Segments at or below this size are protected from losing respondents
#'   (default: `0.05`).
#' @param max_iter Integer. Maximum number of swap iterations. The algorithm
#'   stops early if no improving swap exists (default: `1000`).
#' @param verbose Logical. Print progress to console (default: `TRUE`).
#'
#' @return A list with:
#' \describe{
#'   \item{assignments}{Integer vector length n. Optimized segment assignments.}
#'   \item{objective}{Integer. Final total threshold hits across all segments.}
#'   \item{hits_per_seg}{Integer vector length k. Hits per segment at
#'     convergence.}
#'   \item{seg_sizes}{Integer vector length k. Final segment sizes.}
#'   \item{history}{Tibble with one row per iteration plus the initial state.
#'     Columns: `iter` (0 = initial), `objective`, `delta` (improvement from
#'     this swap), `from` (source segment), `to` (destination segment).}
#'   \item{n_iter}{Integer. Number of iterations completed before convergence
#'     or hitting `max_iter`.}
#' }
#'
#' @export
iterative_optimize <- function(
    data_mat,
    non_na_mat,
    w,
    init_assign,
    polar_idx,
    prof_idx,
    group_col_idx,
    target_groups,
    polar_threshold   = 0.20,
    profile_threshold = 0.15,
    min_seg_pct       = 0.05,
    max_iter          = 1000,
    verbose           = TRUE
) {

  n <- nrow(data_mat)
  p <- ncol(data_mat)
  k <- max(init_assign, na.rm = TRUE)
  all_segs <- seq_len(k)
  min_seg_n <- ceiling(n * min_seg_pct)

  assignments <- init_assign

  hit_polar_idx <- unlist(group_col_idx[target_groups], use.names = FALSE)
  hit_prof_idx  <- prof_idx

  seg_sums   <- matrix(0, k, p)
  seg_counts <- matrix(0, k, p)

  seg_sizes  <- integer(k)

  for (s in all_segs) {
    idx <- which(assignments == s)
    seg_sizes[s] <- length(idx)
    seg_sums[s, ]   <- colSums(data_mat[idx, , drop = FALSE] * w[idx])
    seg_counts[s, ] <- colSums(non_na_mat[idx, , drop = FALSE] * w[idx])
  }

  total_sums   <- colSums(data_mat * w)
  total_counts <- colSums(non_na_mat * w)

  count_hits <- function(diff_vec) {
    sum(abs(diff_vec[hit_polar_idx]) >= polar_threshold, na.rm = TRUE) +
      sum(abs(diff_vec[hit_prof_idx]) >= profile_threshold, na.rm = TRUE)
  }

  compute_diffs <- function() {
    seg_means   <- seg_sums / seg_counts
    others_sums <- matrix(total_sums, k, p, byrow = TRUE) - seg_sums
    others_cnts <- matrix(total_counts, k, p, byrow = TRUE) - seg_counts
    seg_means - others_sums / others_cnts
  }

  diffs <- compute_diffs()
  current_hits  <- sapply(all_segs, function(s) count_hits(diffs[s, ]))
  current_total <- sum(current_hits)

  if (verbose) {
    cat(sprintf("  Target groups: %s\n", paste(target_groups, collapse = " + ")))
    cat(sprintf("  Thresholds: polar >= %.2f, profile >= %.2f\n",
                polar_threshold, profile_threshold))
    cat(sprintf("  Initial objective: %d  (sizes: %s)\n",
                current_total, paste(seg_sizes, collapse = "/")))
  }

  history <- tibble::tibble(
    iter = 0L, objective = current_total, delta = 0L,
    from = NA_integer_, to = NA_integer_
  )

  for (iter in seq_len(max_iter)) {

    best_delta <- 0L
    best_i     <- NA_integer_
    best_from  <- NA_integer_
    best_to    <- NA_integer_

    for (a in all_segs) {
      if (seg_sizes[a] <= min_seg_n) next

      idx_a <- which(assignments == a)
      n_a   <- length(idx_a)

      rows <- data_mat[idx_a, , drop = FALSE]
      nna  <- non_na_mat[idx_a, , drop = FALSE]
      w_a  <- w[idx_a]

      for (b in setdiff(all_segs, a)) {

        t_sums_a <- -rows * w_a + matrix(seg_sums[a, ], n_a, p, byrow = TRUE)
        t_cnts_a <- -nna  * w_a + matrix(seg_counts[a, ], n_a, p, byrow = TRUE)

        t_sums_b <- rows * w_a + matrix(seg_sums[b, ], n_a, p, byrow = TRUE)
        t_cnts_b <- nna  * w_a + matrix(seg_counts[b, ], n_a, p, byrow = TRUE)

        t_means_a <- t_sums_a / t_cnts_a
        t_means_b <- t_sums_b / t_cnts_b

        ts_broadcast <- matrix(total_sums, n_a, p, byrow = TRUE)
        tc_broadcast <- matrix(total_counts, n_a, p, byrow = TRUE)

        t_others_a <- (ts_broadcast - t_sums_a) / (tc_broadcast - t_cnts_a)
        t_others_b <- (ts_broadcast - t_sums_b) / (tc_broadcast - t_cnts_b)

        t_diff_a <- t_means_a - t_others_a
        t_diff_b <- t_means_b - t_others_b

        new_hits_a <- rowSums(abs(t_diff_a[, hit_polar_idx, drop = FALSE]) >= polar_threshold, na.rm = TRUE) +
          rowSums(abs(t_diff_a[, hit_prof_idx, drop = FALSE]) >= profile_threshold, na.rm = TRUE)
        new_hits_b <- rowSums(abs(t_diff_b[, hit_polar_idx, drop = FALSE]) >= polar_threshold, na.rm = TRUE) +
          rowSums(abs(t_diff_b[, hit_prof_idx, drop = FALSE]) >= profile_threshold, na.rm = TRUE)

        deltas <- (new_hits_a + new_hits_b) - (current_hits[a] + current_hits[b])

        best_in_pair <- which.max(deltas)
        if (length(best_in_pair) > 0 && deltas[best_in_pair] > best_delta) {
          best_delta <- deltas[best_in_pair]
          best_i     <- idx_a[best_in_pair]
          best_from  <- a
          best_to    <- b
        }
      }
    }

    if (best_delta <= 0) {
      if (verbose) cat(sprintf("  Converged at iteration %d (no improving swap)\n", iter))
      break
    }

    i <- best_i
    a <- best_from
    b <- best_to

    assignments[i] <- b

    row_i <- data_mat[i, ]
    nna_i <- non_na_mat[i, ]
    wi    <- w[i]

    seg_sums[a, ]   <- seg_sums[a, ] - wi * row_i
    seg_sums[b, ]   <- seg_sums[b, ] + wi * row_i
    seg_counts[a, ] <- seg_counts[a, ] - wi * nna_i
    seg_counts[b, ] <- seg_counts[b, ] + wi * nna_i
    seg_sizes[a]    <- seg_sizes[a] - 1L
    seg_sizes[b]    <- seg_sizes[b] + 1L

    diffs[a, ] <- seg_sums[a, ] / seg_counts[a, ] -
      (total_sums - seg_sums[a, ]) / (total_counts - seg_counts[a, ])
    diffs[b, ] <- seg_sums[b, ] / seg_counts[b, ] -
      (total_sums - seg_sums[b, ]) / (total_counts - seg_counts[b, ])

    current_hits[a] <- count_hits(diffs[a, ])
    current_hits[b] <- count_hits(diffs[b, ])
    current_total   <- sum(current_hits)

    history <- dplyr::bind_rows(history, tibble::tibble(
      iter = iter, objective = current_total, delta = best_delta,
      from = a, to = b
    ))

    if (verbose && (iter %% 25 == 0 || iter <= 5)) {
      cat(sprintf("    Iter %4d: obj=%d  delta=+%d  swap %d->%d  sizes=%s\n",
                  iter, current_total, best_delta, a, b,
                  paste(seg_sizes, collapse = "/")))
    }
  }

  if (verbose) {
    cat(sprintf("  Final objective: %d  (%+d from initial)  sizes: %s\n\n",
                current_total, current_total - history$objective[1],
                paste(seg_sizes, collapse = "/")))
  }

  list(
    assignments  = assignments,
    objective    = current_total,
    hits_per_seg = current_hits,
    seg_sizes    = seg_sizes,
    history      = history,
    n_iter       = max(history$iter)
  )
}
