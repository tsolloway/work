#' seg_needs_profile_cut
#'
#' @description Profiles cells of the needs cut against the person-level spec
#'   variables: what distinguishes the respondents in a cell from everyone else
#'   who was classified. A quick read for deciding what a cell is, before the
#'   full shell from [seg_needs_write_cut()].
#'
#'   Reads the executed spec at person level - one row per respondent, with
#'   every column that varies across a respondent's contexts removed, the same
#'   rule [seg_needs_write_shell()] uses. Each cell is compared with the rest of
#'   the classified respondents: means, difference, index (cell / total x 100)
#'   and a Welch t-test p-value. Unweighted.
#'
#' @param seg A seg object after [seg_needs_cut()] and [seg_get_spec()].
#' @param cells Cells to profile, as codes or labels. Default: every cell with
#'   at least `min_n` respondents.
#' @param blocks Character. Spec block prefixes to profile against, e.g.
#'   `c("DE", "HO")`. Default: every person-level block.
#' @param vars Character. Individual spec variables, added to `blocks`.
#' @param exclude_basis Logical. Leave out spec variables built from the needs
#'   loop itself (default `TRUE`). A cell typed on the needs will of course
#'   differ on the needs; those rows restate the typing and crowd out the
#'   profile. Applies only when neither `blocks` nor `vars` is given.
#' @param top_n Integer. Distinguishing variables printed per cell
#'   (default `10`).
#' @param alpha Numeric. Significance level for a variable to be printed
#'   (default `0.05`).
#' @param min_n Integer. Smallest cell profiled by default (default `30`).
#'
#' @return The seg object, invisibly, with the full comparison in
#'   `seg[["needs"]][["reports"]][["profile_cut"]]`. Prints the most
#'   distinguishing variables per cell.
#'
#' @export
seg_needs_profile_cut <- function(seg, cells = NULL, blocks = NULL, vars = NULL,
                                  exclude_basis = TRUE, top_n = 10, alpha = 0.05, min_n = 30){

  cut <- seg[["needs"]][["cut"]]
  if(!is.list(cut) || !is.data.frame(cut[["persons"]])){
    stop("No cut found. Run seg_needs_cut() first.", call. = FALSE)
  }

  pf <- needs_person_frame(seg)

  shell_all <- dplyr::bind_rows(
    tidyr::unnest(seg[["shell"]][["polars"]],   cols = "vars"),
    tidyr::unnest(seg[["shell"]][["profiles"]], cols = "vars")
  )
  shell_all <- shell_all[shell_all$var %in% names(pf), c("prefix", "block_label", "var", "label")]

  # an explicit blocks / vars request is taken as asked, basis included
  n_basis <- 0
  if(exclude_basis && is.null(blocks) && is.null(vars)){
    basis <- needs_basis_vars(seg)
    n_basis <- sum(shell_all$var %in% basis)
    shell_all <- shell_all[!shell_all$var %in% basis, ]
  }

  if(!is.null(blocks) || !is.null(vars)){
    bad <- setdiff(blocks, shell_all$prefix)
    if(length(bad)) stop("Not a person-level spec block: ", paste(bad, collapse = ", "), call. = FALSE)
    bad <- setdiff(vars, shell_all$var)
    if(length(bad)) stop("Not a person-level spec variable: ", paste(bad, collapse = ", "), call. = FALSE)
    shell_all <- shell_all[shell_all$prefix %in% blocks | shell_all$var %in% vars, ]
  }

  key <- cut[["cells"]]
  if(is.null(cells)){
    cells <- key$code[key$n >= min_n]
  } else if(is.character(cells)){
    code <- key$code[match(cells, key$label)]
    if(anyNA(code)) stop("Unknown cell label(s): ", paste(cells[is.na(code)], collapse = ", "), call. = FALSE)
    cells <- code
  }

  df <- dplyr::inner_join(
    cut[["persons"]][!is.na(cut[["persons"]]$cell), c("person_id", "cell")],
    pf[c("person_id", shell_all$var)],
    by = "person_id"
  )
  X <- as.matrix(df[shell_all$var])

  col_stats <- function(rows){
    Xs <- X[rows, , drop = FALSE]
    n  <- colSums(!is.na(Xs))
    m  <- colSums(Xs, na.rm = TRUE) / n
    v  <- (colSums(Xs^2, na.rm = TRUE) - n * m^2) / pmax(n - 1, 1)
    list(n = n, m = m, v = pmax(v, 0))
  }
  total <- col_stats(rep(TRUE, nrow(X)))

  out <- lapply(cells, function(k){
    a <- col_stats(df$cell == k)
    b <- col_stats(df$cell != k)
    se <- sqrt(a$v / a$n + b$v / b$n)
    t  <- (a$m - b$m) / se
    dof <- se^4 / ((a$v / a$n)^2 / pmax(a$n - 1, 1) + (b$v / b$n)^2 / pmax(b$n - 1, 1))
    p  <- 2 * stats::pt(-abs(t), dof)
    p[!is.finite(p)] <- 1

    dplyr::tibble(
      cell       = k,
      cell_label = key$label[key$code == k],
      cell_n     = sum(df$cell == k),
      block      = shell_all$prefix,
      var        = shell_all$var,
      label      = shell_all$label,
      mean_cell  = round(a$m, 3),
      mean_rest  = round(b$m, 3),
      diff       = round(a$m - b$m, 3),
      index      = round(100 * a$m / total$m),
      p_value    = signif(p, 3)
    )
  }) %>% dplyr::bind_rows()


  # ---- report ---------------------------------------------------------------
  cat("\n=== Needs cut profile: ", length(cells), " cell(s) x ", nrow(shell_all),
      " person-level variables (vs the rest of the classified) ===\n", sep = "")
  if(n_basis > 0){
    cat("  ", n_basis, " variables built from the needs loop are left out - they restate the typing.\n",
        sep = "")
  }

  for(k in cells){
    x <- out[out$cell == k & out$p_value < alpha, ]
    x <- utils::head(x[order(-abs(x$diff)), ], top_n)
    cat("\n  ", key$label[key$code == k], " (n = ", sum(df$cell == k), ")\n", sep = "")
    if(nrow(x) == 0){
      cat("      nothing distinguishes this cell at p < ", alpha, "\n", sep = "")
      next
    }
    for(i in seq_len(nrow(x))){
      cat(sprintf("      %s %-55s %5.2f vs %5.2f  (idx %3d)\n",
                  if(x$diff[i] > 0) "+" else "-",
                  substr(paste(x$var[i], x$label[i], sep = " - "), 1, 55),
                  x$mean_cell[i], x$mean_rest[i], as.integer(x$index[i])))
    }
  }

  # A cell has a profile when clearly more variables separate it than chance
  # would pass at alpha - with hundreds of variables, a handful of hits is
  # what noise looks like.
  n_sig    <- tapply(out$p_value < alpha, out$cell, sum)
  expected <- alpha * nrow(shell_all)
  weak     <- as.integer(names(n_sig))[n_sig <= 2 * expected]

  cat("\n=== Verdict ===\n")
  cat("  significant variables per cell: ",
      paste0(key$label[match(as.integer(names(n_sig)), key$code)], " ", n_sig, collapse = " | "),
      "\n  (about ", round(expected), " per cell would clear p < ", alpha, " by chance alone)\n", sep = "")
  if(length(weak)){
    cat("  ", paste(key$label[match(weak, key$code)], collapse = ", "),
        " - no profile beyond chance. A cell nothing\n",
        "  separates is a cell the client cannot find or target.\n", sep = "")
  } else {
    cat("  Every cell profiled separates on more variables than chance explains.\n")
  }
  cat("  Unweighted.\n\n")

  seg[["needs"]][["reports"]][["profile_cut"]] <- out

  invisible(seg)
}
