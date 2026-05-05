#' install_pkg
#'
#' @description
#' Install R packages using `pak`, with a graceful fallback to
#' `utils::install.packages()` (source first, then binary) for any
#' package `pak` cannot install. Failures are collected and retried
#' one-at-a-time before the function reports back.
#'
#' @details
#' `upgrade` and `update_pkgs` are orthogonal:
#' * `upgrade` is forwarded to `pak::pkg_install()` and controls how
#'   aggressively dependencies of `x` are upgraded.
#' * `update_pkgs` decides whether all currently outdated packages
#'   (per `utils::old.packages()`) are added to the install set.
#'
#' @param x Character vector of package names to install. May be `NULL`
#'   when `update_pkgs = TRUE`.
#' @param ask Logical. Forwarded to `pak::pkg_install()`. If `TRUE`,
#'   prompts before replacing an installed package with a different
#'   version. New packages never trigger a prompt.
#' @param upgrade Logical. Forwarded to `pak::pkg_install()`. When
#'   `FALSE` (default) `pak` does the minimum work to satisfy `x`.
#' @param update_pkgs Logical. If `TRUE`, every package reported by
#'   `utils::old.packages()` is unioned into `x`.
#' @param on_exit_restart Logical. If `TRUE` (default), the R session
#'   is restarted via `work::restart(keep = TRUE)` when the function
#'   exits. The restart is only scheduled after input validation
#'   succeeds, so a bad call does not blow away the session.
#'
#' @returns `TRUE` when every requested package installed successfully,
#'   otherwise a character vector naming the packages that did not
#'   install (with a `warning()` listing them).
#'
#' @seealso [install_pak()], [restart()]
#' @export
install_pkg <- function(
    x = NULL,
    ask = FALSE,
    upgrade = FALSE,
    update_pkgs = FALSE,
    on_exit_restart = TRUE
){

  # Resolve target list ------------------------------------------------------

  if (update_pkgs) {
    outdated <- utils::old.packages()
    outdated <- if (is.null(outdated)) character() else as.data.frame(outdated)[["Package"]]
    x <- unique(c(x, outdated))
  }

  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    if (update_pkgs) {
      message("No packages need updating")
      return(TRUE)
    }
    stop("No packages identified")
  }

  # Schedule the restart only after input validation passed
  if (on_exit_restart) on.exit(work::restart(keep = TRUE))

  work::install_pak()

  # First pass: batch installs via pak, falling back per-package on errors --

  pending <- unique(x)
  failed  <- character()

  while (length(pending) > 0) {

    result <- .try_pak_install(pending, ask = ask, upgrade = upgrade)

    if (isTRUE(result$ok)) break

    bad <- result$pkg

    # Could not identify the offending package — give up on the batch
    if (is.null(bad) || !nzchar(bad)) {
      failed  <- unique(c(failed, pending))
      pending <- character()
      break
    }

    # Try base install.packages (source then binary) for the offender
    if (!isTRUE(.try_base_install(bad))) {
      failed <- unique(c(failed, bad))
    }

    remaining <- setdiff(pending, c(bad, failed))

    # No forward progress — bail to avoid an infinite loop
    if (identical(remaining, pending)) {
      failed  <- unique(c(failed, pending))
      pending <- character()
      break
    }

    pending <- remaining
  }

  # Second pass: retry failures one-at-a-time --------------------------------

  if (length(failed) > 0) {
    still_failed <- character()
    for (pkg in failed) {
      retry <- .try_pak_install(pkg, ask = ask, upgrade = upgrade)
      if (isTRUE(retry$ok)) next
      if (!isTRUE(.try_base_install(pkg))) {
        still_failed <- c(still_failed, pkg)
      }
    }
    failed <- still_failed
  }

  if (length(failed) == 0) return(TRUE)

  warning(as.character(glue::glue(
    "The following packages didn't install: ",
    "{glue::glue_collapse(failed, sep = ', ', last = ' and ')}"
  )))
  failed
}


# Internal helpers ============================================================

# Run pak::pkg_install once and report back as list(ok, pkg).
# `pkg` is the package name parsed from the error message (NULL if
# unidentifiable).
.try_pak_install <- function(x, ask, upgrade) {
  tryCatch(
    {
      pak::pkg_install(x, ask = ask, upgrade = upgrade)
      list(ok = TRUE, pkg = NULL)
    },
    error = function(e) {
      list(ok = FALSE, pkg = .parse_failed_pkg(e, x))
    }
  )
}


# Best-effort parse of a pak error to identify which package failed.
# Matches against the candidate list first (robust across pak versions);
# falls back to NULL when nothing matches.
.parse_failed_pkg <- function(e, candidates) {

  msg <- e[["parent"]][["message"]]
  if (is.null(msg) || !nzchar(msg)) msg <- conditionMessage(e)
  if (length(msg) == 0 || !nzchar(msg)) return(NULL)

  hits <- candidates[vapply(
    candidates,
    function(p) grepl(p, msg, fixed = TRUE),
    logical(1)
  )]

  if (length(hits) > 0) {
    # Prefer the longest match so e.g. "ggplot2" wins over "ggplot"
    return(hits[which.max(nchar(hits))])
  }

  if (grepl("rJava", msg, fixed = TRUE)) {
    warning("rJava failed; install it outside of this wrapper")
  }

  NULL
}


# Try utils::install.packages() as source, then binary. Returns
# TRUE/FALSE based on whether the package is findable afterward.
.try_base_install <- function(pkg) {

  attempt <- function(type) {
    tryCatch(
      {
        utils::install.packages(pkg, type = type, dependencies = TRUE)
        nzchar(system.file(package = pkg))
      },
      error = function(e) FALSE
    )
  }

  if (isTRUE(attempt("source"))) return(TRUE)
  isTRUE(attempt("binary"))
}
