#' map_progress
#'
#' A safe wrapper around `purrr::map()` or `furrr::future_map()` that
#' displays progress updates via `progressr`. Each iteration updates a
#' progress bar and prints a contextual message (e.g., the current element
#' being processed).
#'
#' @param .x A vector, list, or other iterable to map over.
#' @param .f A function or formula to apply to each element.
#' @param .label Optional string or function producing per-iteration messages.
#'   If a string, it is prefixed to each element name (e.g., `"Running subgroup"`).
#'   If a function, it should accept `.x[[i]]` and return a message string.
#' @param .handlers A list of `progressr` handlers controlling progress output.
#'   Defaults to `progressr::handler_cli`, which prints a progress bar on the left
#'   and the message on the right.
#' @param .parallel Logical; if TRUE, use `furrr::future_map()` instead of
#'   `purrr::map()`. Requires an active `future::plan()`.
#' @param .furrr_packages Optional character vector of packages to load in each worker.
#' @param .furrr_globals Named list of globals to export to workers, or `TRUE`
#'   (default) for automatic detection. Use an explicit list to prevent
#'   `future` from recursively scanning the closure environment.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of mapped results, identical in structure to `purrr::map()`.
#' @export
map_progress <- function(
    .x,
    .f,
    ...,
    .label = NULL,
    .handlers = progressr::handler_cli,
    .parallel = FALSE,
    .furrr_packages = NULL,
    .furrr_globals = TRUE
) {
  # Tear down any lingering global handler before setting up a new one
  tryCatch(progressr::handlers(global = FALSE), error = function(e) NULL)

  # Save current handlers, restore on exit
  old_handlers <- getOption("progressr.handlers", NULL)
  on.exit({
    tryCatch(progressr::handlers(global = FALSE), error = function(e) NULL)
    options(progressr.handlers = old_handlers)
  }, add = TRUE)

  # Set up handlers temporarily
  options(progressr.handlers = .handlers)
  progressr::handlers(global = TRUE)

  # Graceful fallback if no active parallel plan
  if (.parallel && inherits(future::plan(), "sequential")) {
    cli::cli_alert_info("No active parallel plan detected; falling back to sequential mode.")
    .parallel <- FALSE
  }

  progressr::with_progress({
    p <- progressr::progressor(steps = length(.x) + 1)

    # Mapping logic shared by both modes
    map_fun <- function(.xi, ...) {
      msg <- if (is.null(.label)) {
        as.character(.xi)
      } else if (is.character(.label)) {
        glue::glue("{.label}: {.xi}")
      } else if (is.function(.label)) {
        .label(.xi)
      } else {
        stop("`.label` must be NULL, a string, or a function.")
      }

      p(message = msg)
      .f(.xi, ...)
    }

    if (is.list(.furrr_globals)) {
      .furrr_globals <- c(.furrr_globals, list(.f = .f, p = p, .label = .label))
    }

    # Branch: sequential or parallel execution
    if (.parallel) {
      res <- furrr::future_map(
        .x,
        map_fun,
        ...,
        .options = furrr::furrr_options(packages = .furrr_packages, seed = TRUE, globals = .furrr_globals)
      )
    } else {
      res <- purrr::map(.x, map_fun, ...)
    }

    # Finalize progress bar cleanly
    if (!is.null(p)) p(amount = 0, message = NULL, class = "finish")

    res
  })
}
