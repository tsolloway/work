#' imap_progress
#'
#' A safe wrapper around `purrr::imap()` or `furrr::future_imap()` that
#' displays progress updates via `progressr`.
#'
#' @param .x A vector, list, or other iterable to iterate over.
#' @param .f A function of two arguments: element and name/index.
#' @param .label Optional string or function producing per-iteration messages.
#' @param .handlers Progressr handlers (default = `handler_cli`).
#' @param .parallel Logical; if TRUE, use `furrr::future_imap()`.
#' @param .furrr_packages Character vector of packages to load in parallel workers.
#' @param .furrr_globals Named list of globals to export to workers, or `TRUE`
#'   (default) for automatic detection. Use an explicit list to prevent
#'   `future` from recursively scanning the closure environment, which can
#'   inflate serialization size (e.g. when using `devtools::load_all()`).
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list identical to `purrr::imap()` output.
#' @export
imap_progress <- function(
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

  # Preserve handler state
  old_handlers <- getOption("progressr.handlers", NULL)
  on.exit({
    tryCatch(progressr::handlers(global = FALSE), error = function(e) NULL)
    options(progressr.handlers = old_handlers)
  }, add = TRUE)

  # Activate handlers
  options(progressr.handlers = .handlers)
  progressr::handlers(global = TRUE)

  # Graceful fallback if no plan is set
  if (.parallel && inherits(future::plan(), "sequential")) {
    cli::cli_alert_info("No active parallel plan detected; falling back to sequential mode.")
    .parallel <- FALSE
  }

  progressr::with_progress({
    p <- progressr::progressor(steps = length(.x) + 1)

    # Mapping logic shared by both modes
    map_fun <- function(.xi, .name, ...) {
      msg <- if (is.null(.label)) {
        as.character(.name)
      } else if (is.character(.label)) {
        glue::glue("{.label}: {.name}")
      } else if (is.function(.label)) {
        .label(.xi, .name)
      } else {
        stop("`.label` must be NULL, a string, or a function.")
      }

      p(message = msg)
      .f(.xi, .name, ...)
    }

    # When explicit globals are provided, merge in the internal variables
    # that map_fun references (.f, p, .label) so workers can find them
    if (is.list(.furrr_globals)) {
      .furrr_globals <- c(.furrr_globals, list(.f = .f, p = p, .label = .label))
    }

    # Branch explicitly: sequential vs. parallel
    if (.parallel) {
      res <- furrr::future_imap(
        .x,
        map_fun,
        ...,
        .options = furrr::furrr_options(packages = .furrr_packages, seed = TRUE, globals = .furrr_globals)
      )
    } else {
      res <- purrr::imap(.x, map_fun, ...)
    }

    if (!is.null(p)) p(amount = 0, message = NULL, class = "finish")

    res
  })
}
