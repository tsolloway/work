#' restart
#'
#' @description Custom restart function that executes common tasks:
#' removes objects, detaches packages, runs garbage collection, and optionally restarts the R session.
#'
#' @param keep Logical or character vector.
#'   - `FALSE` (default) drops all objects in the global environment unless `clean = FALSE`.
#'   - `TRUE` keeps all objects.
#'   - Character vector specifies which objects to keep.
#' @param run_gc Logical. If `TRUE` (default), runs `gc()` before restarting.
#' @param clean Logical. Passed to `.rs.restartR()`. If `FALSE`, the workspace is preserved unless `keep` is a character vector specifying objects to retain.
#' @param restart_session Logical. If `TRUE` (default), the R session is restarted.
#' @param start_after Logical. If `TRUE` (default), executes `work::start()` after restart.
#' @param restart_after_command Optional character vector of commands to execute after restart. If `NULL`, `work::start()` will be used if `start_after = TRUE`.
#'
#' @return Invisible `NULL`. The function's main effect is side-effects (cleaning environment, restarting R session).
#'
#' @export
restart <- function(
    keep = FALSE,
    run_gc = TRUE,
    clean = FALSE,
    restart_session = TRUE,
    start_after = TRUE,
    restart_after_command = NULL
) {

  # Detach work package safely
  code_to_eval <- "purrr::possibly(~detach('package:work', unload = TRUE))()"

  environment_objects <- ls(envir = .GlobalEnv)


  # Determine which objects to remove
  if (isFALSE(clean)) {

    if (is.character(keep)) {

      # Remove everything except specified objects
      environment_objects <- setdiff(environment_objects, keep)
      if (length(environment_objects) > 0) {
        code_to_eval <- c(code_to_eval, "rm(list = environment_objects, envir = .GlobalEnv)")
      }

    } else if (isFALSE(keep)) {

      # clean = FALSE and keep = FALSE → do nothing
      # (no objects removed)

    } else if (isTRUE(keep)) {

      # Keep all objects → do nothing

    }
  } else {

    # clean = TRUE → remove objects based on keep
    if (isFALSE(keep)) {

      code_to_eval <- c(code_to_eval, "rm(list = environment_objects, envir = .GlobalEnv)")

    } else if (is.character(keep)) {

      environment_objects <- setdiff(environment_objects, keep)
      if (length(environment_objects) > 0) {
        code_to_eval <- c(code_to_eval, "rm(list = environment_objects, envir = .GlobalEnv)")
      }

    } else if (isTRUE(keep)) {

      # Keep all objects → do nothing
    }
  }


  # Run garbage collection
  if (run_gc) {
    code_to_eval <- c(code_to_eval, "gc(verbose = FALSE, reset = TRUE)")
  }


  # Set commands to run after restart
  if (is.null(restart_after_command) && start_after) {
    restart_after_command <- "work::start()"
  } else if (!is.null(restart_after_command) && start_after) {
    restart_after_command <- c(restart_after_command, "work::start()")
  }


  # Restart session if requested
  if (restart_session) {

    if (is.null(restart_after_command)) {
      code_to_eval <- c(code_to_eval, glue::glue(".rs.restartR(clean = {clean})"))
    } else {
      after_cmd <- paste0(restart_after_command, collapse = "; ")
      code_to_eval <- c(code_to_eval, glue::glue(".rs.restartR(afterRestartCommand = '{after_cmd}', clean = {clean})"))
    }
  }


  # Remove empty lines
  code_to_eval <- code_to_eval[stringi::stri_length(code_to_eval) > 0]


  # Execute
  invisible(eval(parse(text = code_to_eval)))
}
