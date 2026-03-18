#' drivers
#'
#' @description Orchestrator for driver analysis. Loops over DVs and IV sets,
#'   calling \code{driver_impact()} for each combination. Pure computation —
#'   use \code{drivers_write()} to produce Excel output.
#'
#' @param df Data frame.
#' @param dv Character scalar, vector, or named vector. Dependent variable(s).
#'   If unnamed, names are set to values.
#' @param ivs Character vector or named list of character vectors. Independent
#'   variable set(s). If a plain vector, wrapped as \code{list(ivs = ivs)}.
#' @param engine Character scalar, named list, or NULL. One of \code{"linear"}
#'   or \code{"logistic"}. If a named list, names must match \code{names(dv)}
#'   to specify per-DV engines. If NULL, auto-detected per DV: logistic when
#'   the DV has exactly 2 unique non-NA values, linear otherwise.
#' @param subgroups Character vector or NULL. Names of 0/1 indicator columns.
#' @param dictionary Optional. A named object or named list of named objects
#'   (keyed by IV-set name) for label lookup.
#' @param shift_percentage Numeric. Passed to logistic engine (default 0.05).
#' @param weight Character or NULL. Weight column name.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{results}: nested list \code{results[[dv_name]][[iv_set_name]]}
#'       containing \code{driver_impact()} output (each with \code{table} and
#'       \code{fits}).
#'     \item \code{meta}: list with \code{engine}, \code{shift_percentage},
#'       \code{subgroups}, \code{dv}, \code{ivs}.
#'   }
#'
#' @export
drivers <- function(
    df,
    dv,
    ivs,
    engine = NULL,
    subgroups = NULL,
    dictionary = NULL,
    shift_percentage = 0.05,
    weight = NULL
){

  # Normalize inputs
  if (is.null(names(dv))) names(dv) <- dv

  if (!is.list(ivs) || tibble::is_tibble(ivs)) {
    ivs <- list(ivs = ivs)
  }

  # Auto-detect engine per DV if NULL
  if (is.null(engine)) {
    engine <- purrr::map_chr(rlang::set_names(dv, names(dv)), function(dvx) {
      n_unique <- length(unique(stats::na.omit(df[[dvx]])))
      if (n_unique == 2L) "logistic" else "linear"
    })
  }

  # Normalize engine: scalar → replicate for all DVs; vector → name by DV
  if (is.character(engine) && length(engine) > 1) {
    if (is.null(names(engine))) {
      if (length(engine) != length(dv)) stop("'engine' vector must be the same length as 'dv'.")
      engine <- rlang::set_names(as.list(engine), names(dv))
    } else {
      engine <- as.list(engine)
    }
  }


  results <- purrr::imap(dv, function(dvx, dvn) {

    purrr::imap(ivs, function(ivx, ivn) {

      # Resolve per-IV-set dictionary
      if (is.list(dictionary) && !tibble::is_tibble(dictionary)) {
        xdictionary <- dictionary[[ivn]]
      } else {
        xdictionary <- dictionary
      }

      # Resolve per-DV engine
      if (is.list(engine)) {
        xengine <- engine[[dvn]]
      } else {
        xengine <- engine
      }

      driver_impact(
        df = df,
        dv = dvx,
        ivs = ivx,
        engine = xengine,
        subgroups = subgroups,
        dictionary = xdictionary,
        shift_percentage = shift_percentage,
        weight = weight
      )
    })
  }) %>%
    suppressWarnings()


  list(
    results = results,
    meta = list(
      engine = engine,
      shift_percentage = shift_percentage,
      subgroups = subgroups,
      dv = dv,
      ivs = ivs
    )
  )
}
