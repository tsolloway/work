#' driver_impact
#'
#' @description Wrapper for driver engines. Handles subgroup iteration,
#'   column prefixing, dictionary joins, and index renaming. Mirrors the
#'   \code{bn_impact()} pattern.
#'
#' @param df Data frame.
#' @param dv Character. Name of the dependent variable column.
#' @param ivs Character vector. Names of independent variable columns.
#' @param engine Character. One of \code{"linear"} or \code{"logistic"}.
#' @param subgroups Character vector or NULL. Names of 0/1 indicator columns
#'   in \code{df} used to filter subgroups. If NULL, computes on total.
#' @param dictionary Optional. A named object (vector, list, or tibble) passed
#'   to \code{dictionary_from_named_object()} to produce a \code{var}/\code{label}
#'   lookup. Joined to output on \code{Variable}.
#' @param shift_percentage Numeric. Fraction of IV range for logistic prob shift
#'   (default 0.05). Ignored for linear engine.
#' @param weight Character or NULL. Column name for weights.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{table}: tibble with \code{Variable}, optional \code{Label},
#'       and subgroup-prefixed metric columns.
#'     \item \code{fits}: named list of fit lists, keyed by subgroup name.
#'   }
#'
#' @export
driver_impact <- function(
    df,
    dv,
    ivs,
    engine = c("linear", "logistic"),
    subgroups = NULL,
    dictionary = NULL,
    shift_percentage = 0.05,
    weight = NULL
){

  engine <- match.arg(engine)

  # If no subgroups, run on total
  if (is.null(subgroups)) {
    subgroups <- "Total"
    df[["Total"]] <- 1L
  }

  first_subgroup <- subgroups[[1]]

  # Run engine per subgroup
  all_fits <- list()

  run_engine <- function(sub_df) {
    if (engine == "linear") {
      driver_engine_linear(df = sub_df, dv = dv, ivs = ivs, weight = weight)
    } else {
      driver_engine_logistic(df = sub_df, dv = dv, ivs = ivs, shift_percentage = shift_percentage, weight = weight)
    }
  }

  output <- purrr::imap(
    rlang::set_names(subgroups),
    function(.x, .y) {
      sub_df <- df %>%
        dplyr::filter(.data[[.y]] == 1) %>%
        droplevels()

      result <- run_engine(sub_df)
      all_fits[[.y]] <<- result[["fits"]]

      result[["table"]] %>%
        rlang::set_names(glue::glue("{.y}_{names(.)}"))
    }
  ) %>%
    dplyr::bind_cols() %>%
    dplyr::rename(Variable = !!paste0(first_subgroup, "_variable")) %>%
    dplyr::select(-dplyr::ends_with("_variable"))

  # Strip _index suffix from subgroup-prefixed index columns
  names(output) <- names(output) %>% gsub("_index$", "", .)


  # Dictionary join
  if (!is.null(dictionary)) {

    dictionary <- dictionary_from_named_object(dictionary)

    output <- output %>%
      dplyr::left_join(
        dictionary,
        by = dplyr::join_by(Variable == var)
      ) %>%
      dplyr::relocate(label, .after = "Variable") %>%
      dplyr::rename("Label" = "label")
  }


  list(
    table = output,
    fits = all_fits
  )
}
