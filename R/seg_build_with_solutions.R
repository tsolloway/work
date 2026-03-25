#' seg_build_with_solutions
#'
#' @description Rebuilds `seg$data$with_solutions` by joining the base data
#'   (`with_solutions` or `with_shell`) with all segment assignment columns
#'   stored in the per-family `df_solution` list-columns. This is the inverse of
#'   [seg_bind_summary_tables()] — where that function builds the lightweight
#'   global summary, this function builds the wide respondent-level dataset.
#'
#'   The function is used internally by the clustering orchestrators after new
#'   solutions are added, but can also be called standalone to rebuild the data
#'   layer from the current solution state.
#'
#' @param seg A seg object with `seg$solutions$analysis` populated.
#' @param resp_id_name Character or `NULL`. Respondent ID column name in the
#'   base data. If `NULL`, auto-detected via [get_resp_id_name()].
#'
#' @return The seg object with `seg$data$with_solutions` rebuilt.
#'
#' @export
seg_build_with_solutions <- function(seg, resp_id_name = NULL) {


  if (is.null(resp_id_name)) {
    resp_id_name <- seg %>% get_resp_id_name()
  }


  df_segment_append <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows() %>%
    dplyr::pull(df_solution) %>%
    purrr::discard(~is.null(.x) || all(is.na(.x))) %>%
    purrr::reduce(function(x, y) {
      x %>%
        dplyr::select(!dplyr::any_of(setdiff(names(y), "id"))) %>%
        dplyr::left_join(y, by = "id")
    })


  df_base <- seg[["data"]][["with_solutions"]]
  if (is.null(df_base) || all(is.na(df_base))) {
    df_base <- seg[["data"]][["with_shell"]]
  }


  df_return <- dplyr::left_join(
    df_base %>%
      dplyr::select(
        !dplyr::any_of(
          names(df_segment_append) %>%
            tail(-1)
        )
      ),
    df_segment_append,
    by = rlang::set_names("id", resp_id_name)
  )


  seg[["data"]][["with_solutions"]] <- df_return


  return(seg)
}
