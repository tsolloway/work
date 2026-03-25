#' seg_reorder_solution
#' @description Reorder segment numbers for a solution. Takes an existing LDA
#'   solution and remaps the segment assignments and coefficient function to
#'   reflect a new ordering.
#' @param seg A seg object with solutions.
#' @param solution_old Character. The `lda_name` of the solution to reorder.
#' @param solution_new Character. The new `lda_name` for the reordered solution.
#' @param new_order Integer vector. New segment ordering
#'   (e.g., `c(6,2,3,4,5,1)` maps old seg 1 → new seg 6, etc.).
#' @return The seg object with the reordered solution added.
#' @export
seg_reorder_solution <- function(
    seg,
    solution_old,
    solution_new,
    new_order
){

  # solution_old = "LDA_opt_kmeans_cluster_N6"
  # solution_new = "Solution_A"
  # new_order = c(6,2,3,4,5,1)

  summary_table <- seg[["solutions"]][["summary_table"]]


  summary_table_new <- summary_table %>%
    dplyr::filter(lda_name == !!solution_old)


  if(!all(seq(summary_table_new[["n"]]) == sort(new_order))){
    stop("New order has more or less segments that previous.")
  }


  summary_table_new <- summary_table_new %>% dplyr::mutate(

    solution_name = "recode",
    cluster_name = solution_old,
    lda_name = solution_new,

    lda_coefficient_function = lda_coefficient_function %>%
      purrr::flatten_df() %>%
      dplyr::select(
        dplyr::all_of(c(1, 1 + new_order))
      ) %>%
      rlang::set_names(
        names(purrr::flatten_df(lda_coefficient_function))
      ) %>%
      list(),

    n_segments = list(sort(new_order)),

    df_solution = purrr::map(df_solution, function(d) {
      lda_col <- setdiff(names(d), c("id", solution_old))
      d %>%
        dplyr::mutate(
          !!solution_new := .data[[solution_old]] %>%
            dplyr::recode_values(
              !!!rlang::parse_exprs(glue::glue("{sort(new_order)}~{new_order}"))
            )
        ) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(lda_col),
            ~dplyr::recode_values(
              .x,
              !!!rlang::parse_exprs(glue::glue("{sort(new_order)}~{new_order}"))
            )
          )
        ) %>%
        dplyr::select(dplyr::all_of(c("id", solution_old, solution_new, lda_col)))
    })
  )


  seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]] <- dplyr::bind_rows(
    seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]],
    summary_table_new
  )


  seg[["solutions"]][["summary_table"]] <- seg_bind_summary_tables(seg)


  seg <- seg_build_with_solutions(seg)


  return(seg)

}

