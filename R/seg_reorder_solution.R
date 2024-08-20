#' seg_reorder_solution
#' @description seg_reorder_solution
#' @export
seg_reorder_solution <- function(
    seg, solution_old, solution_new, new_order
  ){


  summary_table <- seg[["solutions"]][["summary_table"]]


  summary_table_new <- summary_table %>%
    filter(lda_name == !!solution_old)



  if(!all(seq(summary_table_new[["n"]]) == sort(new_order))){
    stop("New order has more or less segments that previous.")
  }


  summary_table_new <- summary_table_new %>% mutate(

    solution_name = "recode",
    cluster_name = solution_old,
    lda_name = solution_new,

    lda_coefficient_function = lda_coefficient_function %>%
      flatten_df() %>%
      select(
        all_of(c(1, 1 + new_order))
      ) %>%
      setNames(
        names(flatten_df(lda_coefficient_function))
      ) %>%
      list(),

    lda_predict = lda_predict %>%
      flatten_df() %>%
      select(
        all_of(new_order)
      ) %>%
      mutate(
        seg = apply(., 1, which.max)
      ) %>%
      setNames(
        names(flatten_df(lda_predict))
      ) %>%
      list(),

    confusion = NA,

    df_append = df_append %>%
      flatten_df() %>%
      select(all_of(c("id", solution_old))) %>%
      mutate(
        !!solution_new := .data[[solution_old]] %>%
          case_match(
            !!!rlang::parse_exprs(glue("{sort(new_order)}~{new_order}"))
          )
      ) %>%
      select(-solution_old) %>%
      list()
  )




  seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]] <- bind_rows(
    seg[["solutions"]][["analysis"]][["reorder"]][["solution_table"]],
    summary_table_new
  )


  if(is.null(seg[["solutions"]][["analysis"]][["reorder"]][["df_segment_append"]])){

    seg[["solutions"]][["analysis"]][["reorder"]][["df_segment_append"]] <- summary_table_new[["df_append"]] %>% flatten_df()

  }else{

    seg[["solutions"]][["analysis"]][["reorder"]][["df_segment_append"]] <- full_join(
      seg[["solutions"]][["analysis"]][["reorder"]][["df_segment_append"]],
      summary_table_new[["df_append"]] %>% flatten_df(),
      by = "id"
    )

  }


  summary_table <- seg[["solutions"]][["analysis"]] %>%
    map(pluck, "solution_table") %>%
    bind_rows()



  df_segment_append <- seg[["solutions"]][["analysis"]] %>%
    map(pluck, "df_segment_append") %>%
    reduce(full_join, by = "id")



  df <- left_join(
    seg[["data"]][["with_shell"]],
    df_segment_append,
    by = join_by(seg_uuid == id)
  )


  seg[["solutions"]][["summary_table"]] <- summary_table
  seg[["solutions"]][["df_segment_append"]] <- df_segment_append
  seg[["data"]][["with_solutions"]] <- df


  return(seg)

}

