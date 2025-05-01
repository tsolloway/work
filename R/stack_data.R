#' stack_data
#' @description stack_data
#' @export
stack_data <- function(
    instructions,
    df,
    uuid,
    stack_id,
    subgroups = NULL
){
  dfx <- instructions %>%
    imap(~{
      df %>% select(all_of(c(uuid, !!.x))) %>%
        tidyr::pivot_longer(
          -all_of(uuid),
          cols_vary = "fastest",
          values_to = .y
        ) %>%
        select(-name) %>%
        mutate(
          stack_num = rep(
            stack_id %>%
              select("id") %>%
              unlist(),
            nrow(df)
          ),
          stack_name = rep(
            stack_id %>%
              select("name") %>%
              unlist() %>%
              setNames(NULL),
            nrow(df)
          )
        ) %>%
        relocate(
          c("stack_num", "stack_name"),
          .after = all_of(uuid)
        )
    }) %>%
    plyr::join_all(by = c(uuid, "stack_num", "stack_name")) %>%
    as_tibble() %>%
    filter(
      rowSums(across(names(instructions), ~ !is.na(.))) > 0
    )



  if(!is.null(subgroups)){
    dfx <- dfx %>%
      right_join(
        df_flat %>%
          select(all_of(c(uuid, subgroups))),
        .,
        by = join_by(!!uuid)
      ) %>%
      relocate(
        c("stack_num", "stack_name"),
        .after = all_of(uuid)
      )
  }

  return(dfx)
}


