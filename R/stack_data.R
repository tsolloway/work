#' stack_data
#' @description stack_data
#' @export
stack_data <- function(
    df,
    instructions,
    labels,
    brand_table,
    dv = NULL,
    ivs = NULL,
    subgroups = NULL,
    uuid_flat,
    uuid_stack = "uuid_stack",
    only_completes = TRUE,
    store_flat = TRUE,
    return_only_data = FALSE
){

  var_types <- list(
    ids = c(uuid_stack, uuid_flat, "brand_number", "brand_name"),
    subgroups = subgroups %>% unlist() %>% as.character(),
    dvs = dv %>% unlist() %>% as.character(),
    ivs = ivs %>% map(~unlist(.) %>% as.character())
  )


  dictionary <- tibble(
    var = var_types %>% unlist(),
    id = var %in% var_types[["ids"]],
    subgroup = var %in% var_types[["subgroups"]],
    dvs = var %in% var_types[["dvs"]],
    ivs = var %in% unlist(var_types[["ivs"]])
  ) %>% left_join(
    labels %>%
      tibble(
        var = names(.),
        label = .
      ),
    by = join_by(var)
  ) %>%
    left_join(
      var_types[["ivs"]] %>%
        imap(
          ~tibble(
            iv_battery = .y,
            var = .x
          )
        ) %>%
        bind_rows(),
      by = join_by(var)
    ) %>%
    left_join(
      instructions %>%
        imap(
          ~tibble(
            var = .y,
            stack_instructions = list(.x %>% unlist() %>% as.character)
          )
        ) %>%
        bind_rows(),
      by = join_by(var)
    ) %>%
    mutate(
      label = case_when(
        var == uuid_flat ~ uuid_flat,
        var == uuid_stack ~ uuid_stack,
        is.na(label)  ~ var %>% gsub("_", " ", .) %>% stringr::str_to_title(),
        .default = label
      )
    ) %>%
    relocate(
      label, .after = var
    )



  df_stack <- instructions %>%
    imap(~{
      df %>% select(all_of(c(uuid_flat, !!.x))) %>%
        tidyr::pivot_longer(
          -all_of(uuid_flat),
          cols_vary = "fastest",
          values_to = .y
        ) %>%
        select(-name) %>%
        mutate(
          brand_number = rep(
            brand_table %>%
              select("id") %>%
              unlist(),
            nrow(df)
          ),
          brand_name = rep(
            brand_table %>%
              select("name") %>%
              unlist() %>%
              setNames(NULL),
            nrow(df)
          )
        ) %>%
        relocate(
          c("brand_number", "brand_name"),
          .after = all_of(uuid_flat)
        )
    }) %>%
    plyr::join_all(by = c(uuid_flat, "brand_number", "brand_name")) %>%
    as_tibble()



  if(only_completes){
    df_stack <- df_stack %>%
      filter(
        rowSums(across(names(instructions), ~ !is.na(.))) > 0
      )
  }



  if(!is.null(subgroups)){
    df_stack <- df_stack %>%
      right_join(
        df_flat %>%
          select(all_of(c(uuid_flat, subgroups))),
        .,
        by = join_by(!!uuid_flat)
      )
  }


  df_stack <- df_stack %>%
    mutate(
      {{uuid_stack}} := nrow(.) %>% uuid::UUIDgenerate(n = .)
    ) %>%
    select(
      dictionary[["var"]] %>% setNames(NULL)
    )


  result <- list(
    df_stack = df_stack,
    dictionary_stack = dictionary,
    var_types = var_types,
    df_mean_summary = NULL,
    subgroup_count = NULL
  )


  if(store_flat){
    result[["df_flat"]] <- df_flat
  }


  if(return_only_data){
    result <- df_stack
  }


  return(result)
}


