#' create_means_check
#' @description create_means_check
#' @export
create_means_check <- function(
    obj = NULL,
    df_stack = NULL,
    dictionary_stack = NULL,
    df_flat = NULL,
    remove_df_flat_storage = TRUE
){

  if(!is.null(obj)){

    df_stack <- obj[["df_stack"]]
    dictionary_stack <- obj[["dictionary_stack"]]
    df_flat <- obj[["df_flat"]]

    if(remove_df_flat_storage){
      obj[["df_flat"]] <- NULL
    }

    results <- obj

  }else{

    results <- list()

  }


  results[["df_mean_summary"]] <- df_stack %>%
    group_by(brand_number, brand_name) %>%
    summarise_at(
      dictionary_stack %>%
        filter(dvs|ivs) %>%
        select(var) %>%
        unlist() %>%
        setNames(NULL),
      list(
        Mean = ~mean(., na.rm = TRUE),

        TB_Mean = ~ case_when(
          is.na(.) ~ NA,
          0 ~ 0,
          . == max(., na.rm = TRUE) ~ 1,
          .default = 0
        ) %>% mean(na.rm = TRUE),

        T2B_Mean = ~ case_when(
          is.na(.) ~ NA,
          0 ~ 0,
          . %in% top2(.) ~ 1,
          .default = 0
        ) %>% mean(na.rm = TRUE),

        BB_Mean = ~ case_when(
          is.na(.) ~ NA,
          .default = 0
        ) %>% mean(na.rm = TRUE),

        B2B_Mean = ~ case_when(
          is.na(.) ~ NA,
          . %in% bottom2(.) ~ 1,
          .default = 0
        ) %>% mean(na.rm = TRUE),

        n = ~is.na(.) %>% not() %>% sum()
      )
    ) %>%
    ungroup() %>%
    mutate_all(
      ~tidyr::replace_na(., NA)
    ) %>%
    suppressWarnings()


  #transpose means summary
  results[["df_mean_summary"]] <- results[["df_mean_summary"]] %>%
    select(-c(brand_name, brand_number)) %>%
    t() %>%
    as.data.frame() %>%
    setNames(
      glue("{results[['df_mean_summary']][['brand_number']]} - {results[['df_mean_summary']][['brand_name']]}")
    ) %>%
    bind_cols(
      variable = rownames(.),
      .
    ) %>%
    as_tibble()


  results[["subgroup_count"]] <- df_flat %>%
    summarise_at(
      .vars = dictionary_stack %>%
        filter(subgroup) %>%
        select(var) %>%
        unlist() %>%
        setNames(NULL),
      sum
    ) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = "Subgroups",
      values_to = "Count"
    )


  return(results)
}
