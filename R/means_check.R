#' means_check
#' @description means_check
#' @export
means_check <- function(
    df_stack,
    stack_labels,
    wb = NULL,
    sheet_name = NULL,
    title = "ProjectName (Number)",
    sub_title = "Means Check",
    variable_width = "auto",
    label_width = "auto",
    create_formatted_excel = TRUE,
    write_file = TRUE
){

  work::start()

  output <- list()


  df_summary <- df_stack %>%
    group_by(brand_number, brand_name) %>%
    summarise_at(
      c(names(stack_labels)) %>% unlist() %>% setNames(NULL),
      list(
        Mean = ~mean(., na.rm = TRUE),
        n = ~is.na(.) %>% not() %>% sum()
      )
    ) %>%
    ungroup() %>%
    mutate(
      Brand = glue("{brand_number} - {brand_name}")
    ) %>%
    relocate(Brand, .before = 1) %>%
    select(-brand_number, -brand_name) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    as_tibble() %>%
    janitor::row_to_names(1) %>%
    rename(Variable = Brand) %>%
    mutate_if(., names(.) != "Variable", as.numeric) %>%
    left_join(
      df_stack %>%
        summarise_at(
          c(names(stack_labels)) %>% unlist() %>% setNames(NULL),
          list(
            Mean = ~mean(., na.rm = TRUE),
            n = ~is.na(.) %>% not() %>% sum()
          )
        ) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble(),
      by = join_by(Variable == rowname)
    ) %>%
    relocate(Total = V1, .after = Variable) %>%
    mutate(
      Variable = Variable %>% gsub("_Mean", "", .)
    ) %>%
    left_join(
      tibble(
        Variable = names(stack_labels),
        Label = stack_labels
      ),
      by = join_by(Variable)
    ) %>%
    relocate(Label, .after = Variable) %>%
    mutate_if(is.numeric, ~round(., 4)) %>%
    mutate(grp = Variable %>% grepl("_n", .)) %>%
    group_split(grp) %>%
    bind_cols(.name_repair = "minimal") %>%
    setNames(
      ., c(head(names(.), length(.)/2), glue("{head(names(.), length(.)/2)} - N"))
    ) %>%
    select(order(colnames(.))) %>%
    relocate(Variable, Label, Total, "Total - N", .before = 1) %>%
    select(-c("grp - N", "Label - N", "Variable - N", "grp"))



  output[["means_summary"]] <- df_summary



  if(create_formatted_excel){

    df_summary_formatted <- append_means_check(
      df_means = df_summary,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      variable_width = variable_width,
      label_width = label_width,
      write_file = write_file
    )

    output[["means_summary_formatted"]] <- df_summary_formatted

  }



  return(output)

}
