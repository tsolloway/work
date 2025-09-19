#' subgroup_summary
#' @description subgroup_summary
#' @export
subgroup_summary <- function(
    df,
    subgroups,
    wb = NULL,
    sheet_name = NULL,
    title = "ProjectName (Number)",
    sub_title = "Subgroup Count",
    label_width = "auto",
    create_formatted_excel = TRUE,
    write_file = TRUE
){

  work::start()

  output <- list()


  df_subgroup <- df %>%
    select(all_of(subgroups)) %>%
    summarise_all(sum) %>%
    t() %>%
    as.data.frame() %>%
    mutate(
      Subroup = rownames(.) %>% gsub("_", " ", .),
      .before = 1
    ) %>%
    as_tibble() %>%
    rename(Count = V1)


  output[["subgroup_count"]] <- df_subgroup


  if(create_formatted_excel){

    df_summary_formatted <- append_subgroup_summary(
      df_subgroup = df_subgroup,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      label_width = label_width,
      write_file = write_file
    )

    output[["subgroup_count_formatted"]] <- df_summary_formatted

  }


  return(output)

}
