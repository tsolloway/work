#' means_check_deliverable
#' @description means_check_deliverable
#' @export
means_check_deliverable <- function(
    df_stack,
    df_stack_assigned,
    subgroups,
    df_flat,
    stack_labels,
    title = "ProjectName (Number)",
    wb = NULL,
    write_file = TRUE
){
  work::start()
  require(openxlsx)

  if( is.null(wb) ) wb <- oxl_create_workbook()

  output <- list()


  if(!is.null(df_stack)){

    sheet_name <- "means_check"
    sub_title <- "Means Check"
    if(!is.null(df_stack_assigned)){
      sheet_name <- "means_all"
      sub_title <- "Means Check - Among All"
    }

    df_stack <- means_check(
      df_stack = df_stack,
      stack_labels = stack_labels,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = "Means Check - Among All",
      create_formatted_excel = TRUE,
      write_file = FALSE
    )


    output[["means_summary"]] <- df_stack[["means_summary"]]

    wb <- df_stack[["means_summary_formatted"]]

  }


  if(!is.null(df_stack_assigned)){

    sheet_name <- "means_check"
    sub_title <- "Means Check"
    if(!is.null(df_stack)){
      sheet_name <- "means_assigned"
      sub_title <- "Means Check - Among Assigned"
    }

    df_stack_assigned <- means_check(
      df_stack = df_stack_assigned,
      stack_labels = stack_labels,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      create_formatted_excel = TRUE,
      write_file = FALSE
    )


    output[["means_summary_assigned"]] <- df_stack_assigned[["means_summary"]]

    wb <- df_stack_assigned[["means_summary_formatted"]]

  }


  if(!is.null(df_flat) && !is.null(subgroups)){

    df_flat <- subgroup_summary(
      df = df_flat,
      subgroups = subgroups,
      wb = wb,
      sheet_name = "subgroup_count",
      title = title,
      sub_title = "Subgroup Count",
      label_width = "auto",
      create_formatted_excel = TRUE,
      write_file = FALSE
    )

    output[["subgroup_count"]] <- df_flat[["subgroup_count"]]

    wb <- df_flat[["subgroup_count_formatted"]]

  }

  output[["formatted_deliverable"]] <- wb



  if(write_file){
    saveWorkbook(wb, glue("{title} - Means Check.xlsx"), overwrite = TRUE)
  }


  return(output)

}
