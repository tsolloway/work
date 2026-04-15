#' means_check_write
#'
#' @description Write means summary tables to a formatted Excel workbook.
#'   Accepts the output of `means_summaries()` or `stack_data()` as its first
#'   argument. When a `stack_data()` result is passed (with
#'   `include_mean_summaries = TRUE`), the pre-computed summaries are extracted
#'   automatically.
#'
#' @param input Named list — either the output of `means_summaries()` or the
#'   output of `stack_data()` (which contains a `mean_summaries` element).
#' @param file_name Character. Prefix for the output file name. File is saved as
#'   `{file_name} - Means Check.xlsx`.
#' @param sub_title Character or `NULL`. Subtitle text displayed in each sheet
#'   header. If `NULL` (default), inherits from `file_name`.
#' @param variable_width Numeric. Column width for the Variable column.
#'   Default 25.
#' @param label_width Numeric or `"auto"`. Column width for the Label column.
#'   Default `"auto"`.
#' @param wb An `openxlsx` workbook object. If `NULL` (default), a new one is
#'   created.
#' @param write_file If `TRUE` (default), save the workbook to disk.
#'
#' @return The input (invisibly), for continued piping.
#'
#' @export
means_check_write <- function(
    input,
    file_name = "ProjectName (Number)",
    sub_title = NULL,
    variable_width = 25,
    label_width = "auto",
    wb = NULL,
    write_file = TRUE
){

  if("mean_summaries" %in% names(input)){
    means_list <- input[["mean_summaries"]]
  } else {
    means_list <- input
  }

  if(is.null(sub_title)) sub_title <- file_name
  if(is.null(wb)) wb <- oxl_create_workbook()

  title_lookup <- list(
    subgroup_count                  = "Subgroup Count",
    means_stacked                   = "Means - All",
    means_stacked_weighted          = "Means - All Weighted",
    means_stacked_assigned          = "Means - Assigned",
    means_stacked_assigned_weighted = "Means - Assigned Weighted"
  )

  for(nm in names(means_list)){
    title <- title_lookup[[nm]] %||% nm
    sheet_name <- nm

    if(nm == "subgroup_count"){
      wb <- append_subgroup_summary(
        df_subgroup = means_list[[nm]],
        wb = wb,
        sheet_name = sheet_name,
        title = title,
        sub_title = sub_title,
        subgroup_width = variable_width,
        write_file = FALSE
      )
    } else {
      wb <- append_means_check(
        df_means = means_list[[nm]],
        wb = wb,
        sheet_name = sheet_name,
        title = title,
        sub_title = sub_title,
        variable_width = variable_width,
        label_width = label_width,
        write_file = FALSE
      )
    }
  }

  if(write_file){
    openxlsx::saveWorkbook(wb, glue::glue("{file_name} - Means Check.xlsx"), overwrite = TRUE)
  }

  invisible(input)

}
