#' bn_impact_write
#'
#' @description Takes the output of \code{bn_impact()} and writes a formatted
#'   Excel workbook. Reads metadata (type, index_by, subgroups) from the result
#'   object.
#'
#' @param bn_impact_result The output of \code{bn_impact()}.
#' @param file_name Character. Prefix for output file name. File is saved as
#'   \code{{file_name} - Network Drivers of {dv}.xlsx}. If NULL, inherits from
#'   \code{sub_title}. If both are NULL, file is saved as
#'   \code{Network Drivers of {dv}.xlsx}.
#' @param sub_title Character. Subtitle text (e.g. project name). If NULL,
#'   inherits from \code{file_name}. Default NULL.
#' @param title Character. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Network Drivers of {dv}"} using the DV display name.
#' @param variable_width Column width for the Variable column (default 20).
#' @param label_width Column width for the Label column (default \code{"auto"}).
#' @param path Directory to write workbook to (default \code{"."}).
#'
#' @return Workbook object (invisibly).
#'
#' @export
bn_impact_write <- function(
    bn_impact_result,
    file_name = NULL,
    sub_title = NULL,
    title = NULL,
    variable_width = 20,
    label_width = "auto",
    path = "."
){

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  table <- bn_impact_result[["table"]]
  meta  <- bn_impact_result[["meta"]]
  subgroups <- meta[["subgroups"]]
  index_by  <- meta[["index_by"]]
  type      <- meta[["type"]]
  dv        <- meta[["dv"]]

  # Use name of dv if named, otherwise the value itself
dv_display <- if (!is.null(names(dv))) names(dv) else dv

  # Title
  if (is.null(title)) {
    title <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  }

  # Footer based on estimation type
  engine_footer <- switch(type,
    "gr" = "Impact estimated with exact conditional probability distributions",
    "cp" = "Impact estimated with Monte Carlo conditional probability",
    "mi" = "Impact estimated with mutual information",
    "Impact estimated with Bayesian network inference"
  )

  lift <- meta[["lift"]]

  index_footer <- if (index_by %in% c("lift_first", "lift_second")) {
    lift_idx <- if (index_by == "lift_first") 1L else min(2L, length(lift))
    lift_val <- lift[lift_idx]
    if (lift_val == 0) {
      "Indexed by average market lift"
    } else {
      paste0("Indexed by ", round(lift_val * 100), "% market lift")
    }
  } else {
    switch(index_by,
      "maxVmin" = "Indexed by max vs min impact",
      "mi"      = "Indexed by mutual information",
      "none"    = NULL
    )
  }

  footer <- paste(c(engine_footer, index_footer), collapse = ". ")

  # Sheet name
  sheet_name <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)

  wb <- oxl_create_workbook()

  wb <- append_bn_impact(
    analysis_table = table,
    subgroups = subgroups,
    wb = wb,
    sheet_name = sheet_name,
    title = title,
    sub_title = sub_title,
    footer = footer,
    variable_width = variable_width,
    label_width = label_width,
    index_by = index_by
  )

  # File name: "{file_name} - Network Drivers of {dv}.xlsx" or "Network Drivers of {dv}.xlsx"
  dv_suffix <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  fname <- if (!is.null(file_name)) {
    paste0(file_name, " - ", dv_suffix, ".xlsx")
  } else {
    paste0(dv_suffix, ".xlsx")
  }
  file_path <- file.path(path, fname)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  invisible(wb)
}
