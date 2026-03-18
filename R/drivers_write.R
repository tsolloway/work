#' drivers_write
#'
#' @description Takes the output of \code{drivers()} and writes formatted Excel
#'   workbooks. One workbook per DV, one sheet per IV set. Reads metadata
#'   (engine, shift_percentage, subgroups) from the result object.
#'
#' @param drivers_result The output of \code{drivers()}.
#' @param file_name Character. Prefix for output file names. Files are saved as
#'   \code{{file_name} - Drivers of {dv}.xlsx}. If NULL, inherits from
#'   \code{sub_title}. If both are NULL, files are saved as
#'   \code{Drivers of {dv}.xlsx}.
#' @param sub_title Character. Subtitle text (e.g. project name). Default NULL.
#' @param variable_width Column width for the Variable column (default 20).
#' @param label_width Column width for the Label column (default \code{"auto"}).
#' @param single_workbook Logical. If TRUE, all sheets are written to a single
#'   workbook. Sheet names are prefixed with the DV name (e.g.
#'   \code{{dv} - {ivs}}). Default FALSE.
#' @param path Directory to write workbooks to (default \code{"."}).
#'
#' @return Named list of workbook objects (invisibly).
#'
#' @export
drivers_write <- function(
    drivers_result,
    file_name = NULL,
    sub_title = NULL,
    variable_width = 20,
    label_width = "auto",
    single_workbook = FALSE,
    path = "."
){

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  meta <- drivers_result[["meta"]]
  results <- drivers_result[["results"]]
  engine_spec <- meta[["engine"]]
  shift_pct <- meta[["shift_percentage"]]
  subgroups <- meta[["subgroups"]]

  if (single_workbook) {

    # Build all DV-IV combos and generate sheet names
    combos <- do.call(rbind, lapply(names(results), function(dvn) {
      data.frame(dv = dvn, iv = names(results[[dvn]]), stringsAsFactors = FALSE)
    }))
    combos[["sheet"]] <- paste(combos[["dv"]], "-", combos[["iv"]])

    # Only truncate if any sheet name exceeds 31 chars
    if (any(nchar(combos[["sheet"]]) > 31)) {
      n_chars <- 5L
      repeat {
        combos[["sheet"]] <- paste(
          substr(combos[["dv"]], 1, n_chars), "-",
          substr(combos[["iv"]], 1, n_chars)
        )
        if (!anyDuplicated(combos[["sheet"]]) && all(nchar(combos[["sheet"]]) <= 31)) break
        n_chars <- n_chars + 1L
        if (n_chars > 14L) {
          combos[["sheet"]] <- substr(paste(combos[["dv"]], "-", combos[["iv"]]), 1, 31)
          break
        }
      }
    }
    sheet_lookup <- rlang::set_names(combos[["sheet"]], paste(combos[["dv"]], combos[["iv"]]))

    wb <- oxl_create_workbook()

    for (dvn in names(results)) {
      dvx <- results[[dvn]]

      if (is.list(engine_spec)) {
        xengine <- engine_spec[[dvn]]
      } else {
        xengine <- engine_spec
      }

      if (xengine == "linear") {
        footer <- "Drivers are estimated with OLS regression"
      } else if (xengine == "logistic") {
        footer <- glue::glue(
          "Drivers are estimated with logistic regression and impacts are calculated with a {shift_pct * 100}% shift in predictors"
        ) %>% as.character()
      }

      for (ivn in names(dvx)) {
        sheet_name <- sheet_lookup[[paste(dvn, ivn)]]

        wb <- append_drivers(
          analysis_table = dvx[[ivn]][["table"]],
          subgroups = subgroups,
          wb = wb,
          sheet_name = sheet_name,
          title = paste("Drivers of", dvn, "predicted by", ivn),
          sub_title = sub_title,
          footer = footer,
          variable_width = variable_width,
          label_width = label_width,
          engine = xengine
        )
      }
    }

    fname <- if (!is.null(file_name)) {
      glue::glue("{file_name} - Drivers.xlsx")
    } else {
      "Drivers.xlsx"
    }
    file_path <- file.path(path, fname)
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    invisible(list(wb))

  } else {

    workbooks <- purrr::imap(results, function(dvx, dvn) {

      wb <- oxl_create_workbook()

      if (is.list(engine_spec)) {
        xengine <- engine_spec[[dvn]]
      } else {
        xengine <- engine_spec
      }

      if (xengine == "linear") {
        footer <- "Drivers are estimated with OLS regression"
      } else if (xengine == "logistic") {
        footer <- glue::glue(
          "Drivers are estimated with logistic regression and impacts are calculated with a {shift_pct * 100}% shift in predictors"
        ) %>% as.character()
      }

      for (ivn in names(dvx)) {
        wb <- append_drivers(
          analysis_table = dvx[[ivn]][["table"]],
          subgroups = subgroups,
          wb = wb,
          sheet_name = ivn,
          title = paste("Drivers of", dvn, "predicted by", ivn),
          sub_title = sub_title,
          footer = footer,
          variable_width = variable_width,
          label_width = label_width,
          engine = xengine
        )
      }

      wb
    })

    purrr::iwalk(workbooks, function(wb, dvn) {
      fname <- if (!is.null(file_name)) {
        glue::glue("{file_name} - Drivers of {dvn}.xlsx")
      } else {
        glue::glue("Drivers of {dvn}.xlsx")
      }
      file_path <- file.path(path, fname)
      openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
    })

    invisible(workbooks)
  }
}
