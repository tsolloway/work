#' write_file
#'
#' @description Write a data frame or openxlsx workbook to disk.
#' Supports common locations like desktop, downloads, OneDrive.
#'
#' @param x Data frame or openxlsx workbook.
#' @param where Character; where to write: "here", "desktop", "downloads", "onedrive", or "file".
#' @param file Optional file name (with or without extension).
#' @param type File type: "csv" or "xlsx".
#' @export
write_file <- function(
    x,
    where = c("here", "desktop", "downloads", "onedrive", "file"),
    file = NULL,
    type = c("csv", "xlsx")
){

  type <- match.arg(type)
  where <- match.arg(where)

  # Auto-type for special objects
  if (is_truthy(attr(x, "write_type")) && is_truthy(attr(x, "analysis"))) {
    if (attr(x, "write_type") == "openxlsx_formatted" && attr(x, "analysis") == "dmd_check") {
      type <- "xlsx"
      if (is.null(file)) file <- paste0("dmd-check-", Sys.Date(), ".xlsx")
    }
  }


  # Capture unevaluated expression
  x_expr <- rlang::enquo(x)
  x_val <- rlang::eval_tidy(x_expr)


  # --- Attempt to infer object name ---
  if (is.null(file)) {

    # 1. Try tidy evaluation name
    file <- tryCatch(rlang::as_name(x_expr), error = function(e) NULL)

    # 2. If piped or ".", walk back through sys.calls() for earliest symbol
    if (is.null(file) || file %in% c(".", "x", "")) {
      calls <- sys.calls()
      # Find all symbols in the call stack
      syms <- unique(unlist(lapply(calls, all.names)))
      # Filter out common verbs and keep plausible variable names
      syms <- setdiff(syms, c("%>%", "<-", "write_file", "mutate", "filter", "select", "arrange"))
      # Pick the first name that exists in the global env
      file <- purrr::keep(syms, ~ exists(.x, envir = .GlobalEnv)) %>% purrr::pluck(1, .default = "output")
    }
  }

  # Ensure correct extension
  if (tolower(tools::file_ext(file)) != type) file <- paste0(file, ".", type)

  # Determine full path
  where <- switch(
    where,
    "here" = getwd(),
    "desktop" = get_path("desktop"),
    "downloads" = get_path("downloads"),
    "onedrive" = get_path("onedrive"),
    "file" = ""
  )

  file <- file.path(where, file)

  if(!dir.exists(where)) dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  # Write file
  if (type == "csv") {
    write.csv(x = x, file = file, row.names = FALSE, na = "")
  } else if (type == "xlsx") {
    openxlsx::saveWorkbook(wb = x, file = file, overwrite = TRUE)
  }

  invisible(file)
}
