#' seg_fa_name_winner
#'
#' @description Uses Claude to name the factors of a winning PCA solution.
#'   For each factor, sends Claude the variable labels that load on it
#'   (sorted by absolute loading, with the signed loading shown so it can
#'   weight by contribution) and asks for a concise 1-2 word thematic name.
#'   The new names are written into the `Name` column of the winning sheet in
#'   the PCA Excel workbook so they propagate through [seg_get_fa_winner()]
#'   and [seg_input_sheet()].
#'
#' @param seg A seg object with the PCA file path populated by [seg_pca()].
#' @param fa_winner Integer. The winning factor solution number (sheet name in
#'   the PCA workbook).
#' @param from_excel Logical. If `TRUE` (default), reads loading data directly
#'   from the PCA Excel workbook (matches the workflow where
#'   [seg_get_fa_winner()] has not been called yet). If `FALSE`, reads from
#'   `seg[["input_sheet"]][["input_fa_table"]]` (must be populated by
#'   [seg_get_fa_winner()] first) and additionally updates that table's
#'   `fa_name` column to mirror what's written to Excel.
#' @param max_words Integer. Maximum words per factor name (default `2`).
#' @param min_words Integer. Minimum words per factor name (default `1`).
#' @param model Character. Claude model ID (default
#'   `"claude-sonnet-4-5-20250929"`).
#' @param api_key Character or `NULL`. Anthropic API key. If `NULL` (default),
#'   reads from `ANTHROPIC_API_KEY` via [get_environment_key()].
#' @param row_header Integer. Row where the data header starts in the PCA
#'   sheet (default: `4`).
#' @param file_location Character. Path to the PCA Excel file. Defaults to
#'   `seg[["paths"]][["files"]][["pca"]]`.
#' @param quietly Logical. If `TRUE` (default), suppress all console output.
#'   Set to `FALSE` to print per-factor evidence and the resulting names.
#'
#' @return The seg object. Names are always written to the PCA Excel workbook
#'   at the `Name` column of the winning sheet. When `from_excel = FALSE`, the
#'   `fa_name` column of `seg[["input_sheet"]][["input_fa_table"]]` is also
#'   updated in place.
#'
#' @export
seg_fa_name_winner <- function(
    seg,
    fa_winner,
    from_excel = TRUE,
    max_words = 2,
    min_words = 1,
    model = "claude-sonnet-4-5-20250929",
    api_key = NULL,
    row_header = 4,
    file_location = NULL,
    quietly = TRUE
){


  if(is.null(api_key)){
    api_key <- get_environment_key("ANTHROPIC_API_KEY")
  }


  if(is.null(file_location)){
    file_location <- seg[["paths"]][["files"]][["pca"]]
  }


  if(is.null(file_location) || !file.exists(file_location)){
    cli::cli_abort(c(
      "PCA file not found.",
      "i" = "Run {.fn seg_pca} first, or pass {.arg file_location} explicitly."
    ))
  }


  # ---- pull per-row loadings: fa_n, label, loading ----
  if(from_excel){
    fa_data <- .fa_name_read_excel(
      file_location = file_location,
      fa_winner     = fa_winner,
      row_header    = row_header
    )
  }else{
    if(is.null(seg[["input_sheet"]][["input_fa_table"]])){
      cli::cli_abort(c(
        "{.field seg$input_sheet$input_fa_table} is empty.",
        "i" = "Call {.fn seg_get_fa_winner} first, or pass {.arg from_excel = TRUE}."
      ))
    }

    fa_data <- seg[["input_sheet"]][["input_fa_table"]] %>%
      dplyr::transmute(
        fa_n,
        label = source_label,
        loading
      )
  }


  # ---- sort per-row loadings by factor and absolute contribution ----
  arranged <- fa_data %>%
    dplyr::arrange(fa_n, -abs(loading))


  # ---- build evidence per factor, weighted by absolute loading ----
  evidence <- arranged %>%
    dplyr::group_by(fa_n) %>%
    dplyr::summarise(
      lines = paste(
        glue::glue("  - {label} (loading: {sprintf('%+.2f', loading)})"),
        collapse = "\n"
      ),
      .groups = "drop"
    )


  if(!quietly){
    cli::cli_h2("Naming {nrow(evidence)} factors (FA {fa_winner})")
    for(i in seq_len(nrow(evidence))){
      cli::cli_h3("Factor {evidence$fa_n[i]}")
      cli::cli_text(evidence$lines[i])
    }
  }


  # ---- call Claude ----
  prompt   <- .fa_name_build_prompt(
    evidence  = evidence,
    min_words = min_words,
    max_words = max_words
  )

  response <- .fa_name_call_claude(
    prompt  = prompt,
    model   = model,
    api_key = api_key
  )

  name_map <- .fa_name_parse_response(
    response        = response,
    factor_numbers  = evidence$fa_n
  )


  if(!quietly){
    cli::cli_h3("Results")
    for(g in names(name_map)){
      cli::cli_bullets(c("v" = "Factor {g} -> {.val {name_map[[g]]}}"))
    }
  }


  # ---- write Name column back to the PCA Excel sheet + add summary tab ----
  .fa_name_write_excel(
    file_location = file_location,
    fa_winner     = fa_winner,
    name_map      = name_map,
    arranged      = arranged,
    row_header    = row_header
  )


  cli::cli_alert_success("Wrote names to {.path {file_location}}")


  # ---- mirror back to seg's input_fa_table if it was the source ----
  if(!from_excel){
    seg[["input_sheet"]][["input_fa_table"]] <- seg[["input_sheet"]][["input_fa_table"]] %>%
      dplyr::mutate(
        fa_name = unlist(name_map[as.character(fa_n)])
      )
  }


  return(seg)
}


# ---- internal helpers --------------------------------------------------------

#' Read per-row loadings from the winning sheet of a PCA workbook.
#' @keywords internal
.fa_name_read_excel <- function(file_location, fa_winner, row_header){

  df <- openxlsx::read.xlsx(
    xlsxFile = file_location,
    sheet    = as.character(fa_winner),
    startRow = row_header
  ) %>%
    tibble::as_tibble() %>%
    tidyr::fill(Name) %>%
    dplyr::slice(., -nrow(.))


  factor_colnames <- df %>%
    dplyr::select(tidyselect::starts_with("F")) %>%
    dplyr::select(-Factor) %>%
    names()


  for(i in seq(nrow(df))){
    df[i, "loading"] <- df[[i, factor_colnames[df[[i, "Factor"]]]]]
  }


  df %>%
    dplyr::transmute(
      fa_n    = Factor,
      label   = Label,
      loading = loading
    )
}


#' Build the Claude prompt asking for one short name per factor.
#' @keywords internal
.fa_name_build_prompt <- function(evidence, min_words, max_words){

  factor_blocks <- purrr::map_chr(seq_len(nrow(evidence)), function(i){
    glue::glue("Factor {evidence$fa_n[i]}:\n{evidence$lines[i]}")
  })


  glue::glue(
    "You are naming factors from a principal components analysis (PCA) on ",
    "survey items. For each factor below you are given the variable labels ",
    "that load on it, sorted by absolute loading, with the signed loading ",
    "shown in parentheses. Higher absolute loading means the variable ",
    "contributes more to the factor and should weigh more in the name you ",
    "choose. The sign of the loading indicates direction within the polar ",
    "pair and does not change the underlying construct.\n\n",
    "Provide a short descriptive name ({min_words} to {max_words} words) per ",
    "factor that captures the common construct of its highest-loading ",
    "variables.\n\n",
    "IMPORTANT: every factor name must be UNIQUE. If two factors share a ",
    "theme, use a more specific qualifier (e.g. \"Functional Trust\" vs ",
    "\"Emotional Trust\") rather than repeating the same word. Duplicate ",
    "names break downstream Excel lookups.\n\n",
    "{paste(factor_blocks, collapse = '\n\n')}\n\n",
    "Respond with ONLY the names, one per line, in the format:\n",
    "Factor N: Name\n\n",
    "Do not include any other text, headers, or commentary."
  )
}


#' POST the prompt to the Anthropic Messages API and return raw text.
#' @keywords internal
.fa_name_call_claude <- function(prompt, model, api_key){

  body <- list(
    model      = model,
    max_tokens = 1024,
    messages   = list(
      list(role = "user", content = prompt)
    )
  )


  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key`         = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type`      = "application/json"
    ),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )


  if(httr::status_code(resp) != 200){
    err_body <- httr::content(resp, as = "text", encoding = "UTF-8")
    cli::cli_abort("Claude API error ({httr::status_code(resp)}): {err_body}")
  }


  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  parsed$content[[1]]$text
}


#' Parse the `Factor N: Name` lines into a named list keyed by factor number.
#' @keywords internal
.fa_name_parse_response <- function(response, factor_numbers){

  lines <- trimws(strsplit(response, "\n")[[1]])
  lines <- lines[nchar(lines) > 0]


  name_map <- list()
  for(line in lines){
    m <- regmatches(line, regexec("Factor\\s+(\\d+)\\s*:\\s*(.+)", line))[[1]]
    if(length(m) == 3){
      name_map[[m[2]]] <- trimws(m[3])
    }
  }


  missing <- setdiff(as.character(factor_numbers), names(name_map))
  if(length(missing) > 0){
    cli::cli_warn("Could not parse names for factor{?s} {missing}; falling back to {.val Factor N}.")
    for(g in missing){
      name_map[[g]] <- glue::glue("Factor {g}")
    }
  }


  if(anyDuplicated(unlist(name_map))){
    dupes <- unlist(name_map)
    dupes <- dupes[duplicated(dupes) | duplicated(dupes, fromLast = TRUE)]
    cli::cli_warn(c(
      "Claude returned duplicate factor names: {.val {unique(unname(dupes))}}.",
      "i" = "Downstream Excel lookups key off the name; re-run or rename manually if this is a problem."
    ))
  }


  name_map
}


#' Overwrite column B (Name) of the winning sheet with the new names, merge
#' the cells per factor with wrapped + centered text, and (re)write the
#' `summary` tab after `variance_explained`.
#' @keywords internal
.fa_name_write_excel <- function(file_location, fa_winner, name_map, arranged, row_header){

  # Read the current sheet to align name_map[factor] to every row.
  # PCA sheet rows are: header at `row_header`, var rows at row_header+1..N,
  # variance/footer row at the bottom (dropped here).
  current <- openxlsx::read.xlsx(
    xlsxFile = file_location,
    sheet    = as.character(fa_winner),
    startRow = row_header
  ) %>%
    tibble::as_tibble() %>%
    dplyr::slice(., -nrow(.))


  new_names <- unlist(name_map[as.character(current$Factor)])


  # Per-factor Excel row ranges (B-column) for merging.
  factor_runs <- current %>%
    dplyr::mutate(excel_row = dplyr::row_number() + row_header) %>%
    dplyr::group_by(Factor) %>%
    dplyr::summarise(
      start_row = min(excel_row),
      end_row   = max(excel_row),
      .groups   = "drop"
    )


  wb <- openxlsx::loadWorkbook(file_location)


  openxlsx::writeData(
    wb,
    sheet     = as.character(fa_winner),
    x         = new_names,
    startRow  = row_header + 1,
    startCol  = 2,                  # column B = Name
    colNames  = FALSE
  )


  # Merge B-column cells for each factor so the name spans its variable rows.
  for(i in seq_len(nrow(factor_runs))){
    openxlsx::mergeCells(
      wb,
      sheet = as.character(fa_winner),
      cols  = 2,
      rows  = factor_runs$start_row[i]:factor_runs$end_row[i]
    )
  }


  # Wrap text + center horizontally and vertically across the merged Name cells.
  merge_style <- openxlsx::createStyle(
    wrapText = TRUE,
    halign   = "center",
    valign   = "center"
  )

  openxlsx::addStyle(
    wb,
    sheet      = as.character(fa_winner),
    style      = merge_style,
    rows       = (row_header + 1):(row_header + nrow(current)),
    cols       = 2,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # ---- add / replace the summary tab ----
  .fa_name_write_descriptions(
    wb        = wb,
    arranged  = arranged,
    name_map  = name_map,
    fa_winner = fa_winner
  )


  openxlsx::saveWorkbook(wb, file_location, overwrite = TRUE)

  invisible(NULL)
}


#' Add (or replace) a `summary` sheet listing every factor's name and the
#' full variable / loading evidence that produced it. Placed right after the
#' `variance_explained` sheet.
#' @keywords internal
.fa_name_write_descriptions <- function(wb, arranged, name_map, fa_winner){

  sheet_name <- "summary"


  if(sheet_name %in% openxlsx::sheets(wb)){
    openxlsx::removeWorksheet(wb, sheet_name)
  }

  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)


  title_style   <- openxlsx::createStyle(textDecoration = "bold", fontSize = 18)
  header_style  <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)
  loading_style <- openxlsx::createStyle(halign = "right", numFmt = "+0.00;-0.00")
  label_style   <- openxlsx::createStyle(wrapText = TRUE, valign = "top")


  openxlsx::writeData(
    wb, sheet_name,
    x        = glue::glue("Factor Summary (FA {fa_winner})"),
    startRow = 1,
    startCol = 2,
    colNames = FALSE
  )

  openxlsx::addStyle(wb, sheet_name, style = title_style, rows = 1, cols = 2)


  row <- 3

  for(f in sort(unique(arranged$fa_n))){

    factor_rows <- arranged %>% dplyr::filter(fa_n == f)
    nvars       <- nrow(factor_rows)
    fname       <- name_map[[as.character(f)]]


    factor_header <- sprintf('Factor %d: "%s" (%d variable%s)',
                             f, fname, nvars, ifelse(nvars == 1, "", "s"))

    openxlsx::writeData(
      wb, sheet_name,
      x        = factor_header,
      startRow = row,
      startCol = 2,
      colNames = FALSE
    )

    openxlsx::addStyle(wb, sheet_name, style = header_style, rows = row, cols = 2)

    row <- row + 1


    openxlsx::writeData(
      wb, sheet_name,
      x        = factor_rows$loading,
      startRow = row,
      startCol = 2,
      colNames = FALSE
    )

    openxlsx::writeData(
      wb, sheet_name,
      x        = factor_rows$label,
      startRow = row,
      startCol = 3,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style      = loading_style,
      rows       = row:(row + nvars - 1),
      cols       = 2,
      gridExpand = TRUE,
      stack      = TRUE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style      = label_style,
      rows       = row:(row + nvars - 1),
      cols       = 3,
      gridExpand = TRUE,
      stack      = TRUE
    )

    row <- row + nvars + 1   # +1 blank line between factors
  }


  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 2)
  openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = 3, widths = 90)


  # Reorder so the summary tab sits immediately after variance_explained.
  sheets   <- openxlsx::sheets(wb)
  ve_idx   <- which(sheets == "variance_explained")
  desc_idx <- which(sheets == sheet_name)

  if(length(ve_idx) == 1 && length(desc_idx) == 1){
    other_idx <- setdiff(seq_along(sheets), c(ve_idx, desc_idx))
    openxlsx::worksheetOrder(wb) <- c(ve_idx, desc_idx, other_idx)
  }


  invisible(NULL)
}
