#' seg_generate_spec
#'
#' @description Generate a segmentation spec Excel workbook with Polars and Profiles sheets.
#' Preserves template formulas for computed columns.
#'
#' @param project_name character, project display name (appears in header)
#' @param polar_blocks list of polar block definitions (from \code{\link{seg_create_polar_block}})
#' @param profile_blocks list of profile block definitions (from \code{\link{seg_create_profile_block}})
#' @param version integer, label format version (1 = parenthetical, 2 = double-pipe). Default 1.
#' @param output_path character, file path for output xlsx. Default: \code{project_name - Specs.xlsx} in working directory.
#' @return output path (invisibly)
#' @export
seg_generate_spec <- function(
  project_name,
  polar_blocks,
  profile_blocks,
  version = 1,
  output_path = NULL
) {

  if (is.null(output_path)) {
    output_path <- file.path(getwd(), paste0(project_name, " - Specs.xlsx"))
  }



  wb <- oxl_create_workbook()
  .write_polars_sheet(wb, polar_blocks, version)
  .write_profiles_sheet(wb, profile_blocks, project_name)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

  total_polars   <- sum(vapply(polar_blocks, function(b) nrow(b$pairs), integer(1)))
  total_profiles <- sum(vapply(profile_blocks, function(b) nrow(b$items), integer(1)))
  message("Spec file saved to: ", output_path)
  message("Total polar items: ", total_polars, " across ", length(polar_blocks), " blocks")
  message("Total profile items: ", total_profiles, " across ", length(profile_blocks), " blocks")
  message("Grand total: ", total_polars + total_profiles, " variables")

  invisible(output_path)
}


# ============================================================
# INTERNAL HELPERS (not exported)
# ============================================================

.HEADER_FILLS <- c("#D9E2F3", "#E2EFDA", "#FCE4D6", "#EDEDED", "#D6DCE4", "#FFF2CC")


.make_polar_source_var <- function(source_pattern, index) {
  if (source_pattern == "G2") {
    if (index <= 10) paste0("G2x1r", index) else paste0("G2x2r", index)
  } else {
    paste0(source_pattern, "r", index)
  }
}


.build_profile_syntax <- function(r) {
  hs <- paste0("IFERROR(FIND(\"/\",H", r, "),FALSE)")
  hc <- paste0("IFERROR(FIND(\",\",G", r, "),FALSE)")
  vb <- paste0("H", r, "=\"\"")
  cp <- paste0("OR(LEFT(H", r, ",1)=\">\",LEFT(H", r, ",1)=\"<\")")

  # ZF=1 tree: NAs become 0
  zf <- paste0(
    "IF(AND(", hs, ",", hc, "),",
      "E", r, "&\" = rowMeans(across(c(\"&G", r, "&\"), ~ replace_na(.x, 0))) \"&H", r, ",",
    "IF(AND(", hs, ",NOT(", hc, ")),",
      "E", r, "&\" = replace_na(\"&G", r, "&\", 0) \"&H", r, ",",
    "IF(AND(", vb, ",", hc, "),",
      "E", r, "&\" = rowMeans(across(c(\"&G", r, "&\"), ~ replace_na(.x, 0)))\",",
    "IF(AND(", vb, ",NOT(", hc, ")),",
      "E", r, "&\" = replace_na(\"&G", r, "&\", 0)\",",
    "IF(", cp, ",",
      "E", r, "&\" = case_when(\"&G", r, "&\" \"&H", r, "&\" ~ 1, is.na(\"&G", r, "&\") ~ 0, .default = 0)\",",
    "IF(", hc, ",",
      "E", r, "&\" = as.integer(if_any(c(\"&G", r, "&\"), ~ replace_na(.x, 0) %in% \"&H", r, "&\"))\",",
    "E", r, "&\" = recode_values(\"&G", r, "&\", \"&H", r, "&\" ~ 1, default = 0)\"))))))")

  # ZF=0 tree: NAs propagate
  nozf <- paste0(
    "IF(AND(", hs, ",", hc, "),",
      "E", r, "&\" = rowMeans(across(c(\"&G", r, "&\")), na.rm = TRUE) \"&H", r, ",",
    "IF(AND(", hs, ",NOT(", hc, ")),",
      "E", r, "&\" = \"&G", r, "&\" \"&H", r, ",",
    "IF(AND(", vb, ",", hc, "),",
      "E", r, "&\" = rowMeans(across(c(\"&G", r, "&\")), na.rm = TRUE)\",",
    "IF(AND(", vb, ",NOT(", hc, ")),",
      "E", r, "&\" = \"&G", r, ",",
    "IF(", cp, ",",
      "E", r, "&\" = case_when(\"&G", r, "&\" \"&H", r, "&\" ~ 1, .default = 0)\",",
    "IF(", hc, ",",
      "E", r, "&\" = as.integer(if_any(c(\"&G", r, "&\"), ~ .x %in% \"&H", r, "&\"))\",",
    "E", r, "&\" = recode_values(\"&G", r, "&\", \"&H", r, "&\" ~ 1, NA ~ NA, default = 0)\"))))))")

  paste0("IF(G", r, "=\"\",\"\",IF(I", r, "=1,", zf, ",", nozf, "))")
}


.write_polars_sheet <- function(wb, polar_blocks, version) {
  sheet <- "Polars"
  openxlsx::addWorksheet(wb, sheet)

  openxlsx::setColWidths(wb, sheet, cols = 1:14,
    widths = c(20.83, 1.83, 5.83, 5.83, 10.83, 70.83, 10, 10,
               40.83, 40.83, 10, 40.83, 53.66, 54.0))

  openxlsx::freezePane(wb, sheet, firstActiveRow = 4)

  s_bold_14 <- openxlsx::createStyle(textDecoration = "Bold", fontSize = 14)
  s_bold    <- openxlsx::createStyle(textDecoration = "Bold")
  s_center  <- openxlsx::createStyle(halign = "center", valign = "center")
  s_bold_c  <- openxlsx::createStyle(textDecoration = "Bold", halign = "center", valign = "center")

  # Row 1: title link + version toggle
  openxlsx::writeFormula(wb, sheet, x = "Profiles!F1", startRow = 1, startCol = 6)
  openxlsx::addStyle(wb, sheet, s_bold_14, rows = 1, cols = 6)
  openxlsx::writeData(wb, sheet, x = "version", startRow = 1, startCol = 7, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, s_center, rows = 1, cols = 7)
  openxlsx::writeData(wb, sheet, x = version, startRow = 1, startCol = 8, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, s_center, rows = 1, cols = 8)

  # Row 3: column headers
  hdrs <- c(A = "Notes", C = "Block", D = "Num", E = "Var", F = "Label",
            G = "Source Var", H = "Side Lead", I = "Left Label", J = "Right Label",
            K = "Value", L = "Syntax", M = "Source Label", N = "Opposite Label")
  hdr_cols <- match(names(hdrs), LETTERS)
  for (i in seq_along(hdrs)) {
    openxlsx::writeData(wb, sheet, x = hdrs[i], startRow = 3, startCol = hdr_cols[i], colNames = FALSE)
  }
  openxlsx::addStyle(wb, sheet, s_bold_c, rows = 3, cols = hdr_cols, gridExpand = TRUE)

  row <- 4L
  for (block_idx in seq_along(polar_blocks)) {
    blk  <- polar_blocks[[block_idx]]
    fill <- openxlsx::createStyle(fgFill = .HEADER_FILLS[((block_idx - 1) %% length(.HEADER_FILLS)) + 1])

    all_cols <- c(1, 3:14)
    openxlsx::addStyle(wb, sheet, fill, rows = row, cols = all_cols, gridExpand = TRUE, stack = TRUE)

    # Block number
    if (block_idx == 1) {
      openxlsx::writeData(wb, sheet, x = 1L, startRow = row, startCol = 3, colNames = FALSE)
    } else {
      openxlsx::writeFormula(wb, sheet, x = paste0("MAX(C4:C", row - 1, ")+1"),
                   startRow = row, startCol = 3)
    }
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 3, stack = TRUE)

    # Prefix + block name
    openxlsx::writeData(wb, sheet, x = blk$prefix, startRow = row, startCol = 5, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 5, stack = TRUE)
    openxlsx::writeData(wb, sheet, x = blk$block_name, startRow = row, startCol = 6, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 6, stack = TRUE)

    header_row <- row
    row <- row + 1L

    # Data rows
    pairs <- blk$pairs
    for (i in seq_len(nrow(pairs))) {
      num <- sprintf("%02d", i)
      src <- .make_polar_source_var(blk$source_pattern, i)
      r   <- row

      openxlsx::writeData(wb, sheet, x = num,             startRow = r, startCol = 4,  colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = src,             startRow = r, startCol = 7,  colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = pairs$lead[i],   startRow = r, startCol = 8,  colNames = FALSE)
      openxlsx::addStyle(wb, sheet, s_center, rows = r, cols = 8)
      openxlsx::writeData(wb, sheet, x = pairs$left[i],   startRow = r, startCol = 9,  colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = pairs$right[i],  startRow = r, startCol = 10, colNames = FALSE)

      # E: Var = prefix & num
      openxlsx::writeFormula(wb, sheet, x = paste0("$E$", header_row, "&D", r),
                   startRow = r, startCol = 5)

      # F: Label (side-lead-aware with version toggle)
      openxlsx::writeFormula(wb, sheet,
        x = paste0('IF(OR(H', r, '="L",H', r, '="l"),',
                   'TRIM(I', r, ')&IF($H$1=1," (","   ||   ")&TRIM(J', r, ')&IF($H$1=1,")",""),',
                   'TRIM(J', r, ')&IF($H$1=1," (","   ||   ")&TRIM(I', r, ')&IF($H$1=1,")",""))'),
        startRow = r, startCol = 6)

      # K: Value
      openxlsx::writeFormula(wb, sheet,
        x = paste0('IF(OR(H', r, '="L",H', r, '="l"),"1:2","3:4")'),
        startRow = r, startCol = 11)
      openxlsx::addStyle(wb, sheet, s_center, rows = r, cols = 11)

      # L: Syntax
      openxlsx::writeFormula(wb, sheet,
        x = paste0('E', r, '&" = recode_values("&G', r, '&", "&K', r, '&" ~ 1, default = 0)"'),
        startRow = r, startCol = 12)

      # M: Source Label
      openxlsx::writeFormula(wb, sheet,
        x = paste0('TRIM(I', r, ')&" ("&TRIM(J', r, ')&")"'),
        startRow = r, startCol = 13)

      # N: Opposite Label
      openxlsx::writeFormula(wb, sheet,
        x = paste0('IF(OR(H', r, '="R",H', r, '="r"),',
                   'TRIM(I', r, ')&IF($H$1=1," (","   ||   ")&TRIM(J', r, ')&IF($H$1=1,")",""),',
                   'TRIM(J', r, ')&IF($H$1=1," (","   ||   ")&TRIM(I', r, ')&IF($H$1=1,")",""))'),
        startRow = r, startCol = 14)

      row <- row + 1L
    }

    row <- row + 2L
  }
}


.write_profiles_sheet <- function(wb, profile_blocks, project_name) {
  sheet <- "Profiles"
  openxlsx::addWorksheet(wb, sheet)

  openxlsx::setColWidths(wb, sheet, cols = 1:10,
    widths = c(20.83, 1.83, 5.83, 5.83, 10.83, 70.83, 10, 10, 10, 70.0))

  openxlsx::freezePane(wb, sheet, firstActiveRow = 4)

  s_bold_14 <- openxlsx::createStyle(textDecoration = "Bold", fontSize = 14)
  s_bold    <- openxlsx::createStyle(textDecoration = "Bold")
  s_center  <- openxlsx::createStyle(halign = "center", valign = "center")
  s_bold_c  <- openxlsx::createStyle(textDecoration = "Bold", halign = "center", valign = "center")

  # Row 1: study name
  openxlsx::writeData(wb, sheet, x = project_name, startRow = 1, startCol = 6, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, s_bold_14, rows = 1, cols = 6)

  # Row 3: column headers
  hdrs <- c(A = "Notes", C = "Block", D = "Num", E = "Var",
            F = "Label", G = "Source Var", H = "Value", I = "Zero-filled", J = "Syntax")
  hdr_cols <- match(names(hdrs), LETTERS)
  for (i in seq_along(hdrs)) {
    openxlsx::writeData(wb, sheet, x = hdrs[i], startRow = 3, startCol = hdr_cols[i], colNames = FALSE)
  }
  openxlsx::addStyle(wb, sheet, s_bold_c, rows = 3, cols = hdr_cols, gridExpand = TRUE)

  row <- 4L
  for (block_idx in seq_along(profile_blocks)) {
    blk  <- profile_blocks[[block_idx]]
    fill <- openxlsx::createStyle(fgFill = .HEADER_FILLS[((block_idx - 1) %% length(.HEADER_FILLS)) + 1])

    all_cols <- c(1, 3:10)
    openxlsx::addStyle(wb, sheet, fill, rows = row, cols = all_cols, gridExpand = TRUE, stack = TRUE)

    # Block number: first references Polars, rest auto-increment
    if (block_idx == 1) {
      openxlsx::writeFormula(wb, sheet, x = "MAX(Polars!C4:C350)+1",
                   startRow = row, startCol = 3)
    } else {
      openxlsx::writeFormula(wb, sheet, x = paste0("MAX($C$4:$C", row - 1, ")+1"),
                   startRow = row, startCol = 3)
    }
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 3, stack = TRUE)

    # Prefix + block name
    openxlsx::writeData(wb, sheet, x = blk$prefix, startRow = row, startCol = 5, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 5, stack = TRUE)
    openxlsx::writeData(wb, sheet, x = blk$block_name, startRow = row, startCol = 6, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, s_bold, rows = row, cols = 6, stack = TRUE)

    header_row <- row
    row <- row + 1L

    # Data rows
    items <- blk$items
    for (i in seq_len(nrow(items))) {
      num <- sprintf("%02d", i)
      r   <- row

      openxlsx::writeData(wb, sheet, x = num,                startRow = r, startCol = 4, colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = items$label[i],     startRow = r, startCol = 6, colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = items$source_var[i], startRow = r, startCol = 7, colNames = FALSE)
      openxlsx::writeData(wb, sheet, x = items$value[i],     startRow = r, startCol = 8, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, s_center, rows = r, cols = 8)
      openxlsx::writeData(wb, sheet, x = 1L,                 startRow = r, startCol = 9, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, s_center, rows = r, cols = 9)

      # E: Var formula
      if (i == 1) {
        openxlsx::writeFormula(wb, sheet, x = paste0("E", header_row, "&D", r),
                     startRow = r, startCol = 5)
      } else {
        openxlsx::writeFormula(wb, sheet, x = paste0("LEFT(E", r - 1, ",LEN(E", r - 1, ")-2)&D", r),
                     startRow = r, startCol = 5)
      }

      # J: Syntax (branching formula, all zero-filled aware)
      # Split into ZF=1 / ZF=0 trees to keep nesting depth under Excel limit
      # Each tree: 1=slash+comma→mean+val, 2=slash+single→copy+val,
      #   3=blank+comma→mean, 4=blank+single→copy, 5=comparison→case_when,
      #   6=comma+val→if_any, 7=single+val→recode_values
      openxlsx::writeFormula(wb, sheet,
        x = .build_profile_syntax(r),
        startRow = r, startCol = 10)

      row <- row + 1L
    }

    row <- row + 2L
  }
}
