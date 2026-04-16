#' get_brand_freq
#'
#' @description Tabulate brand frequency counts across subgroups. Returns a
#'   tibble with one row per subgroup (plus a "Total" row), one column per
#'   unique brand value, and counts in the cells.
#'
#' @param df Data frame containing the brand column and any subgroup indicator
#'   columns (0/1).
#' @param brand_col Character. Name of the column in \code{df} holding brand
#'   labels.
#' @param subgroups Character vector or NULL. Names of 0/1 indicator columns
#'   defining subgroups. Default NULL (Total only).
#'
#' @return A tibble with columns \code{subgroup} (first column, character) and
#'   one integer column per brand value. The first row is always "Total".
#'
#' @examples
#' \dontrun{
#' # Total only (no subgroups)
#' get_brand_freq(df_impute, brand_col = "brand_name")
#' # # A tibble: 1 x 4
#' #   subgroup ChatGPT Gemini Perplexity
#' #   <chr>      <int>  <int>      <int>
#' # 1 Total        820    310        500
#'
#' # With subgroups (0/1 indicator columns in df)
#' get_brand_freq(
#'   df_impute,
#'   brand_col = "brand_name",
#'   subgroups = c("Regular_Google_User", "Power_User")
#' )
#' # # A tibble: 3 x 4
#' #   subgroup            ChatGPT Gemini Perplexity
#' #   <chr>                 <int>  <int>      <int>
#' # 1 Total                   820    310        500
#' # 2 Regular_Google_User     340    150        213
#' # 3 Power_User              210     95        120
#' }
#'
#' @export
get_brand_freq <- function(df, brand_col, subgroups = NULL) {

  df <- as.data.frame(df)

  if (!brand_col %in% names(df)) {
    cli::cli_abort("Column {.val {brand_col}} not found in {.arg df}.")
  }

  brand_values <- sort(unique(as.character(df[[brand_col]])))

  # Build one row for a given filter (logical vector) and label
  compute_row <- function(filter_vec, label) {
    sub_df <- df[filter_vec, , drop = FALSE]
    counts <- table(factor(as.character(sub_df[[brand_col]]), levels = brand_values))
    out <- as.list(as.integer(counts)) %>%
      rlang::set_names(brand_values)
    tibble::tibble(subgroup = label, !!!out)
  }

  # Always include Total
  rows <- list(compute_row(rep(TRUE, nrow(df)), "Total"))

  # Subgroups if provided
  if (!is.null(subgroups)) {
    missing_cols <- setdiff(subgroups, names(df))
    if (length(missing_cols) > 0) {
      cli::cli_abort("Subgroup column{?s} not found in {.arg df}: {.val {missing_cols}}.")
    }

    for (sg in subgroups) {
      rows <- c(rows, list(compute_row(df[[sg]] == 1, sg)))
    }
  }

  dplyr::bind_rows(rows)
}
