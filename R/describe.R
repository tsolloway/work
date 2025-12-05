#' describe
#' @description Provides descriptive statistics, missing counts, completeness, and optional histogram objects for each numeric variable in a data frame.
#' @param df A data frame or tibble.
#' @param vars Optional vector of variable names to describe. Defaults to all columns.
#' @return A tibble with descriptive statistics, missing counts, completeness, and histogram objects (for numeric variables).
#' @examples
#' describe(iris)
#' describe(iris, vars = c("Sepal.Length", "Petal.Length"))
#' @export
describe <- function(df, vars = NULL) {

  # Select variables if specified
  if(!is.null(vars)){
    vars <- vars %>% unlist() %>% as.character()
    df <- df %>% dplyr::select(dplyr::all_of(vars))
  }

  # Identify numeric columns for stats and histograms
  numeric_cols <- df %>% dplyr::select(where(is.numeric)) %>% colnames()

  # Base description for numeric columns
  if(length(numeric_cols) > 0){
    desc <- psych::describe(df %>% dplyr::select(dplyr::all_of(numeric_cols))) %>%
      dplyr::select(-dplyr::any_of("vars"))
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble()

    # Add missing counts and completeness
    desc <- desc %>%
      dplyr::mutate(
        missing = purrr::map_int(variable, ~sum(is.na(df[[.x]]))),
        complete = n / nrow(df),
        hc_histogram = purrr::map(variable, function(col_name) {
          hc_histogram(
            df[[col_name]],
            title = glue::glue("Histogram of {stringr::str_to_title(col_name)}")
          )
        })
      ) %>%
      dplyr::relocate(missing, .after = n) %>%
      dplyr::relocate(complete, .after = missing)

  } else {
    stop("No numeric columns to describe")
}

  return(desc)
  }
