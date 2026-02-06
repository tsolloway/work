#' Sample rows by group with quota and partial replacement
#'
#' Samples rows from a data frame by group, ensuring each group reaches
#' a target quota. If a group has fewer rows than its quota, all available
#' rows are included once, and only the shortfall is sampled with replacement.
#'
#' `group_col` can be provided as an unquoted column name (recommended in pipes)
#' or as a single character string containing the column name.
#'
#' @param df A data frame.
#' @param group_col Grouping column (unquoted) or a single string (e.g., "survey_class").
#' @param quota_vec Either:
#'   \itemize{
#'     \item A single positive integer quota applied to all groups, OR
#'     \item A named numeric/integer vector giving quotas per group (names must match group values as characters).
#'   }
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return A data frame sampled to meet group quotas.
#'
#' @examples
#' df <- iris
#' df$group <- iris$Species
#'
#' # Same quota for all groups
#' out1 <- sample_by_group_quota(df, group, quota_vec = 60, seed = 123)
#'
#' # group_col as a string
#' out2 <- sample_by_group_quota(df, "group", quota_vec = 60, seed = 123)
#'
#' # Per-group quotas
#' quota_vec <- c(setosa = 80, versicolor = 40, virginica = 20)
#' out3 <- sample_by_group_quota(df, group, quota_vec, seed = 123)
#'
#' @export
sample_by_group_quota <- function(df, group_col, quota_vec, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # allow group_col to be unquoted OR a single string
  group_sym <- if (is.character(group_col) && length(group_col) == 1) {
    rlang::sym(group_col)
  } else {
    rlang::ensym(group_col)
  }

  # create a stable, character group key for matching quotas
  df2 <- df %>%
    dplyr::mutate(.group_internal = as.character(!!group_sym))

  # expand a single quota to all groups
  if (length(quota_vec) == 1 && is.numeric(quota_vec)) {
    if (!is.finite(quota_vec) || quota_vec < 0) stop("quota_vec must be a non-negative number.")
    grp_vals <- unique(df2$.group_internal)
    quota_vec <- stats::setNames(rep(as.integer(quota_vec), length(grp_vals)), grp_vals)
  } else {
    if (is.null(names(quota_vec))) stop("quota_vec must be named when providing per-group quotas.")
  }

  out <- df2 %>%
    dplyr::group_by(.group_internal) %>%
    dplyr::group_modify(~ {
      # .y contains the current group key(s)
      g <- as.character(.y$.group_internal[[1]])
      quota <- quota_vec[[g]]

      if (is.null(quota)) stop("Missing quota for group: ", g)

      quota <- as.integer(quota)
      if (is.na(quota) || quota < 0) stop("Invalid quota for group: ", g)

      n_available <- nrow(.x)

      if (n_available >= quota) {
        .x %>% dplyr::slice_sample(n = quota, replace = FALSE)
      } else {
        shortfall <- quota - n_available
        dplyr::bind_rows(
          .x,
          .x %>% dplyr::slice_sample(n = shortfall, replace = TRUE)
        )
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.group_internal)

  out
}
