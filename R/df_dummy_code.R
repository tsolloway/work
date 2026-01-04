#' Dummy-code predictors (underscore naming) with optional reference-level dropping
#'
#' Builds a dummy-coded data.frame in a deterministic, instruction-matching column order:
#' \itemize{
#'   \item \code{respondent_id} (if provided) is copied through unchanged and never dummy-coded.
#'   \item Numeric / integer / logical predictors are copied as numeric.
#'   \item Character predictors are treated as factors for stable level handling.
#'   \item Factor predictors are dummy-coded into 0/1 columns named \code{var_level} (underscore),
#'         except when \code{skip_binary_factors = TRUE} and the predictor has exactly 2 levels,
#'         in which case the original predictor is kept as a single factor column (not dummy-coded).
#' }
#'
#' Reference-level dropping:
#' \itemize{
#'   \item If \code{drop_reference_level = TRUE}, then for each dummy-coded factor with K levels, the function
#'     drops exactly one level (producing K-1 columns) using \code{reference_level} strategy:
#'     \code{"first"}, \code{"last"}, or \code{"most_frequent"} (default).
#'   \item If \code{drop_reference_level = FALSE}, the function keeps all MECE dummies and returns
#'     \code{recommended_reference_drops}, indicating which single dummy per factor would be dropped
#'     under each strategy.
#' }
#'
#' Within-class variance dropping:
#' If \code{y} is provided, the function checks each resulting predictor column (including dummy columns)
#' for zero within-class variance and drops columns that are constant within at least one class
#' (among classes with \code{n >= min_n_per_class}). The \code{respondent_id} column is never dropped
#' by this check.
#'
#' Hard-coded base-R instructions:
#' The returned \code{instructions_base_r} is a character vector of explicit base-R statements
#' (e.g., \code{ifelse()} assignments) that recreate the same transformation on new data in the same
#' column order, then subsets to \code{kept_cols}. The instructions begin with:
#' \preformatted{
#' if (!exists("df_new")) df_new <- <training_data_name>
#' }
#' so you can immediately run them to reproduce the training transform, or define \code{df_new}
#' first to score new data.
#'
#' Evaluating the instructions (base R):
#' \preformatted{
#' foo <- df_dummy_code(iris)
#' # Option A: use the default fallback (df_new <- iris) and run:
#' eval(parse(text = foo$instructions_base_r))
#'
#' # Option B: score new data (define df_new first):
#' df_new <- iris[1:10, ]
#' eval(parse(text = foo$instructions_base_r))
#' # Result is df_dummy_coded_final in your environment:
#' head(df_dummy_coded_final)
#'
#' # Option C (recommended): evaluate in a clean environment:
#' env <- new.env(parent = emptyenv())
#' env$df_new <- iris[1:10, ]
#' eval(parse(text = foo$instructions_base_r), envir = env)
#' head(env$df_dummy_coded_final)
#' }
#'
#' @param df A data.frame of predictors (may include numeric, logical, factor, character).
#' @param y Optional. Class labels. May be factor/character/numeric; coerced to factor.
#'   If \code{NULL}, within-class variance checks are skipped and no columns are dropped for that reason.
#' @param respondent_id Optional. A single character string naming the respondent id column in \code{df}.
#'   This column is copied through unchanged and not dummy-coded.
#' @param skip_binary_factors Logical. If \code{TRUE}, factor/character predictors with exactly 2 levels
#'   are NOT dummy-coded; they are kept as a single factor column. Default \code{TRUE}.
#' @param dummy_as_factors Logical. If \code{TRUE}, created dummy columns are returned as factors with
#'   levels \code{"0"} and \code{"1"}. If \code{FALSE}, they are returned as integers 0/1. Default \code{TRUE}.
#' @param drop_reference_level Logical. If \code{TRUE}, drop one dummy per dummy-coded factor (K-1 coding).
#'   Default \code{TRUE} (LDA-safe).
#' @param reference_level Character, one of \code{"first"}, \code{"last"}, \code{"most_frequent"}.
#'   Which level to drop when \code{drop_reference_level = TRUE}. Default \code{"most_frequent"}.
#' @param min_n_per_class Integer >= 2. Within-class variance is only assessed for classes
#'   with at least this many rows.
#' @param na_action Character, one of \code{"fail"} or \code{"omit"}.
#'   If \code{"fail"}, stops if any NA is present in \code{df} (or \code{y}, if provided).
#'   If \code{"omit"}, removes rows with any NA in \code{df} (and \code{y}, if provided).
#'
#' @return A list with:
#' \describe{
#'   \item{df_dummy_coded}{A data.frame after dummy coding (and optional dropping). Column order matches instructions.}
#'   \item{kept_cols}{Character vector of column names kept in \code{df_dummy_coded}.}
#'   \item{dropped_cols}{Character vector of predictor column names dropped due to (a) reference-level dropping and/or
#'     (b) zero within-class variance.}
#'   \item{small_classes}{Character vector of class levels with fewer than \code{min_n_per_class} rows (NULL if \code{y} is \code{NULL}).}
#'   \item{recommended_reference_drops}{If \code{drop_reference_level = FALSE}, a list with elements
#'     \code{$first}, \code{$last}, \code{$most_frequent}, each a character vector of dummy-column names recommended
#'     to drop (one per factor). Otherwise \code{NULL}.}
#'   \item{instructions_base_r}{A character vector of base-R code lines to reproduce the transformation.}
#' }
#'
#' @examples
#' foo <- df_dummy_code(iris)
#' # Run instructions on training data (fallback df_new <- iris):
#' eval(parse(text = foo$instructions_base_r))
#' head(df_dummy_coded_final)
#'
#' # Score new data:
#' df_new <- iris[1:10, ]
#' eval(parse(text = foo$instructions_base_r))
#' head(df_dummy_coded_final)
#'
#' @export
df_dummy_code <- function(
    df,
    y = NULL,
    respondent_id = NULL,
    skip_binary_factors = TRUE,
    dummy_as_factors = TRUE,
    drop_reference_level = TRUE,
    reference_level = c("first", "last", "most_frequent"),
    min_n_per_class = 2L,
    na_action = c("fail", "omit")
) {
  na_action <- match.arg(na_action)
  reference_level <- match.arg(reference_level)

  training_df_name <- deparse(substitute(df))

  if (!is.data.frame(df)) stop("`df` must be a data.frame.")

  if (!isTRUE(is.logical(skip_binary_factors)) || length(skip_binary_factors) != 1L) {
    stop("`skip_binary_factors` must be a single logical (TRUE/FALSE).")
  }
  if (!isTRUE(is.logical(dummy_as_factors)) || length(dummy_as_factors) != 1L) {
    stop("`dummy_as_factors` must be a single logical (TRUE/FALSE).")
  }
  if (!isTRUE(is.logical(drop_reference_level)) || length(drop_reference_level) != 1L) {
    stop("`drop_reference_level` must be a single logical (TRUE/FALSE).")
  }

  if (!isTRUE(length(min_n_per_class) == 1L) ||
      !isTRUE(is.numeric(min_n_per_class)) ||
      is.na(min_n_per_class) ||
      min_n_per_class < 2L) {
    stop("`min_n_per_class` must be a single integer-like value >= 2.")
  }
  min_n_per_class <- as.integer(min_n_per_class)

  if (!is.null(respondent_id)) {
    if (!is.character(respondent_id) || length(respondent_id) != 1L) {
      stop("`respondent_id` must be a single character string naming a column in `df`.")
    }
    if (!respondent_id %in% names(df)) {
      stop("`respondent_id` must be a column name in `df`.")
    }
  }

  if (!is.null(y) && length(y) != nrow(df)) {
    stop("`y` must have length equal to nrow(df) when provided.")
  }
  y_fac <- if (is.null(y)) NULL else as.factor(y)

  # NA handling
  if (identical(na_action, "fail")) {
    if (anyNA(df)) stop("`df` contains NA. Use na_action = \"omit\" or remove NA beforehand.")
    if (!is.null(y_fac) && anyNA(y_fac)) stop("`y` contains NA. Use na_action = \"omit\" or remove NA beforehand.")
  } else {
    keep_rows <- if (is.null(y_fac)) {
      stats::complete.cases(df)
    } else {
      stats::complete.cases(df, y_fac)
    }
    df <- df[keep_rows, , drop = FALSE]
    if (!is.null(y_fac)) y_fac <- y_fac[keep_rows]
  }

  # Copy respondent_id through, but exclude from dummy coding
  id_vec <- NULL
  if (!is.null(respondent_id)) id_vec <- df[[respondent_id]]

  # df_mm: predictors to code (exclude respondent_id); coerce character -> factor
  df_mm <- df
  if (!is.null(respondent_id)) {
    df_mm <- df_mm[, setdiff(names(df_mm), respondent_id), drop = FALSE]
  }
  chr_idx <- vapply(df_mm, is.character, logical(1))
  if (any(chr_idx)) df_mm[chr_idx] <- lapply(df_mm[chr_idx], as.factor)

  # Build reference-drop mapping for factor vars that WILL be dummy-coded
  ref_drop_by_var <- list(first = list(), last = list(), most_frequent = list())
  factor_vars_to_dummy <- character(0)

  for (v in names(df_mm)) {
    x <- df_mm[[v]]
    if (!is.factor(x)) next

    lvls <- levels(x)

    # If binary factor and skipping binary, there will be no dummy columns
    if (isTRUE(skip_binary_factors) && length(lvls) == 2L) next

    factor_vars_to_dummy <- c(factor_vars_to_dummy, v)

    ref_drop_by_var$first[[v]] <- lvls[[1]]
    ref_drop_by_var$last[[v]] <- lvls[[length(lvls)]]

    # most frequent: observed frequency (ties broken by level order)
    counts <- table(as.character(df[[v]]))
    counts <- counts[lvls]
    mf <- names(counts)[which.max(as.integer(counts))]
    ref_drop_by_var$most_frequent[[v]] <- mf
  }

  # Recommended drops if keeping all MECE dummies
  recommended_reference_drops <- NULL
  if (!isTRUE(drop_reference_level)) {
    rec_first <- character(0)
    rec_last <- character(0)
    rec_mf <- character(0)

    for (v in factor_vars_to_dummy) {
      rec_first <- c(rec_first, paste0(v, "_", ref_drop_by_var$first[[v]]))
      rec_last  <- c(rec_last,  paste0(v, "_", ref_drop_by_var$last[[v]]))
      rec_mf    <- c(rec_mf,    paste0(v, "_", ref_drop_by_var$most_frequent[[v]]))
    }

    recommended_reference_drops <- list(
      first = rec_first,
      last = rec_last,
      most_frequent = rec_mf
    )
  }

  # Build dummy-coded output in deterministic order (matches instructions)
  out_list <- list()
  dropped_cols_ref <- character(0)

  if (!is.null(respondent_id)) out_list[[respondent_id]] <- id_vec

  for (v in names(df_mm)) {
    x <- df_mm[[v]]

    if (is.factor(x)) {
      lvls <- levels(x)

      # Binary factor: keep as single factor column (no dummy expansion)
      if (isTRUE(skip_binary_factors) && length(lvls) == 2L) {
        out_list[[v]] <- as.factor(as.character(df[[v]]))
        next
      }

      drop_lv <- NULL
      if (isTRUE(drop_reference_level)) {
        drop_lv <- ref_drop_by_var[[reference_level]][[v]]
        if (is.null(drop_lv)) stop("Reference drop level could not be determined for variable: ", v)
      }

      for (lv in lvls) {
        nm <- paste0(v, "_", lv)

        # drop reference dummy if enabled
        if (!is.null(drop_lv) && identical(lv, drop_lv)) {
          dropped_cols_ref <- c(dropped_cols_ref, nm)
          next
        }

        vals <- ifelse(as.character(df[[v]]) == lv, 1L, 0L)
        out_list[[nm]] <- if (isTRUE(dummy_as_factors)) as.factor(vals) else as.integer(vals)
      }

    } else {
      out_list[[v]] <- as.numeric(df[[v]])
    }
  }

  df_dummy_coded <- as.data.frame(out_list, check.names = FALSE)

  # Optionally drop within-class-constant columns (never consider respondent_id)
  dropped_cols_var <- character(0)
  small_classes <- NULL

  if (!is.null(y_fac)) {
    cls_counts <- table(y_fac)
    small_classes <- names(cls_counts)[cls_counts < min_n_per_class]

    cand_cols <- setdiff(names(df_dummy_coded), respondent_id)
    keep_col <- rep(TRUE, length(cand_cols))
    names(keep_col) <- cand_cols
    lvls_y <- levels(y_fac)

    for (col in cand_cols) {
      ok <- TRUE
      for (lv in lvls_y) {
        idx <- which(y_fac == lv)
        if (length(idx) < min_n_per_class) next

        col_vec <- df_dummy_coded[[col]]
        if (is.factor(col_vec)) col_vec <- as.integer(as.character(col_vec))
        vj <- stats::var(col_vec[idx])

        if (is.na(vj) || vj <= 0) {
          ok <- FALSE
          break
        }
      }
      if (!ok) keep_col[[col]] <- FALSE
    }

    dropped_cols_var <- names(keep_col)[!keep_col]

    keep_names <- c(
      if (!is.null(respondent_id)) respondent_id else character(0),
      names(keep_col)[keep_col]
    )
    df_dummy_coded <- df_dummy_coded[, keep_names, drop = FALSE]
  }

  kept_cols <- names(df_dummy_coded)

  # Hard-coded instructions (explicit ifelse assignments), aligned to creation order
  instructions_base_r <- .build_hard_coded_instructions(
    df_mm = df_mm,
    kept_cols = kept_cols,
    na_action = na_action,
    respondent_id = respondent_id,
    skip_binary_factors = skip_binary_factors,
    dummy_as_factors = dummy_as_factors,
    drop_reference_level = drop_reference_level,
    reference_level = reference_level,
    ref_drop_by_var = ref_drop_by_var,
    training_df_name = training_df_name
  )

  list(
    df_dummy_coded = df_dummy_coded,
    kept_cols = kept_cols,
    dropped_cols = unique(c(dropped_cols_ref, dropped_cols_var)),
    small_classes = small_classes,
    recommended_reference_drops = recommended_reference_drops,
    instructions_base_r = instructions_base_r
  )
}

.build_hard_coded_instructions <- function(
    df_mm,
    kept_cols,
    na_action,
    respondent_id = NULL,
    skip_binary_factors = TRUE,
    dummy_as_factors = TRUE,
    drop_reference_level = TRUE,
    reference_level = c("first", "last", "most_frequent"),
    ref_drop_by_var = NULL,
    training_df_name
) {
  reference_level <- match.arg(reference_level)

  if (isTRUE(drop_reference_level)) {
    if (is.null(ref_drop_by_var) || is.null(ref_drop_by_var[[reference_level]])) {
      stop("Internal error: reference level mapping missing for instructions.")
    }
  }

  # ASCII-only header with a fallback df_new assignment
  lines <- c(
    "# Base R hard-coded recipe to recreate the same dummy coding:",
    "# If `df_new` does not exist, default to the training data used to build this recipe:",
    sprintf("if (!exists(\"df_new\")) df_new <- %s", training_df_name)
  )

  if (identical(na_action, "omit")) {
    lines <- c(lines, "df_new <- df_new[stats::complete.cases(df_new), , drop = FALSE]")
  } else {
    lines <- c(lines, "if (anyNA(df_new)) stop(\"`df_new` contains NA. Remove NA or use na_action = \\\"omit\\\".\")")
  }

  lines <- c(lines, "df_dummy_coded <- list()")

  if (!is.null(respondent_id)) {
    lines <- c(
      lines,
      sprintf("df_dummy_coded[[\"%s\"]] <- df_new[[\"%s\"]]", respondent_id, respondent_id)
    )
  }

  for (v in names(df_mm)) {
    x <- df_mm[[v]]

    if (is.factor(x)) {
      lvls <- levels(x)

      # Binary factor: keep single column (no dummy expansion)
      if (isTRUE(skip_binary_factors) && length(lvls) == 2L) {
        lines <- c(
          lines,
          sprintf("df_dummy_coded[[\"%s\"]] <- as.factor(as.character(df_new[[\"%s\"]]))", v, v)
        )
        next
      }

      drop_lv <- NULL
      if (isTRUE(drop_reference_level)) {
        drop_lv <- ref_drop_by_var[[reference_level]][[v]]
        if (is.null(drop_lv)) stop("Internal error: missing reference mapping for variable: ", v)
      }

      for (lv in lvls) {
        nm <- paste0(v, "_", lv)

        # skip reference dummy if enabled
        if (!is.null(drop_lv) && identical(lv, drop_lv)) next

        if (isTRUE(dummy_as_factors)) {
          lines <- c(
            lines,
            sprintf(
              "df_dummy_coded[[\"%s\"]] <- as.factor(ifelse(as.character(df_new[[\"%s\"]]) == \"%s\", 1, 0))",
              nm, v, lv
            )
          )
        } else {
          lines <- c(
            lines,
            sprintf(
              "df_dummy_coded[[\"%s\"]] <- ifelse(as.character(df_new[[\"%s\"]]) == \"%s\", 1, 0)",
              nm, v, lv
            )
          )
        }
      }

    } else {
      lines <- c(lines, sprintf("df_dummy_coded[[\"%s\"]] <- as.numeric(df_new[[\"%s\"]])", v, v))
    }
  }

  kept_vec <- paste0("c(", paste(sprintf("\"%s\"", kept_cols), collapse = ", "), ")")

  lines <- c(
    lines,
    "df_dummy_coded <- as.data.frame(df_dummy_coded, check.names = FALSE)",
    sprintf("kept_cols <- %s", kept_vec),
    "df_dummy_coded_final <- df_dummy_coded[, kept_cols, drop = FALSE]",
    "# `df_dummy_coded_final` matches the function's returned `df_dummy_coded`."
  )

  lines
}
