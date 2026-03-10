#' pca_analysis
#'
#' @description Runs principal components analysis across a range of factor
#'   counts (3 to n_vars - 2), applies the specified rotation, and returns
#'   loading tables with factor assignments for each solution.
#'
#' @param df A data frame containing the variables to analyze.
#' @param vars Character vector of variable names. If `NULL`, uses all columns.
#' @param labels A data frame with `variable` and `label` columns, a named
#'   character vector, or `NULL` for no labels.
#' @param rotation Character. Rotation method for [psych::principal()] (default:
#'   `"equamax"`).
#' @param max_factors Integer. Maximum number of factors to evaluate (default:
#'   `30`). Set to `NULL` to evaluate all possible solutions.
#' @param seed Integer. Random seed for reproducibility (default: `1`).
#' @param return_raw_analysis Logical. If `TRUE`, includes the raw
#'   [psych::principal()] output objects (default: `FALSE`).
#'
#' @return A list containing `parameters`, `pca_tables` (one loading table per
#'   factor count), and optionally `raw_analysis`.
#'
#' @export
pca_analysis <- function(
    df, vars = NULL, labels = NULL,
    rotation = c(
      "equamax", "varimax", "quartimax", "bentlerT", "varimin", "geominT", "bifactor",
      "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin",
      "none"
    ),
    max_factors = 30,
    seed = 1,
    return_raw_analysis = FALSE
){

  # rotation = "equamax"

  rotation <- match.arg(rotation)
  delete_label <- FALSE

  if(!is.null(vars)){
    df <- df %>% select(all_of(vars))
  }else if(is.null(vars)){
    vars <- df %>% names()
  }


  if(!is.null(labels)){

    if(is.character(labels)){
      labels <- labels %>% work::dictionary_from_named_object()
    }

    for(i in c("var", "vars", "variables")){
      if(i %in% tolower(names(labels))) names(labels)[which(tolower(names(labels)) %in% i)] <- "variable"
    }

    for(i in c("lab", "labs", "labels")){
      if(i %in% tolower(names(labels))) names(labels)[which(tolower(names(labels)) %in% i)] <- "label"
    }

    if(!identical(names(labels), c("variable", "label"))){
      stop(
        glue('labels object must have colnames c("variable", "label")')
      )
    }
  }else if(is.null(labels)){
    delete_label <- TRUE
    labels <- tibble(
      variable = vars,
      label = vars
    )
  }


  n_factors <- seq(length(vars)) %>% set_names()
  n_factors <- n_factors[-c(1, 2, length(vars) - 1, length(vars))]

  if(!is.null(max_factors)){
    n_factors <- n_factors[n_factors <= max_factors]
  }


  if(!is.null(seed)) set.seed(seed)
  analysis <- imap(
    n_factors,
    possibly(
      ~ {
        if(!is.null(seed)) set.seed(seed)
        psych::principal(
          df, nfactors = .x, rotate = rotation,
          missing = FALSE, use = "pairwise"
        ) %>%
          suppressWarnings()
      },
      otherwise = NA
    )
  ) %>%
    keep(~is_truthy(.x))


  pca_tables <- imap(
    analysis,
    ~ .x %>%
      loadings() %>%
      unclass() %>%
      data.frame() %>%
      setNames(
        .,
        names(.) %>% gsub("RC|PC", "", .) %>% as.numeric() %>% sprintf("F%02d", .)
      ) %>%
      select(., sort(colnames(.))) %>%
      tibble::rownames_to_column(var = "variable") %>%
      {
        f_cols <- select(., starts_with("F"))
        f_idx <- max.col(abs(f_cols))
        mutate(.,
          max = f_cols[cbind(seq_len(nrow(f_cols)), f_idx)],
          factor = f_idx,
          name = glue("Factor {factor}")
        )
      } %>%
      left_join(
        labels,
        by = join_by(variable)
      ) %>%
      select(
        name, variable, label, factor, max, starts_with("F")
      ) %>%
      arrange(factor, -abs(max)) %>%
      suppressWarnings()
  )


  if(delete_label){
    pca_tables <- imap(
      pca_tables,
      ~ .x %>% select(-label)
    )
  }


  variance_explained <- imap(
    analysis,
    ~ .x %>%
      pluck("Vaccounted") %>%
      t() %>%
      tail(1) %>%
      as_tibble() %>%
      setNames(., names(.) %>% str_scrub())
  ) %>%
    bind_rows() %>%
    mutate(
      proportion_var = cumulative_var - lag(cumulative_var),
      proportion_var = ifelse(is.na(proportion_var), cumulative_var, proportion_var),
      solution = glue("Solution {names(analysis)}")
    ) %>%
    select(solution, proportion_var, cumulative_var)


  parameters <- list(
    method = "Principal Components Analysis",
    rotation = rotation %>% stringr::str_to_title()
  )


  results <- list(
    pca_tables = pca_tables,
    variance_explained = variance_explained,
    parameters = parameters,
    meta = list(analytic = "pca")
  )


  if(return_raw_analysis) results[["raw_analysis"]] <- analysis


  return(results)
}
