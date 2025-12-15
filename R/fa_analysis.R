#' fa_analysis
#' @description fa_analysis
#' @export
fa_analysis <- function(
    df, vars = NULL, labels = NULL, method = "pa",
    rotation = c(
      "equamax", "varimax", "quartimax", "bentlerT", "varimin", "geominT", "bifactor",
      "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin",
      "none"
    ),
    seed = 1,
    return_raw_analysis = FALSE
){

  # method = "pa"
  # rotation = "equamax"
  # require(psych)

  rotation <- match.arg(rotation)
  detele_label <- FALSE

  absmax <- function(x) { x[which.max( abs(x) )]}

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
    detele_label <- TRUE
    labels <- tibble(
      variable = vars,
      label = vars
    )
  }


  n_factors <- seq(length(vars)) %>% set_names()
  n_factors <- n_factors[-c(1, 2, length(vars) - 1, length(vars))]


  if(!is.null(seed)) set.seed(seed)
  analysis <- imap(
    n_factors,
    possibly(
      ~ {
        if(!is.null(seed)) set.seed(seed)
        psych::principal(
          df, nfactors = .x, method = method, rotate = rotation
        ) %>%
          suppressWarnings()
      },
      otherwise = NA
    )
  ) %>%
    keep(~is_truthy(.x))


  fa_tables <- imap(
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
      add_rownames(var = "variable") %>%
      rowwise() %>%
      mutate(
        max = absmax(c_across(starts_with("F"))),
        factor = which.max(abs(c_across(starts_with("F")))),
        name = glue("Factor {factor}")
      ) %>%
      ungroup() %>%
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


  if(detele_label){
    fa_tables <- imap(
      fa_tables,
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
    method = ifelse(method == "pa", "Principle Axis", method),
    rotation = rotation %>% stringr::str_to_title()
  )


  results <- list(
    fa_tables = fa_tables,
    variance_explained = variance_explained,
    parameters = parameters,
    meta = list(analytic = "fa")
  )


  if(return_raw_analysis) results[["raw_analysis"]] <- analysis


  return(results)
}

