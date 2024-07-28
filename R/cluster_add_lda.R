#' cluster_add_lda
#' @description cluster_add_lda
#' @export
cluster_add_lda <- function(
    cluster_table, df, lda_vars = NULL, lda_vars_profiles = NULL, id_name,
    filter_name = NULL, priors = c("equal", "size"), use_reduced = FALSE
){

  priors <- match.arg(priors)

  id <- df[[id_name]]


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]]) #%>% dplyr::select(all_of(vars))
  }else if(is.null(filter_name)){
    df_temp <- df #%>% dplyr::select(all_of(vars))
  }


  lda_score <- function(x, df){
    x <- x %>% predict(df)

    x <- bind_cols(
      pluck(x, "posterior"),
      pluck(x, "class")
    ) %>%
      suppressMessages() %>%
      setNames(., glue("seg_{seq(ncol(.))}"))

    names(x)[ncol(x)] <- "seg"

    return(x)
  }


  lda_typing_tool_objects <- function(fit){

    result <- list()

    gpm <- fit %>% pluck("means")
    gpm_center <- gpm %>% colMeans() #equals colSums(prior * gpm)

    prior <- fit %>% pluck("prior")
    svd <- fit %>% pluck("svd")

    scaling <- fit %>% pluck("scaling")

    dm <- scale(gpm, center = gpm_center, scale = FALSE) %*% scaling

    dist_const <- 0.5 * rowSums(dm^2) - log(prior)

    dm <- dm %>% t() %>% data.frame() %>% setNames(glue("seg_{seq(nrow(dm))}"))

    result[["gpm_center"]] <- gpm_center
    result[["scaling"]] <- scaling
    result[["dm"]] <- dm
    result[["dist_const"]] <- dist_const
    result[["svd"]] <- svd

    return(result)
  }



  if(use_reduced){

    result <- cluster_table %>% mutate(
      "lda_name" = glue("LDA_opt_{cluster_name}")
    )
  }else if(!use_reduced){

    result <- cluster_table %>% mutate(
      "lda_name" = glue("LDA_{cluster_name}")
    )
  }



  if(is.null(lda_vars)){

    if(use_reduced){

      result[["lda_inputs"]] <- result[["reduced_inputs"]]
      result[["lda_profiles"]] <- result[["reduced_profiles"]]

    }else if(!use_reduced){

      result[["lda_inputs"]] <- result[["inputs"]]
      result[["lda_profiles"]] <- result[["profiles"]]

    }

  }else if(!is.null(lda_vars)){

    result <- cluster_table %>%
      mutate(
        "lda_name" = glue("LDA_{cluster_name}"),
        "lda_inputs" = list(lda_vars),
        "lda_profiles" = list(lda_vars_profiles)
      )

  }



  if(priors == "equal"){

    result <- result %>%
      mutate(
        "lda_fit" = purrr::pmap(
          list(cluster_seed, priors_equal, cluster_name, lda_inputs),
          possibly(
            function(x,y,z,w) MASS::lda(df_temp %>% dplyr::select(all_of(unlist(w))), x[[z]], prior = y),
            otherwise = NA))
      )

  }else if(priors == "size"){

    result <- result %>%
      mutate(
        "lda_fit" = purrr::pmap(
          list(cluster_seed, priors_size, cluster_name, lda_inputs),
          possibly(
            function(x,y,z,w)MASS::lda(df_temp %>% dplyr::select(all_of(unlist(w))), x[[z]], prior = y),
            otherwise = NA))
      )
  }



  result <- result %>%
    mutate(

      "lda_typing_tool_objects" = purrr::map(
        lda_fit, possibly(
          ~lda_typing_tool_objects(.x),
          otherwise = NA)
      ),

      "lda_predict" = purrr::map2(
        lda_fit, lda_inputs, possibly(
          ~lda_score(.x, df %>% dplyr::select(all_of(unlist(.y)))),
          otherwise = NA)),

      "lda_seg" = purrr::map2(
        lda_predict, lda_name,
        possibly(
          ~pluck(.x, "seg") %>% as.character() %>% as.numeric() %>%
            bind_cols(id, .) %>% set_names(c("id", .y)) %>%
            suppressMessages(),
          otherwise = NA))
    )



  if(is.null(filter_name)){

    result <- result %>%
      mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          possibly(
            function(x,y,z,w)caret::confusionMatrix(
              as.factor(y %>% pluck(w)),
              as.factor(x %>% pluck(z))
            ), otherwise = NA))
      )

  }else if(!is.null(filter_name)){

    result <- result %>%
      mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          possibly(
            function(x,y,z,w)caret::confusionMatrix(
              as.factor(y %>% pluck(w) %>% .[df[[filter_name]]]),
              as.factor(x %>% pluck(z))
            ), otherwise = NA))
      )
  }



  result <- result %>%
    mutate(
      "accuracy" = purrr::map(
        confusion,
        possibly(
          ~.x %>%
            pluck("overall") %>%
            pluck("Accuracy") %>%
            round(10),
          otherwise = NA)) %>% unlist(),

      "df_append" = purrr::map2(
        cluster_seed, lda_seg,
        possibly(
          ~full_join(
            .x, .y, by = join_by("id")
          ),
          otherwise = NA)),
    )


  return(result)
}

