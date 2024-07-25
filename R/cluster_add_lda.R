#' cluster_add_lda
#' @description cluster_add_lda
#' @export
cluster_add_lda <- function(cluster_table, df, vars, id_name, filter_name = NULL, priors = c("equal", "size")){

  priors <- match.arg(priors)

  id <- df[[id_name]]


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]]) %>% dplyr::select(all_of(vars))
  }else if(is.null(filter_name)){
    df_temp <- df %>% dplyr::select(all_of(vars))
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



  result <- cluster_table %>%
    mutate(
      "lda_name" = glue::glue("LDA_{cluster_name}")
    )


  if(priors == "equal"){

    result <- result %>%
      mutate(
        "lda_fit" = pmap(
          list(cluster_seed, priors_equal, cluster_name),
          possibly(
            function(x,y,z)MASS::lda(df_temp, x %>% pluck(z), prior = y),
            otherwise = NA))
      )

  }else if(priors == "size"){

    result <- result %>%
      mutate(
        "lda_fit" = pmap(
          list(cluster_seed, priors_size, cluster_name),
          possibly(
            function(x,y,z)MASS::lda(df_temp, x %>% pluck(z), prior = y),
            otherwise = NA))
      )
  }



  result <- result %>%
    mutate(
      "lda_predict" = map(
        lda_fit, possibly(
          ~lda_score(.x, df %>% dplyr::select(all_of(vars))),
          otherwise = NA)),

      "lda_seg" = map2(
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
        "confusion" = pmap(
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
        "confusion" = pmap(
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
      "accuracy" = map(
        confusion,
        possibly(
          ~.x %>%
            pluck("overall") %>%
            pluck("Accuracy") %>%
            round(10),
          otherwise = NA)) %>% unlist(),

      "df_append" = map2(
        cluster_seed, lda_seg,
        possibly(
          ~full_join(
            .x, .y, by = join_by("id")
          ),
          otherwise = NA)),
    )


  return(result)
}

