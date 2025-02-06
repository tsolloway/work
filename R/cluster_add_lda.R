#' cluster_add_lda
#' @description cluster_add_lda
#' @export
cluster_add_lda <- function(
    cluster_table,
    df,
    lda_vars = NULL,
    lda_vars_profiles = NULL,
    resp_id_name = NULL,
    filter_name = NULL,
    priors = c("equal", "size"),
    use_reduced = FALSE
){

  # cluster_table = results
  # df = df
  # lda_vars = NULL
  # lda_vars_profiles = NULL
  # resp_id_name = resp_id_name
  # filter_name = filter_name
  # priors = priors
  # use_reduced = FALSE


  set.seed(1)

  priors <- match.arg(priors)


  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  id <- df[[resp_id_name]]


  if(is.null(id)) stop("id is NULL in 'cluster_add_lda'")


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]])
  }else if(is.null(filter_name)){
    df_temp <- df
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

    temp_list <- rlang::quo(
      list(
        cluster_seed,
        priors_equal,
        cluster_name,
        lda_inputs)
    )

  }else if(priors == "size"){


    temp_list <- rlang::quo(
      list(
        cluster_seed,
        priors_size,
        cluster_name,
        lda_inputs
      )
    )

  }


  result <- result %>%
    mutate(
      "lda_fit" = purrr::pmap(
        {{temp_list}},
        possibly(
          function(x, y, z, w){
            set.seed(1)

            dx <- dplyr::left_join(
              x,
              df_temp %>% dplyr::select(all_of(c(!!resp_id_name, unlist(w)))),
              by = dplyr::join_by("id" == !!resp_id_name)
            )

            dx <- dx %>% dplyr::filter( !is.na(dx[[{z}]]) )

            dg <- dx %>% dplyr::select(dplyr::all_of(z)) %>% unlist()

            dx <- dx %>% dplyr::select(!dplyr::any_of(c("id", resp_id_name, z)))

            dp <- y / sum(y)

            MASS::lda(
              x = dx,
              grouping = dg,
              prior = dp
            )
          },
          otherwise = NA))
    )


  result$cluster_seed

  result <- result %>%
    mutate(
      "lda_coefficient_function" = purrr::pmap(
        list(
          lda_fit,
          lda_inputs,
          cluster_seed,
          cluster_name
        ), possibly(
          function(x,y,z,w){
            coefficient_lda(
              fit = x,
              input = df_temp %>%
                dplyr::filter(.data[[resp_id_name]] %in% z[["id"]]) %>%
                dplyr::select(all_of(unlist(y))),
              grp = z[[w]]
            )
          },
          otherwise = NA)
      ),

      "lda_predict" = purrr::map2(
        lda_fit, lda_inputs, possibly(
          ~lda_score(
            .x,
            df %>%
              dplyr::select(all_of(unlist(.y)))),
          otherwise = NA)
      ),

      "lda_seg" = purrr::map2(
        lda_predict, lda_name,
        possibly(
          ~pluck(.x, "seg") %>%
            as.character() %>%
            as.numeric() %>%
            bind_cols(id, .) %>%
            set_names(c("id", .y)) %>%
            suppressMessages(),
          otherwise = NA)
      )
    )



  if(is.null(filter_name)){

    result <- result %>%
      mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          possibly(
            function(x,y,z,w){

              y <- y %>% filter(y[["id"]] %in% x[["id"]])

              caret::confusionMatrix(
                as.factor(y %>% pluck(w)),
                as.factor(x %>% pluck(z))
              ) %>%
                suppressWarnings()

            },otherwise = NA))
      )

  }else if(!is.null(filter_name)){

    result <- result %>%
      mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          possibly(
            function(x,y,z,w){

              y <- y %>% filter(y[["id"]] %in% x[["id"]])

              caret::confusionMatrix(
                as.factor(y %>% pluck(w) %>% .[df[[filter_name]]]),
                as.factor(x %>% pluck(z))
              ) %>%
                suppressWarnings()
            },
            otherwise = NA))
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

