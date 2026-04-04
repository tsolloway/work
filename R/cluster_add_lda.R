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
    priors = c("size", "equal"),
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

  if (is.character(priors)) priors <- match.arg(priors, c("size", "equal"))
  # numeric priors pass through as-is (already normalised by caller)


  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  id <- df[[resp_id_name]]


  if(is.null(id)) stop("id is NULL in 'cluster_add_lda'")


  if(!is.null(filter_name)){
    df_temp <- df %>% dplyr::filter(.data[[filter_name]])
  }else if(is.null(filter_name)){
    df_temp <- df
  }



  lda_score <- function(x, df){
    seed_classes <- x$lev
    pred <- x %>% predict(df)
    predicted_classes <- levels(pred$class)

    if (!setequal(seed_classes, predicted_classes)) {
      missing <- setdiff(seed_classes, predicted_classes)
      warning(
        "LDA predicted classes do not match seed classes. ",
        "Seed had: ", paste(seed_classes, collapse = ", "),
        ". Predicted: ", paste(predicted_classes, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Returning NA for this solution.",
        call. = FALSE
      )
      return(NA)
    }

    x <- dplyr::bind_cols(
      purrr::pluck(pred, "posterior"),
      purrr::pluck(pred, "class")
    ) %>%
      suppressMessages() %>%
      setNames(., glue::glue("seg_{seq(ncol(.))}"))

    names(x)[ncol(x)] <- "seg"

    return(x)
  }



  if(use_reduced){

    result <- cluster_table %>% dplyr::mutate(
      "lda_name" = glue::glue("LDA_opt_{cluster_name}")
    )
  }else if(!use_reduced){

    result <- cluster_table %>% dplyr::mutate(
      "lda_name" = glue::glue("LDA_{cluster_name}")
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
      dplyr::mutate(
        "lda_name" = glue::glue("LDA_{cluster_name}"),
        "lda_inputs" = list(lda_vars),
        "lda_profiles" = list(lda_vars_profiles)
      )

  }



  if(all(priors == "equal")){

    temp_list <- rlang::quo(
      list(
        cluster_seed,
        priors_equal,
        cluster_name,
        lda_inputs)
    )

  }else if(all(priors == "size")){

    temp_list <- rlang::quo(
      list(
        cluster_seed,
        priors_size,
        cluster_name,
        lda_inputs
      )
    )

  }else{

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
    dplyr::mutate(
      "lda_fit" = purrr::pmap(
        {{temp_list}},
        purrr::possibly(
          function(x, y, z, w){
            set.seed(1)

            # x=result[["cluster_seed"]][[1]]
            # y=result[["priors_size"]][[1]]
            # z=result[["cluster_name"]][[1]]
            # w=result[["lda_inputs"]][[1]]

            dx <- dplyr::left_join(
              x,
              df_temp %>% dplyr::select(dplyr::all_of(c(!!resp_id_name, unlist(w)))),
              by = dplyr::join_by("id" == !!resp_id_name)
            )

            dx <- dx %>% dplyr::filter( !is.na(dx[[{z}]]) )

            dg <- dx %>% dplyr::select(dplyr::all_of(z)) %>% unlist() %>% setNames(NULL)

            dx <- dx %>% dplyr::select(!dplyr::any_of(c("id", resp_id_name, z)))

            dp <- y / sum(y)

            .collinear <- FALSE
            fit <- withCallingHandlers(
              MASS::lda(x = dx, grouping = dg, prior = dp),
              warning = function(w) {
                if (grepl("collinear", conditionMessage(w))) {
                  .collinear <<- TRUE
                  invokeRestart("muffleWarning")
                }
              }
            )
            attr(fit, "collinear") <- .collinear
            fit
          },
          otherwise = NA)
        )
    )


  result <- result %>%
    dplyr::mutate(
      "lda_coefficient_function" = purrr::pmap(
        list(
          lda_fit,
          lda_inputs,
          cluster_seed,
          cluster_name
        ), purrr::possibly(
          function(x,y,z,w){
            coefficient_lda(
              fit = x,
              input = df_temp %>%
                dplyr::filter(.data[[resp_id_name]] %in% z[["id"]]) %>%
                dplyr::select(dplyr::all_of(unlist(y))),
              grp = z[[w]]
            )
          },
          otherwise = NA)
      ),

      "lda_predict" = purrr::map2(
        lda_fit, lda_inputs, purrr::possibly(
          ~dplyr::bind_cols(
            dplyr::select(df, seg_uuid),
            lda_score(.x, df %>% dplyr::select(dplyr::all_of(unlist(.y))))
          ),
          otherwise = NA)
      ),

      "lda_seg" = purrr::map2(
        lda_predict, lda_name,
        purrr::possibly(
          ~purrr::pluck(.x, "seg") %>%
            as.character() %>%
            as.numeric() %>%
            dplyr::bind_cols(id, .) %>%
            rlang::set_names(c("id", .y)) %>%
            suppressMessages(),
          otherwise = NA)
      )
    )



  if(is.null(filter_name)){

    result <- result %>%
      dplyr::mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          purrr::possibly(
            function(x,y,z,w){

              temp <- dplyr::left_join(
                x,
                y,
                by = dplyr::join_by(id)
              )

              caret::confusionMatrix(
                as.factor(temp[[w]]),
                as.factor(temp[[z]])
              ) %>%
                suppressWarnings()

            },otherwise = NA))
      )

  }else if(!is.null(filter_name)){

    result <- result %>%
      dplyr::mutate(
        "confusion" = purrr::pmap(
          list(cluster_seed, lda_seg, cluster_name, lda_name),
          purrr::possibly(
            function(x,y,z,w){

              # y <- y %>% dplyr::filter(y[["id"]] %in% x[["id"]])
              #
              # caret::confusionMatrix(
              #   as.factor(y %>% purrr::pluck(w) %>% .[df[[filter_name]]]),
              #   as.factor(x %>% purrr::pluck(z))
              # ) %>%
              #   suppressWarnings()

              temp <- dplyr::left_join(
                x,
                y[df[[filter_name]], ],
                by = dplyr::join_by(id)
              )

              caret::confusionMatrix(
                as.factor(temp[[w]]),
                as.factor(temp[[z]])
              ) %>%
                suppressWarnings()
            },
            otherwise = NA))
      )
  }



  result <- result %>%
    dplyr::mutate(
      "accuracy" = purrr::map(
        confusion,
        purrr::possibly(
          ~.x %>%
            purrr::pluck("overall") %>%
            purrr::pluck("Accuracy") %>%
            round(10),
          otherwise = NA)) %>% unlist(),

      "kappa" = purrr::map(
        confusion,
        purrr::possibly(
          ~.x %>%
            purrr::pluck("overall") %>%
            purrr::pluck("Kappa") %>%
            round(10),
          otherwise = NA)) %>% unlist(),

      "cv" = purrr::pmap(
        {{temp_list}},
        purrr::possibly(
          function(x, y, z, w){
            set.seed(1)

            dx <- dplyr::left_join(
              x,
              df_temp %>% dplyr::select(dplyr::all_of(c(!!resp_id_name, unlist(w)))),
              by = dplyr::join_by("id" == !!resp_id_name)
            )

            dx <- dx %>% dplyr::filter(!is.na(dx[[{z}]]))
            dg <- dx %>% dplyr::select(dplyr::all_of(z)) %>% unlist() %>% setNames(NULL)
            dx <- dx %>% dplyr::select(!dplyr::any_of(c("id", resp_id_name, z)))
            dp <- y / sum(y)

            cv_result <- suppressWarnings(MASS::lda(x = dx, grouping = dg, prior = dp, CV = TRUE))
            round(mean(cv_result$class == dg), 10)
          },
          otherwise = NA)) %>% unlist(),

      "split_half" = purrr::pmap(
        {{temp_list}},
        purrr::possibly(
          function(x, y, z, w){
            set.seed(1)

            dx <- dplyr::left_join(
              x,
              df_temp %>% dplyr::select(dplyr::all_of(c(!!resp_id_name, unlist(w)))),
              by = dplyr::join_by("id" == !!resp_id_name)
            )

            dx <- dx %>% dplyr::filter(!is.na(dx[[{z}]]))
            dg <- dx %>% dplyr::select(dplyr::all_of(z)) %>% unlist() %>% setNames(NULL)
            dx_vars <- dx %>% dplyr::select(!dplyr::any_of(c("id", resp_id_name, z)))
            dp <- y / sum(y)

            n_obs <- nrow(dx_vars)
            idx <- sample(n_obs)
            half <- floor(n_obs / 2)
            idx_a <- idx[seq_len(half)]
            idx_b <- idx[(half + 1):n_obs]

            lda_a <- suppressWarnings(MASS::lda(x = dx_vars[idx_a, ], grouping = dg[idx_a], prior = dp))
            lda_b <- suppressWarnings(MASS::lda(x = dx_vars[idx_b, ], grouping = dg[idx_b], prior = dp))

            pred_a <- predict(lda_a, dx_vars)$class
            pred_b <- predict(lda_b, dx_vars)$class

            round(mean(pred_a == pred_b), 10)
          },
          otherwise = NA)) %>% unlist(),

      "collinear" = purrr::map(
        lda_fit,
        purrr::possibly(
          ~isTRUE(attr(.x, "collinear")),
          otherwise = NA)) %>% unlist(),

      "n_segments" = purrr::map(
        lda_predict,
        purrr::possibly(
          ~sort(unique(as.numeric(as.character(.x$seg)))),
          otherwise = NA)),

      "df_solution" = purrr::map2(
        cluster_seed, lda_seg,
        purrr::possibly(
          ~dplyr::full_join(
            .x, .y, by = dplyr::join_by("id")
          ),
          otherwise = NA)),
    )


  return(result)
}
