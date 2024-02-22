#' engine_linear
#' @description engine_linear
#' @export
engine_linear <- function(
    df, dv, ivs
){
  tibble::tibble("ivs" = ivs) %>%
    dplyr::mutate(
      fit = ivs %>% purrr::map(~lm(glue::glue("{dv}~{.x}"), df)),
      tidy = fit %>% purrr::map(broom::tidy),
      glance = fit %>% purrr::map(broom::glance),
      coeficient = fit %>% purrr::map(~coefficients(.x)[[2]]) %>% unlist(),
      index = coeficient %>% purrr::map(~(.x/mean(abs(coeficient)))*100) %>% unlist(),
      index_abs = index %>% abs,
      r2 = glance %>% purrr::map(~.x["r.squared"]) %>% unlist(),
      p = glance %>% purrr::map(~.x["p.value"]) %>% unlist(),
      n = glance %>% purrr::map(~.x["nobs"]) %>% unlist(),
    )
}



#' engine_logistic
#' @description engine_logistic
#' @export
engine_logistic <- function(
    df, dv, ivs, shift_percentage = .05,
    dv_recode = c("none", "tb", "t2b", "t3b", "custom"),
    custom_car_recode_syntax = NULL
){

  dv_recode <- match.arg(dv_recode)

  if( dv_recode != "none"){
    df[["dv"]] <- df[[dv]] %>% work::fields_recode(dv_recode, custom_car_recode_syntax = custom_car_recode_syntax)
  }else{
    df[["dv"]] <- df[[dv]]
  }


  prob_success <- function(x, beta0, beta1) {
    (exp((x) * beta1) * exp(beta0)) /
      ( 1 + (exp((x) * beta1) * exp(beta0)) )
  }


  temp <- tibble::tibble("ivs" = ivs) %>%
    dplyr::mutate(
      fit = ivs %>% purrr::map(~rms::lrm(formula(glue::glue("df[['dv']]~df[['{.x}']]")), df)) %>% suppressWarnings(),
      se = fit %>% purrr::map(~.x[["var"]] %>% diag() %>% sqrt() %>% .[2]) %>% unlist(),

      beta0 = fit %>% purrr::map(~{
        y = .x[["coefficients"]][1]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      beta1 = fit %>% purrr::map(~{
        y = .x[["coefficients"]][2]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),


      wald_z = purrr::map2(beta1, se, ~.x/.y) %>% unlist(),

      p = fit %>% purrr::map(~{
        y = .x[["stats"]][["P"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      r2 = fit %>% purrr::map(~{
        y = .x[["stats"]][["R2"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      Dxy = fit %>% purrr::map(~{
        y = .x[["stats"]][["Dxy"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      n = fit %>% purrr::map(~{
        y = .x[["stats"]][["Obs"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist()
    )

  temp %>% mutate(
    iv_mean = df[, ivs] %>% purrr::map(mean, na.rm = TRUE) %>% unlist(),
    iv_range_low = df[, ivs] %>% purrr::map(function(x)range(x, na.rm = T) %>% head(1)) %>% unlist(),
    iv_range_hi = df[, ivs] %>% purrr::map(function(x)range(x, na.rm = T) %>% tail(1)) %>% unlist(),
    iv_shift = shift_percentage * (iv_range_hi - iv_range_low),
    prob_low = prob_success(iv_mean - 0.5*iv_shift, beta0, beta1),
    prob_high = prob_success(iv_mean + 0.5*iv_shift, beta0, beta1),
    prob_shift = prob_high - prob_low,
    index = prob_shift %>% purrr::map(~(.x/mean(abs(prob_shift), na.rm=T))*100) %>% unlist(),
    index_abs = index %>% abs
  )

}



#' driver
#' @description driver
#' @export
driver <- function(
    df, dv, ivs, subgroups = NULL, labels = NULL, engine = c("linear", "logistic"), shift_percentage = .05,
    dv_recode = c("none", "tb", "t2b", "t3b", "custom"),
    custom_car_recode_syntax = NULL
){

  engine <- match.arg(engine)

  if( !is.null(labels) ){
    labels <- c(labels, NA)
  }else{
    labels <- c(ivs, NA)
  }

  if( engine == "linear" ){

    if( is.null(subgroups) ){
      analysis <- work::engine_linear(df = df, dv = dv, ivs = ivs)
    }else{
      analysis <- subgroups %>% purrr::map(~work::engine_linear(
        df = df %>% dplyr::filter(df[[.x]] == 1),
        dv = dv, ivs = ivs
      )) %>% set_names(subgroups)
    }


  }else if( engine == "logistic" ){

    if( is.null(subgroups) ){
      analysis <- work::engine_logistic(
        df = df, dv = dv, ivs = ivs, shift_percentage = shift_percentage,
        dv_recode = dv_recode, custom_car_recode_syntax = custom_car_recode_syntax
      ) %>% list("SINGLE" = .)
    }else{

      analysis <- subgroups %>% purrr::map(~work::engine_logistic(
        df = df %>% dplyr::filter(df[[.x]] == 1),
        dv = dv, ivs = ivs, shift_percentage = shift_percentage,
        dv_recode = dv_recode, custom_car_recode_syntax = custom_car_recode_syntax
      )) %>% set_names(subgroups)
    }
  }

  analysis_table <- analysis %>% purrr::imap(~{
    .x[["fit"]] <- NULL
    names(.x) <- paste0(.y, "_", names(.x))
    .x[[gsub("_", " ", .y)]] <- .x[[paste0(.y, "_index_abs")]]
    .x <- .x %>% work::add_NA_rows(1)
    .x[nrow(.x), ncol(.x)] <- mean(.x[[paste0(.y, "_prob_shift")]], na.rm = TRUE)
    .x
  }) %>%
    dplyr::bind_cols() %>%
    dplyr::bind_cols(
      Variables = .[[1]],
      Labels = labels,
      .
    )

  analysis_table[nrow(analysis_table), "Variables"] <- "Total Impact"

  if( is.null(subgroups) ){
    analysis <- analysis[["SINGLE"]]
    names(analysis_table) <- names(analysis_table) %>% gsub("SINGLE_", "", ., fixed = TRUE)
  }

  output <- list(
    analysis = analysis,
    analysis_table = analysis_table
  )

  return(output)
}


#' driver
#' @description driver
#' @export
drivers <- function(
    df, dv, ivs, subgroups = NULL, labels = NULL, engine, shift_percentage = .05, label_width = "auto", write = TRUE
){

  analysis <- dv %>% purrr::imap(function(dvx, dvn){
    ivs %>% purrr::map(function(ivx){
      work::driver(df, dv=dvx, ivs=ivx, subgroups = subgroups, labels = labels, engine = engine[[dvn]], shift_percentage = shift_percentage)
    })
  })

  output <- analysis %>% purrr::imap(function(dvx, dvn){

    wb <- openxlsx::createWorkbook()

    for( i in names(dvx) ){

      if(engine[[dvn]] == "linear"){
        footer <- "Divers are estimated with OLS regression"
      }else if(engine[[dvn]] == "logistic"){
        footer <- glue::glue("Divers are estimated with logistic regression and impacts are calculated with a {shift_percentage*100}% shift in predictors") %>% as.character()
      }

      wb <- work::append_drivers(
        dvx[[i]][["analysis_table"]],
        wb = wb,
        sheet_name = i ,
        title = paste("Drivers of", dvn, "predicted by", i),
        footer = footer,
        label_width = label_width)
    }

    return(wb)
  })


  if( write ){
    output %>% purrr::iwalk(~{
      openxlsx::saveWorkbook(.x, glue::glue("Drivers - {.y}.xlsx"), overwrite = TRUE)
    })
  }


  return(
    list(
      analysis = analysis,
      formatted_workbooks = output
    )
  )
}

