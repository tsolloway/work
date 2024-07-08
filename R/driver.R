#' engine_linear
#' @description engine_linear
#' @export
engine_linear <- function(
    df, dv, ivs
){

  require(broom)

  tibble("ivs" = ivs) %>%
    mutate(
      fit = ivs %>% map(~lm(glue("{dv}~{.x}"), df)),
      tidy = fit %>% map(broom::tidy),
      glance = fit %>% map(broom::glance),
      coeficient = fit %>% map(~coefficients(.x)[[2]]) %>% unlist(),
      index = coeficient %>% map(~(.x/mean(abs(coeficient)))*100) %>% unlist(),
      index_abs = index %>% abs,
      r2 = glance %>% map(~.x["r.squared"]) %>% unlist(),
      p = glance %>% map(~.x["p.value"]) %>% unlist(),
      n = glance %>% map(~.x["nobs"]) %>% unlist(),
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

  require(rms)

  dv_recode <- match.arg(dv_recode)

  if( dv_recode != "none"){
    df[["dv"]] <- df[[dv]] %>% fields_recode(dv_recode, custom_car_recode_syntax = custom_car_recode_syntax)
  }else{
    df[["dv"]] <- df[[dv]]
  }


  prob_success <- function(x, beta0, beta1) {
    (exp((x) * beta1) * exp(beta0)) /
      ( 1 + (exp((x) * beta1) * exp(beta0)) )
  }


  temp <- tibble("ivs" = ivs) %>%
    mutate(
      fit = ivs %>% map(~rms::lrm(formula(glue("df[['dv']]~df[['{.x}']]")), df)) %>% suppressWarnings(),
      se = fit %>% map(~.x[["var"]] %>% diag() %>% sqrt() %>% .[2]) %>% unlist(),

      beta0 = fit %>% map(~{
        y = .x[["coefficients"]][1]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      beta1 = fit %>% map(~{
        y = .x[["coefficients"]][2]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),


      wald_z = map2(beta1, se, ~.x/.y) %>% unlist(),

      p = fit %>% map(~{
        y = .x[["stats"]][["P"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      r2 = fit %>% map(~{
        y = .x[["stats"]][["R2"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      Dxy = fit %>% map(~{
        y = .x[["stats"]][["Dxy"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist(),

      n = fit %>% map(~{
        y = .x[["stats"]][["Obs"]]
        ifelse(is.null(y), NA, y)
      }) %>% unlist()
    )

  temp %>% mutate(
    iv_mean = df[, ivs] %>% map(mean, na.rm = TRUE) %>% unlist(),
    iv_range_low = df[, ivs] %>% map(function(x)range(x, na.rm = T) %>% head(1)) %>% unlist(),
    iv_range_hi = df[, ivs] %>% map(function(x)range(x, na.rm = T) %>% tail(1)) %>% unlist(),
    iv_shift = shift_percentage * (iv_range_hi - iv_range_low),
    prob_low = prob_success(iv_mean - 0.5 * iv_shift, beta0, beta1),
    prob_high = prob_success(iv_mean + 0.5 * iv_shift, beta0, beta1),
    prob_shift = prob_high - prob_low,
    index = prob_shift %>% map(~(.x / mean(abs(prob_shift), na.rm=T)) * 100) %>% unlist(),
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
      analysis <- engine_linear(df = df, dv = dv, ivs = ivs)
    }else{
      analysis <- subgroups %>% map(~engine_linear(
        df = df %>% filter(df[[.x]] == 1),
        dv = dv, ivs = ivs
      )) %>% set_names(subgroups)
    }


  }else if( engine == "logistic" ){

    if( is.null(subgroups) ){
      analysis <- engine_logistic(
        df = df, dv = dv, ivs = ivs, shift_percentage = shift_percentage,
        dv_recode = dv_recode, custom_car_recode_syntax = custom_car_recode_syntax
      ) %>% list("SINGLE" = .)
    }else{

      analysis <- subgroups %>% map(~engine_logistic(
        df = df %>% filter(df[[.x]] == 1),
        dv = dv, ivs = ivs, shift_percentage = shift_percentage,
        dv_recode = dv_recode, custom_car_recode_syntax = custom_car_recode_syntax
      )) %>% set_names(subgroups)
    }
  }

  analysis_table <- analysis %>% imap(~{
    .x[["fit"]] <- NULL
    names(.x) <- paste0(.y, "_", names(.x))
    .x[[gsub("_", " ", .y)]] <- .x[[paste0(.y, "_index_abs")]]
    .x <- .x %>% add_NA_rows(1)
    .x[nrow(.x), ncol(.x)] <- mean(.x[[paste0(.y, "_prob_shift")]], na.rm = TRUE)
    .x
  }) %>%
    bind_cols() %>%
    bind_cols(
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

  require(openxlsx)

  analysis <- dv %>% imap(function(dvx, dvn){
    ivs %>% imap(function(ivx, ivn){
      driver(df, dv=dvx, ivs=ivx, subgroups = subgroups, labels = labels[[ivn]], engine = engine[[dvn]], shift_percentage = shift_percentage)
    })
  })

  output <- analysis %>% imap(function(dvx, dvn){

    wb <- createWorkbook()

    for( i in names(dvx) ){

      if(engine[[dvn]] == "linear"){
        footer <- "Divers are estimated with OLS regression"
      }else if(engine[[dvn]] == "logistic"){
        footer <- glue("Divers are estimated with logistic regression and impacts are calculated with a {shift_percentage*100}% shift in predictors") %>% as.character()
      }

      wb <- append_drivers(
        analysis_table = dvx[[i]][["analysis_table"]],
        wb = wb,
        sheet_name = i,
        title = paste("Drivers of", dvn, "predicted by", i),
        footer = footer,
        label_width = label_width)
    }

    return(wb)
  })


  if( write ){
    output %>% iwalk(~{
      saveWorkbook(.x, glue("Drivers - {.y}.xlsx"), overwrite = TRUE)
    })
  }


  return(
    list(
      analysis = analysis,
      formatted_workbooks = output
    )
  )
}

