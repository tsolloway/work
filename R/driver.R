#' engine_linear
#' @description engine_linear
#' @export
engine_linear <- function(
    df, dv, ivs
){

  require(broom)

  temp <- tibble("ivs" = ivs) %>%
    mutate(
      fit = ivs %>% map(possibly(~lm(glue("{dv}~{.x}"), df), otherwise = NA)),
    )

  temp %>%
    filter(!is.na(fit)) %>%
    mutate(
      tidy = fit %>% map(broom::tidy),
      glance = fit %>% map(broom::glance),
      coeficient = fit %>% map(~coefficients(.x) %>% tail(1)) %>% as.numeric(),
      index = coeficient %>% map(~(.x/mean(abs(coeficient)))*100) %>% as.numeric(),
      index_abs = index %>% abs,
      r2 = glance %>% map(~.x["r.squared"]) %>% unlist(),
      p = glance %>% map(~.x["p.value"]) %>% unlist(),
      n = glance %>% map(~.x["nobs"]) %>% unlist(),
    ) %>%
    left_join(temp, ., by = join_by(ivs, fit))
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
      fit = ivs %>% purrr::map(possibly(~rms::lrm(formula(glue("df[['dv']]~df[['{.x}']]")), df) %>% suppressWarnings() %>% suppressMessages(), otherwise = NA)),

      se = fit %>% map(possibly(~.x %>% vcov() %>% diag() %>% sqrt() %>% tail(1) )) %>% as.numeric(),

      beta0 = fit %>% map(~{
        y = .x %>% pluck("coefficients") %>% head(1)
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric(),

      beta1 = fit %>% map(~{
        y = .x %>% pluck("coefficients") %>% tail(1)
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric(),

      wald_z = map2(beta1, se, ~.x/.y) %>% as.numeric(),

      p = fit %>% map(~{
        y = .x %>% pluck("stats") %>% pluck("P")
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric(),

      r2 = fit %>% map(~{
        y = .x %>% pluck("stats") %>% pluck("R2")
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric(),

      Dxy = fit %>% map(~{
        y = .x %>% pluck("stats") %>% pluck("Dxy")
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric(),

      n = fit %>% map(~{
        y = .x %>% pluck("stats") %>% pluck("Obs")
        ifelse(is.null(y), NA, y)
      }) %>% as.numeric()

    ) %>% suppressWarnings()


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
  ) %>% suppressWarnings()

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
    labels <- labels %>% dictionary_from_named_object()
  }else{
    labels <- ivs %>% dictionary_from_named_object()
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

      analysis <- subgroups %>% map(
        ~engine_logistic(
          df = df %>% filter(df[[.x]] == 1),
          dv = dv,
          ivs = ivs,
          shift_percentage = shift_percentage,
          dv_recode = dv_recode,
          custom_car_recode_syntax = custom_car_recode_syntax
        )
      ) %>%
        set_names(subgroups)
    }
  }



  analysis_table <- analysis %>% imap(
    ~{
      .x[["fit"]] <- NULL
      names(.x) <- paste0(.y, "_", names(.x))
      .x[[gsub("_", " ", .y)]] <- .x[[paste0(.y, "_index_abs")]]
      .x <- .x %>% add_NA_rows(1)

      if(engine == "logistic"){
        .x[nrow(.x), ncol(.x)] <- mean(abs(.x[[paste0(.y, "_prob_shift")]]), na.rm = TRUE)
      }else if(engine == "linear"){
        .x[nrow(.x), ncol(.x)] <- mean(abs(.x[[paste0(.y, "_r2")]]), na.rm = TRUE)
      }

      .x
    }
  ) %>%
    bind_cols() %>%
    rename(Variable = Total_ivs) %>%
    select(-ends_with("ivs")) %>%
    left_join(
      labels,
      by = join_by(Variable == var)
    ) %>%
    relocate(Label = label, .after = Variable)


  analysis_table[nrow(analysis_table), "Variable"] <- "Total Impact"


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



#' drivers
#' @description drivers
#' @export
drivers <- function(
    df, dv, ivs, subgroups = NULL, labels = NULL, engine, shift_percentage = .05, label_width = "auto", write = TRUE
){


  if(is.null(names(dv))){
    names(dv) <- dv
  }

  if(!is.list(ivs) && is.character(ivs)){
    ivs <- list(
      "ivs" = ivs
    )
  }


  analysis <- dv %>% imap(function(dvx, dvn){

    ivs %>% imap(function(ivx, ivn){


      if(is.list(labels) && !tibble::is_tibble(labels)){
        xlabels <- labels[[ivn]] %>% dictionary_from_named_object()
      }else{
        xlabels <- labels %>% dictionary_from_named_object()
      }


      if(is.list(engine)){
        xengine <- engine[[dvn]]
      }else{
        xengine <- engine
      }


      driver(df, dv = dvx, ivs = ivx, subgroups = subgroups, labels = xlabels, engine = xengine, shift_percentage = shift_percentage)

    })
  }) %>%
    suppressWarnings()



  output <- analysis %>% imap(function(dvx, dvn){

    wb <- oxl_create_workbook()

    for( i in names(dvx) ){

      if(is.list(engine)){
        xengine <- engine[[dvn]]
      }else{
        xengine <- engine
      }


      if(xengine == "linear"){
        footer <- "Divers are estimated with OLS regression"
      }else if(xengine == "logistic"){
        footer <- glue("Divers are estimated with logistic regression and impacts are calculated with a {shift_percentage*100}% shift in predictors") %>% as.character()
      }


      wb <- append_drivers(
        analysis_table = dvx[[i]][["analysis_table"]],
        subgroups = subgroups,
        wb = wb,
        sheet_name = i,
        title = paste("Drivers of", dvn, "predicted by", i),
        footer = footer,
        label_width = label_width,
        engine = xengine
      )
    }

    return(wb)
  })


  if( write ){
    output %>% iwalk(~{
      openxlsx::saveWorkbook(.x, glue("Drivers - {.y}.xlsx"), overwrite = TRUE)
    })
  }


  return(
    list(
      analysis = analysis,
      formatted_workbooks = output
    )
  )
}
