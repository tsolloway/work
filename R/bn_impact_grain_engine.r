#' bn_impact_grain_engine
#' @description bn_impact_grain_engine
#' @export
bn_impact_grain_engine <- function(
    bn,
    df,
    iv = NULL,
    dv = NULL,
    ivs = NULL,
    n_boot = 1,
    return_dv_estimate = FALSE,
    make_tibble = TRUE,
    use_parallel = FALSE
){

  library(work)
  library(bnlearn)
  library(gRbase)
  library(gRain)
  work::start()


  if("meta" %in% names(bn)){
    if(bn[["meta"]] == "bn_model_single"){
      dv <- bn[["summary"]][["model"]][["dv"]]
      ivs <- bn[["arcs"]][["ivs"]] %>% select(from, to) %>% unlist() %>% unique() %>% sort()
      fit <- bn[["fit"]]
      bn <- bn[["bn"]]
    }
  }


  df <- df %>%
    select(all_of(c(dv, ivs))) %>%
    as.data.frame()


  # ---------------------------
  #  Define grain difference
  # ---------------------------
  gr_diff <- function(data, indices, bn, dv, iv, fit_boot = NULL) {

    if(is.null(fit_boot)){

      dat_boot <- data[indices, ]                    # resample rows
      fit_boot <- bnlearn::bn.fit(bn, dat_boot)      # fit parameters to fixed structure
      grain_bn <- bnlearn::as.grain(fit_boot) %>% gRbase::compile()

    }else{

      dat_boot <- data
      grain_bn <- bnlearn::as.grain(fit_boot) %>% gRbase::compile()
    }


    evidence_max <- dat_boot %>%
      summarise_at(iv, ~as.character(.) %>% as.numeric() %>% max(na.rm = TRUE) %>% as.character()) %>%
      as.list()

    evidence_min <- dat_boot %>%
      summarise_at(iv, ~as.character(.) %>% as.numeric() %>% min(na.rm = TRUE) %>% as.character()) %>%
      as.list()


    # compute conditional probability difference
    p1 <- gRain::querygrain(grain_bn, nodes = dv, evidence =  evidence_max, simplify = TRUE)[[2]]
    p0 <- gRain::querygrain(grain_bn, nodes = dv, evidence =  evidence_min, simplify = TRUE)[[2]]


    return(
      c(
        "yes" = p1,
        "no" = p0,
        "diff" = p1 - p0
      )
    )

    # return(p1 - p0)

  }



  if(n_boot > 1){

    # ---------------------------
    # Run bootstrap in parallel
    # ---------------------------

    if(use_parallel){

      set.seed(1)
      boot_res <- boot::boot(
        data = df,
        statistic = function(data, indices){
          gr_diff(data = data, indices = indices, bn = bn, dv = dv, iv = iv, fit_boot = NULL)
        },
        R = n_boot,
        parallel = "multicore",       # use "snow" if on Windows
        ncpus = parallel::detectCores() - 1     # leave one core free
      )

    }else if(!use_parallel){

      set.seed(1)
      boot_res <- boot::boot(
        data = df,
        statistic = function(data, indices){
          gr_diff(data = data, indices = indices, bn = bn, dv = dv, iv = iv, fit_boot = NULL)
        },
        R = n_boot
      )

    }

    # ---------------------------
    # Summarize results
    # ---------------------------


    estimate <- mean(boot_res$t[, 3])
    std_error <- sd(boot_res$t[, 3])
    t_stat <- estimate / std_error
    p_value <- 2 * (1 - pt(abs(t_stat), df = nrow(df) - 1))

    ci_perc <- boot::boot.ci(boot_res, index = 3, type = "perc")$percent[4:5] %>% suppressWarnings()


    # ---------------------------
    #  Output
    # ---------------------------
    result <- list(
      dv = dv,
      iv = paste0(iv, collapse = "+"),
      estimate = estimate,
      std_error = std_error,
      t_statistic = t_stat,
      p_value = p_value,
      ci_lower = ci_perc[1],
      ci_upper = ci_perc[2]
    )


    if(return_dv_estimate){
      result[["dv_estimate"]] <- mean(boot_res$t[, 1])
    }



  }else if(n_boot == 1){

    x_estimate <- gr_diff(data = df, indices = seq(nrow(df)), bn = bn, dv = dv, iv = iv, fit_boot = fit)


    result <- list(
      dv = dv,
      iv = paste0(iv, collapse = "+"),
      estimate = x_estimate[["diff"]]
    )


    if(return_dv_estimate){
      result[["dv_estimate"]] <- x_estimate[["yes"]]
    }

  }




  if(make_tibble){
    result <- result %>% tibble::as_tibble()

    if(return_dv_estimate){
      result <- result %>%
        relocate(dv_estimate, .before = estimate)
    }

  }


  return(result)

}
