#' bn_impact_cpquery_engine
#' @description bn_impact_cpquery_engine
#' @export
bn_impact_cpquery_engine <- function(
    bn,
    df,
    iv = NULL,
    dv = NULL,
    ivs = NULL,
    n_boot = 1000,
    n_querry = 10000,
    only_monte_carlo = FALSE,
    make_tibble = TRUE,
    use_parallel = FALSE
){

  library(bnlearn)
  library(dplyr)
  library(work)
  library(glue)
  library(parallel)


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
  #  Define cpquery difference
  # ---------------------------
  cp_diff <- function(data, indices, bn, dv, iv, fit_boot = NULL, n_querry = 10000) {

    if(is.null(fit_boot)){
      dat_boot <- data[indices, ]                    # resample rows
      fit_boot <- bnlearn::bn.fit(bn, dat_boot)      # fit parameters to fixed structure
    }else{
      dat_boot <- data
    }

    dv_max <- dat_boot[[dv]] %>% as.character() %>% as.numeric() %>% max(na.rm = TRUE)

    iv_max <- dat_boot[[iv]] %>% as.character() %>% as.numeric() %>% max(na.rm = TRUE)
    iv_min <- dat_boot[[iv]] %>% as.character() %>% as.numeric() %>% min(na.rm = TRUE)

    # compute conditional probability difference
    p1 <- eval(parse(text = glue("bnlearn::cpquery(fit_boot, event = ({dv} == {dv_max}), evidence = ({iv} == {iv_max}), method = 'ls', n = {n_querry})")))
    p0 <- eval(parse(text = glue("bnlearn::cpquery(fit_boot, event = ({dv} == {dv_max}), evidence = ({iv} == {iv_min}), method = 'ls', n = {n_querry})")))

    return(p1 - p0)
  }



  if(!only_monte_carlo){
    fit <- NULL
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
          cp_diff(data = data, indices = indices, bn = bn, dv = dv, iv = iv, fit_boot = fit, n_querry = n_querry)
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
          cp_diff(data = data, indices = indices, bn = bn, dv = dv, iv = iv, fit_boot = fit, n_querry = n_querry)
        },
        R = n_boot
      )

    }

    # ---------------------------
    # Summarize results
    # ---------------------------
    estimate <- mean(boot_res$t)
    std_error <- sd(boot_res$t)
    t_stat <- estimate / std_error
    p_value <- 2 * (1 - pt(abs(t_stat), df = nrow(df) - 1))

    ci_perc <- boot::boot.ci(boot_res, type = "perc")$percent[4:5] %>% suppressWarnings()


    # ---------------------------
    #  Output
    # ---------------------------
    result <- list(
      dv = dv,
      iv = iv,
      estimate = estimate,
      std_error = std_error,
      t_statistic = t_stat,
      p_value = p_value,
      ci_lower = ci_perc[1],
      ci_upper = ci_perc[2]
    )

    # if(is.null(fit)){
    #   ci_bca <- boot::boot.ci(boot_res, type = "bca")$bca[4:5] %>% suppressWarnings()
    #
    #   result[["ci_adj_lower"]] <- ci_bca[1]
    #   result[["ci_adj_upper"]] <- ci_bca[2]
    # }


  }else if(n_boot == 1){

    result <- list(
      dv = dv,
      iv = iv,
      estimate = cp_diff(data = df, indices = seq(nrow(df)), bn = bn, dv = dv, iv = iv, fit_boot = fit, n_querry = n_querry)
    )

  }



  if(make_tibble){
    result <- result %>% tibble::as_tibble()
  }


  return(result)

}
