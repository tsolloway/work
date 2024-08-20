#' seg_write_shell_parallel
#' @description seg_write_shell_parallel
#' @export
seg_write_shell_parallel <- function(
    seg,
    solution_vars = NULL,
    where = rep(NA, length(solution_vars)),
    strategy = c("multisession", "multicore", "sequential", 'cluster'),
    workers = NULL,
    add_key = TRUE,
    label_width = 75,
    hide_pvalue = FALSE,
    truncate = c("no", "yes", "both"),
    truncate_polar_threshold = .15,
    truncate_profile_threshold = .1,
    version = c("traditional", "both"),
    do_seg_bw = TRUE,
    do_italic = TRUE,
    switched_polars = FALSE,
    setting_polar_threshold = .2,
    setting_profile_threshold = .15,
    setting_tolerance = .05,
    setting_pvalue = .1,
    setting_diff = .1,
    setting_type = c("diff", "pvalue"),
    setting_color = c("bw", "color"),
    verbose = FALSE
){

  require(furrr)

  strategy <- match.arg(strategy)

  truncate <- match.arg(truncate)
  version <- match.arg(version)
  setting_type <- match.arg(setting_type)
  setting_color <- match.arg(setting_color)


  if(truncate == "no"){
    truncate <- FALSE
    truncate_both <- FALSE
  }else if(truncate == "yes"){
    truncate <- TRUE
    truncate_both <- FALSE
  }else if(truncate == "both"){
    truncate <- FALSE
    truncate_both <- TRUE
  }



  if(strategy == "multicore" && !supportsMulticore()){
    strategy <- "multisession"
  }



  if(is.null(workers)){

    workers <- availableCores(omit = 1)

    ntasks <- solution_vars %>% length()

    if(truncate_both){
      ntasks <- ntasks * 2
    }

    if(workers > ntasks){
      workers <- ntasks
    }
  }


  if(workers > availableCores(omit = 1)){
    workers <- availableCores(omit = 1)
  }


  strategy <- switch(
    strategy,
    sequential = future::sequential,
    multisession = future::multisession,
    multicore = future::multicore,
    cluster = future::cluster
  )


  plan(strategy = strategy, workers = workers)


  opts <- furrr_options(
    globals = TRUE,
    packages = c("dplyr", "openxlsx", "psych", "tibble", "tidyr", "purrr", "glue"),
    seed = TRUE
  )


  if(!truncate_both){

    things_to_do <- list(
      x = solution_vars,
      y = where,
      z = rep(truncate, length(solution_vars))
    )

  }else if(truncate_both){

    things_to_do <- list(
      x = rep(solution_vars, 2),
      y = rep(where, 2),
      z = c(
        rep(truncate, length(solution_vars)),
        rep(!truncate, length(solution_vars))
      )
    )
  }


  future_pwalk(
    things_to_do,
    function(x,y,z){
      tryCatch({
        seg_write_shell(
          seg = seg,
          solution_var = x,
          where = y,
          add_key = add_key,
          truncate = z,
          truncate_polar_threshold = truncate_polar_threshold,
          truncate_profile_threshold = truncate_profile_threshold,
          version = version,
          do_seg_bw = do_seg_bw,
          do_italic = do_italic,
          label_width = label_width,
          hide_pvalue = hide_pvalue,
          switched_polars = switched_polars,
          setting_polar_threshold = setting_polar_threshold,
          setting_profile_threshold = setting_profile_threshold,
          setting_tolerance = setting_tolerance,
          setting_pvalue = setting_pvalue,
          setting_diff = setting_diff,
          setting_type = setting_type,
          setting_color = setting_color,
          verbose = verbose
        )
      },
      error = function(e) NA
      )
    },
    .options = opts
  )


  future:::ClusterRegistry("stop")

}












