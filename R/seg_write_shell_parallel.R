#' seg_write_shell_parallel
#'
#' @description Writes solution shell workbooks in parallel. Trims the seg
#'   object to only the components needed by [seg_write_shell()] before
#'   dispatching to workers — drops redundant data copies and clustering
#'   analysis results to stay under the `future.globals.maxSize` limit.
#'
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



  ntasks <- solution_vars %>% length()

  if(truncate_both){
    ntasks <- ntasks * 2
  }


  # trim seg for workers — drop redundant data copies and heavy

  # analysis results to stay under future.globals.maxSize
  seg_worker <- seg

  # keep only the data frame seg_write_shell actually uses
  if (!is.null(seg_worker[["data"]][["with_solutions"]])) {
    seg_worker[["data"]][["original"]] <- NULL
    seg_worker[["data"]][["with_shell"]] <- NULL
  }

  # drop clustering analysis objects (only summary_table is needed)
  seg_worker[["solutions"]][["analysis"]] <- NULL


  future_plan(
    strategy = strategy, workers = workers, ntasks = ntasks
  )

  on.exit(future::plan(future::sequential), add = TRUE)


  # restructure from parallel vectors into named task list for imap_progress
  task_list <- purrr::pmap(things_to_do, function(x, y, z) {
    list(solution_var = x, where = y, truncate = z)
  })
  names(task_list) <- things_to_do$x


  # clean environment worker — prevents future from serializing
  # the dev namespace (~1 GB) that load_all() closures carry
  worker_fn <- function(.x, .y) {
    task <- .x
    tryCatch({
      seg_write_shell(
        seg = seg_worker,
        solution_var = task$solution_var,
        where = task$where,
        add_key = add_key,
        truncate = task$truncate,
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
    error = function(e) warning(glue::glue("seg_write_shell failed for {task$solution_var}: {e$message}"))
    )
  }

  environment(worker_fn) <- list2env(
    list(
      seg_worker = seg_worker,
      add_key = add_key,
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
    ),
    parent = globalenv()
  )


  imap_progress(
    task_list,
    worker_fn,
    .parallel = TRUE,
    .furrr_packages = c("work", "dplyr", "openxlsx", "psych", "tibble", "tidyr", "purrr", "glue"),
    .furrr_globals = list(),
    .label = "Writing"
  )

}
