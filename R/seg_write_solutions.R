#' seg_write_solutions
#' @description seg_write_solutions
#' @export
seg_write_solutions <- function(
    seg, solution = NULL, where = NULL,
    strategy = c("multisession", "multicore", "sequential", 'cluster'),
    workers = availableCores(omit = 1),
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


  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["solution"]]
  }

  if(is.null(where) || is.na(where)){
    where <- getwd()
  }


  solution_summary_table <- seg[["solutions"]][["summary_table"]]


  if(!is.null(solution)){
    solution_summary_table <- solution_summary_table %>% filter(solution_name == solution)
  }


  solution_summary_table <- solution_summary_table %>%
    mutate(
      location = glue("{where}/{solution_name}")
    )


  solution_vars <- solution_summary_table %>%
    select(lda_name) %>%
    unlist() %>%
    setNames(NULL)

  solution_locations <- solution_summary_table %>%
    select(location) %>%
    unlist() %>%
    setNames(NULL)


  walk(
    solution_locations %>% unique(),
    ~dir.create(.x, showWarnings = FALSE)
  )


  seg_write_shell_parallel(
    seg = seg,
    solution_var = solution_vars,
    where = solution_locations,
    strategy = strategy,
    workers = workers,
    add_key = add_key,
    truncate = truncate,
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

}

