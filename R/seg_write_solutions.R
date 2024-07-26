#' seg_write_solutions
#' @description seg_write_solutions
#' @export
seg_write_solutions <- function(seg, solution = NULL, where = NULL){

  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["solution"]]
  }

  if(is.null(where) || is.na(where)){
    where <- getwd()
  }


  solution_summary_table <- seg[["solutions"]][["summary_table"]]


  if(!is.null(solution)){
    solution_summary_table %>% filter(solution_name == solution)
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


  require(furrr)

  plan(multisession, workers = 8)

  opts <- furrr_options(
    globals = TRUE,
    packages = c("dplyr", "openxlsx", "psych", "tibble", "tidyr", "purrr", "glue"),
    seed = TRUE
  )

  future_walk2(
    solution_vars,
    solution_locations,
    ~seg_write_shell(seg = seg, solution_var = .x, where = .y),
    .options = opts
  )

}

