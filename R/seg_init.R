#' seg_init
#' @description seg_init
#' @export
seg_init <- function(
    folder_path = getwd()
){

  result <- list()
  class(result) <- append(class(result), "analytic_segmentation")

  folder_path <- folder_path %>% normalizePath()
  folder_name <- folder_path %>% basename()

  folder_data_path <- glue::glue("{folder_path}/1. Data and Syntax") %>% normalizePath() %>% suppressWarnings()
  folder_process_path <- glue::glue("{folder_path}/2. Specs, Input and FA") %>% normalizePath() %>% suppressWarnings()
  folder_solution_path <- glue::glue("{folder_path}/3. Solutions") %>% normalizePath() %>% suppressWarnings()


  if( dir.exists(folder_data_path) ) unlink(folder_data_path, recursive = TRUE)
  if( dir.exists(folder_process_path) ) unlink(folder_process_path, recursive = TRUE)
  if( dir.exists(folder_solution_path) ) unlink(folder_solution_path, recursive = TRUE)

  dir.create(folder_data_path)
  dir.create(folder_process_path)
  dir.create(folder_solution_path)

  # copy tool templates to folder here

  result[["paths"]] <- list(
    "folders" = list(
      "parent" = folder_path,
      "data" = folder_data_path,
      "process" = folder_process_path,
      "solution" = folder_solution_path
    ),
    "files" = list(
      "data" = NA,
      "spec" = NA,
      "shell" = NA,
      "input" = NA,
      "FA" = NA
    )
  )

  result[["meta"]] <- list(
    "analytic" = "segmentation",
    "folder_name" = folder_name
  )

  return(result)
}
