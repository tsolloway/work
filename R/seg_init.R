#' seg_init
#' @description seg_init
#' @export
seg_init <- function(
    folder_path = getwd(),
    project_name = NULL,
    project_number = NULL,
    force = FALSE
){

  result <- list()
  class(result) <- append(class(result), "analytic_segmentation")

  folder_path <- folder_path %>% normalizePath()
  folder_name <- folder_path %>% basename()

  folder_data_path <- glue::glue("{folder_path}/1. Data and Syntax") %>% normalizePath() %>% suppressWarnings()
  folder_process_path <- glue::glue("{folder_path}/2. Specs, Input and FA") %>% normalizePath() %>% suppressWarnings()
  folder_solution_path <- glue::glue("{folder_path}/3. Solutions") %>% normalizePath() %>% suppressWarnings()


  folders_exist <- any(dir.exists(c(folder_data_path, folder_process_path, folder_solution_path)))

  if (folders_exist && !force) {
    stop("Project folders already exist. Use force = TRUE to overwrite.", call. = FALSE)
  }

  if (folders_exist && force) {
    if( dir.exists(folder_data_path) ) unlink(folder_data_path, recursive = TRUE)
    if( dir.exists(folder_process_path) ) unlink(folder_process_path, recursive = TRUE)
    if( dir.exists(folder_solution_path) ) unlink(folder_solution_path, recursive = TRUE)
  }

  dir.create(folder_data_path)
  dir.create(folder_process_path)
  dir.create(folder_solution_path)

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
      "input" = NA,
      "fa" = NA
    )
  )

  result[["meta"]] <- list(
    "analytic" = "segmentation",
    "folder_name" = folder_name,
    "project_name" = project_name %>% stringr::str_squish(),
    "project_number" = project_number %>% stringr::str_squish()
  )


  result[["data"]] <- list(
    "original" = NA,
    "with_shell" = NA,
    "with_solutions" = NA
  )


  # generate template spec
  spec_project_name <- glue::glue("{result[['meta']][['project_name']]} ({result[['meta']][['project_number']]})")

  blank_labels <- rep("", 20)
  blank_vars   <- rep("", 20)
  blank_values <- rep("", 20)

  template_polars <- lapply(seq_len(6), function(i) {
    seg_create_polar_block(
      prefix = "XYZ",
      name   = "BlockName",
      source = "",
      left   = blank_labels,
      right  = blank_labels
    )
  })

  template_profiles <- lapply(seq_len(20), function(i) {
    if (i == 20) {
      seg_create_profile_block(
        prefix     = "DEM",
        name       = "Demographics",
        label      = blank_labels,
        source_var = blank_vars,
        value      = blank_values
      )
    } else {
      seg_create_profile_block(
        prefix     = "XYZ",
        name       = "BlockName",
        label      = blank_labels,
        source_var = blank_vars,
        value      = blank_values
      )
    }
  })

  spec_path <- seg_generate_spec(
    project_name   = spec_project_name,
    polar_blocks   = template_polars,
    profile_blocks = template_profiles,
    output_path    = file.path(folder_process_path, paste0(spec_project_name, " - Specs.xlsx"))
  )
  result[["paths"]][["files"]][["spec"]] <- spec_path

  return(result)
}
