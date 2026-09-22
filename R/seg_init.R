#' seg_init
#'
#' @description Initializes a new segmentation project. Creates the standard
#'   folder structure, generates a blank spec template, and returns a seg object
#'   with paths and metadata ready for downstream pipeline functions.
#'
#' @param folder_path Character. Root directory for the project (default:
#'   current working directory). Three subfolders are created beneath it:
#'   `1. Data and Syntax`, `2. Specs, Input and FA`, `3. Solutions`.
#' @param project_name Character. Project name used in file naming and metadata.
#' @param project_number Character. Project number used in file naming and
#'   metadata.
#' @param force Logical. If `TRUE`, DELETES the three project subfolders and
#'   everything in them before recreating them - specs, syntax and solutions
#'   included - and warns with a file count first. If `FALSE` (default) and the
#'   folders already exist, attaches to them without touching their contents, so
#'   a project can be re-entered safely. An existing spec at the template path
#'   is never overwritten in either case of attaching.
#'
#' @return A seg object (list with class `"analytic_segmentation"`) containing
#'   `paths`, `meta`, and `data` slots, with a blank spec template saved to the
#'   process folder.
#'
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

  # ATTACH: folders already there and force = FALSE. Reuse them untouched so a
  # project can be re-entered without losing specs, syntax or solutions. The
  # blank spec template is only written when no spec exists (see below), so
  # attaching can never clobber real work.
  attaching <- folders_exist && !force

  if (attaching) {
    message(
      "Project folders already exist - attaching to them. ",
      "Nothing was deleted or overwritten. Use force = TRUE to rebuild from scratch."
    )
  }

  if (folders_exist && force) {

    existing_files <- list.files(
      c(folder_data_path, folder_process_path, folder_solution_path),
      recursive = TRUE, all.files = TRUE, no.. = TRUE
    )

    if (length(existing_files) > 0) {
      warning(
        "force = TRUE is deleting ", length(existing_files),
        " existing file(s) under the three project folders, including any specs, ",
        "syntax and solutions.",
        call. = FALSE, immediate. = TRUE
      )
    }

    if( dir.exists(folder_data_path) ) unlink(folder_data_path, recursive = TRUE)
    if( dir.exists(folder_process_path) ) unlink(folder_process_path, recursive = TRUE)
    if( dir.exists(folder_solution_path) ) unlink(folder_solution_path, recursive = TRUE)
  }

  dir.create(folder_data_path,     showWarnings = FALSE)
  dir.create(folder_process_path,  showWarnings = FALSE)
  dir.create(folder_solution_path, showWarnings = FALSE)

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
      "pca" = NA
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
        prefix      = "DEM",
        name        = "Demographics",
        label       = blank_labels,
        source_var  = blank_vars,
        value       = blank_values,
        # placeholders, not mean rows - the blank value here will be typed over
        zero_filled = 1
      )
    } else {
      seg_create_profile_block(
        prefix      = "XYZ",
        name        = "BlockName",
        label       = blank_labels,
        source_var  = blank_vars,
        value       = blank_values,
        zero_filled = 1
      )
    }
  })

  spec_target <- file.path(folder_process_path, paste0(spec_project_name, " - Specs.xlsx"))

  if (file.exists(spec_target)) {

    # Never overwrite a spec that is already there. This is what makes
    # attaching safe: the template is a convenience for a NEW project, not
    # something that should reset an existing one.
    message("Existing spec found - keeping it: ", basename(spec_target))
    spec_path <- spec_target

  } else {

    spec_path <- seg_generate_spec(
      project_name   = spec_project_name,
      polar_blocks   = template_polars,
      profile_blocks = template_profiles,
      output_path    = spec_target
    )
  }

  result[["paths"]][["files"]][["spec"]] <- spec_path

  return(result)
}
