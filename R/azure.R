# ----------------------------------------
# Azure Helper Suite with Progress and Full Docs
# ----------------------------------------

#' azure_store_endpoint
#'
#' @description Create an Azure Blob Storage endpoint object. This function reads your Azure Blob
#' endpoint and access key, returning a fully-authenticated `AzureStor::storage_endpoint` object.
#'
#' @param endpoint Character; Azure Blob endpoint URL.
#' @param key Character; access key. Defaults to reading from environment variable "WLF_BLOB_KEY".
#'
#' @return `AzureStor::storage_endpoint` object.
#' @export
azure_store_endpoint <- function(
    endpoint = "https://xxxxxxxxxxxxxx.blob.core.windows.net",
    key = get_environment_key("WLF_BLOB_KEY")
){
  AzureStor::storage_endpoint(endpoint, key = key)
}




# ----------------------
# Container helpers
# ----------------------


#' List all container names
#'
#' @description Returns a character vector of all container names in the given Azure Blob Storage endpoint.
#'
#' @param end_point AzureStor endpoint object. Default uses `azure_store_endpoint()`.
#'
#' @return Character vector of container names.
#' @export
azure_container_names <- function(end_point = azure_store_endpoint()) {
  AzureStor::list_storage_containers(end_point) %>% names()
}



#' Check if a container exists
#'
#' @description Returns TRUE if a container exists in the Azure endpoint, FALSE otherwise.
#'
#' @param container Character; container name to check.
#' @param end_point AzureStor endpoint object.
#'
#' @return Logical.
#' @export
azure_container_exists <- function(container, end_point = azure_store_endpoint()) {
  container %in% azure_container_names(end_point)
}



#' Validate container existence
#'
#' @description Validates that a container exists or does not exist according to `check_negate`.
#' Stops execution or warns based on `hard_stop` and `verbose`.
#'
#' @param container Character; container name.
#' @param end_point AzureStor endpoint object.
#' @param check_negate Logical; if TRUE, check that container exists; if FALSE, check that it does NOT exist.
#' @param hard_stop Logical; if TRUE, stop execution on failure; if FALSE, issue warning.
#' @param verbose Logical; whether to display messages.
#'
#' @return Logical indicating whether validation passed.
#' @export
azure_container_validation <- function(
    container,
    end_point = azure_store_endpoint(),
    check_negate = TRUE,
    hard_stop = TRUE,
    verbose = TRUE
) {
  pass <- TRUE
  if (check_negate) {
    msg <- glue::glue("Container '{container}' not found.")
    if (!azure_container_exists(container, end_point)) pass <- FALSE
  } else {
    msg <- glue::glue("Container '{container}' already exists.")
    if (azure_container_exists(container, end_point)) pass <- FALSE
  }

  if (!verbose) msg <- ""
  if (hard_stop && !pass) stop(msg)
  if (!hard_stop && !pass && verbose) warning(msg)
  pass
}



#' Get container object
#'
#' @description Returns an AzureStor container object for file operations.
#'
#' @param container Character; container name.
#' @param end_point AzureStor endpoint object.
#' @param hard_stop Logical; stop if container not found.
#' @param verbose Logical; show warning messages if container not found.
#'
#' @return `AzureStor::blob_container` object, or FALSE if not found and `hard_stop = FALSE`.
#' @export
azure_container_get <- function(
    container,
    end_point = azure_store_endpoint(),
    hard_stop = TRUE,
    verbose = TRUE
) {
  valid <- azure_container_validation(container, end_point, hard_stop = hard_stop, verbose = verbose)
  if (!valid) return(FALSE)
  AzureStor::storage_container(end_point, container)
}



#' Create a new container
#'
#' @description Creates a new Azure Blob Storage container.
#'
#' @param container Character; container name to create.
#' @param end_point AzureStor endpoint object.
#'
#' @return Container object invisibly.
#' @export
azure_container_create <- function(container, end_point = azure_store_endpoint()) {
  azure_container_validation(container, end_point, check_negate = FALSE)
  AzureStor::create_storage_container(end_point, container)
}



#' Delete a container
#'
#' @description Deletes an Azure Blob Storage container, optionally prompting for confirmation.
#'
#' @param container Character; container name.
#' @param confirm Logical; whether to prompt for confirmation.
#' @param end_point AzureStor endpoint object.
#'
#' @return Invisibly returns TRUE.
#' @export
azure_container_delete <- function(container, confirm = TRUE, end_point = azure_store_endpoint()) {
  azure_container_validation(container, end_point)
  AzureStor::delete_storage_container(end_point, container, confirm)
  invisible(TRUE)
}



# ----------------------
# Directory helpers
# ----------------------


#' Check if a directory exists within a container
#'
#' @description Returns TRUE if the specified directory exists in the given container.
#'
#' @param container Character; container name.
#' @param dir Character; directory path within container.
#' @param end_point AzureStor endpoint object.
#' @param hard_stop Logical; stop execution if container not found.
#' @param verbose Logical; show warning messages.
#'
#' @return Logical.
#' @export
azure_container_dir_exists <- function(container, dir, end_point = azure_store_endpoint(), hard_stop = FALSE, verbose = TRUE){
  cont <- azure_container_get(container, end_point, hard_stop = hard_stop, verbose = verbose)
  if (isFALSE(cont)) return(FALSE)
  AzureStor::storage_dir_exists(cont, dir)
}



# ----------------------
# File helpers with progress
# ----------------------


#' List files in container(s)
#'
#' @description Lists all files in one or more containers with a `container` column added.
#'
#' @param container Character vector; containers to list. NULL lists all containers.
#' @param end_point AzureStor endpoint object.
#'
#' @return tibble/data.frame of files.
#' @export
azure_file_list <- function(container = NULL, end_point = azure_store_endpoint()) {
  containers_to_list <- if (is.null(container)) azure_container_names(end_point) else as.character(container)

  cli::cli_progress_bar("Listing files in containers", total = length(containers_to_list))

  output <- purrr::map_dfr(containers_to_list, function(cont_name){
    cli::cli_progress_update()
    cont <- azure_container_get(cont_name, end_point)
    AzureStor::list_storage_files(cont) %>%
      dplyr::mutate(container = cont_name, .before = 1)
  })

  cli::cli_progress_done()
  output
}



#' Delete files in container(s)
#'
#' @description Delete one or more files in a container with optional confirmation and progress bar.
#'
#' @param container Character; container name.
#' @param container_path Character vector; file paths to delete.
#' @param confirm Logical; whether to prompt for confirmation.
#' @param end_point AzureStor endpoint object.
#'
#' @return Invisibly TRUE.
#' @export
azure_file_delete <- function(container, container_path, confirm = TRUE, end_point = azure_store_endpoint()) {
  cont <- azure_container_get(container, end_point)
  container_path <- as.character(container_path)

  cli::cli_progress_bar("Deleting files", total = length(container_path))
  purrr::walk(container_path, function(path) {
    cli::cli_progress_update()
    AzureStor::delete_storage_file(cont, path, confirm = confirm)
  })
  cli::cli_progress_done()
  invisible(TRUE)
}



#' Save R object as RDS to Azure
#'
#' @description Save a single R object as an RDS file in the specified container. Automatically
#' generates a file name if `container_path` is NULL, and ensures `.rds` extension.
#'
#' @param obj R object to save.
#' @param container Character; container name.
#' @param container_path Character; path to save the file within the container.
#' @param end_point AzureStor endpoint object.
#'
#' @return Path of the saved file (invisible).
#' @export
azure_file_save_rds <- function(obj, container, container_path = NULL, end_point = azure_store_endpoint()) {
  cont <- azure_container_get(container, end_point)
  obj_name <- envnames::get_obj_name(obj, n = 2)

  if (is.null(container_path)) container_path <- glue::glue("{obj_name}.rds")
  if (substr(container_path, nchar(container_path), nchar(container_path)) == "/") {
    container_path <- glue::glue("{container_path}{obj_name}.rds")
  }
  if (tools::file_ext(container_path) != "rds") container_path <- glue::glue("{container_path}.rds")

  AzureStor::storage_save_rds(obj, cont, container_path)
  invisible(container_path)
}



#' Load RDS object from Azure
#'
#' @description Load a single R object from an RDS file stored in Azure.
#'
#' @param container_path Character; path to the RDS file.
#' @param container Character; container name.
#' @param end_point AzureStor endpoint object.
#'
#' @return The R object stored in the RDS file.
#' @export
azure_file_load_rds <- function(container_path, container, end_point = azure_store_endpoint()) {
  cont <- azure_container_get(container, end_point)
  if (tools::file_ext(container_path) != "rds") container_path <- glue::glue("{container_path}.rds")
  AzureStor::storage_load_rds(cont, container_path)
}



#' Save object(s) as RData to Azure
#'
#' @description Save one or more objects as an `.RData` file in a container.
#'
#' @param obj R object or list of objects.
#' @param container Character; container name.
#' @param container_path Character; path to save the `.RData` file.
#' @param end_point AzureStor endpoint object.
#'
#' @return Path of the saved file (invisible).
#' @export
azure_file_save_rdata <- function(obj, container, container_path = NULL, end_point = azure_store_endpoint()) {
  cont <- azure_container_get(container, end_point)
  if (is.null(container_path)) container_path <- glue::glue("{envnames::get_obj_name(obj, n = 2)}.RData")
  if (tools::file_ext(container_path) != "RData") container_path <- glue::glue("{container_path}.RData")
  AzureStor::storage_save_rdata(obj, cont, container_path)
  invisible(container_path)
}



#' azure_file_load_rdata
#'
#' @description
#' Load one or more `.RData` files from Azure Blob Storage.
#' Each file’s contents are loaded into the specified environment (default = parent frame).
#' A progress bar displays load progress for multiple files.
#'
#' @param container Character; container name.
#' @param container_path Character vector; one or more paths to `.RData` files in the container.
#' @param end_point AzureStor endpoint object (default = `azure_store_endpoint()`).
#' @param envir Environment to load the objects into. Defaults to `parent.frame()`.
#'
#' @return
#' Invisibly returns a character vector of all object names loaded.
#'
#' @export
azure_file_load_rdata <- function(
    container,
    container_path,
    end_point = azure_store_endpoint(),
    envir = parent.frame()
) {
  start(lib_azure = TRUE)

  # Get container reference
  cont <- azure_container_get(container, end_point)

  # Normalize extensions
  container_path <- as.character(container_path)
  container_path <- ifelse(
    tools::file_ext(container_path) != "RData",
    glue::glue("{container_path}.RData"),
    container_path
  )

  # Initialize progress bar
  cli::cli_progress_bar("Loading .RData files", total = length(container_path))
  loaded_objects <- c()

  for (path in container_path) {
    cli::cli_progress_update()
    tmp <- tempfile(fileext = ".RData")

    # Download file locally then load it
    AzureStor::storage_download(cont, src = path, dest = tmp, overwrite = TRUE)
    objs_before <- ls(envir = envir)
    base::load(tmp, envir = envir)
    objs_after <- ls(envir = envir)
    new_objs <- setdiff(objs_after, objs_before)
    loaded_objects <- c(loaded_objects, new_objs)
  }

  cli::cli_progress_done()
  invisible(unique(loaded_objects))
}
