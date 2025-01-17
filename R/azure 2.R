
#' azure_store_endpoint
#' @description azure_store_endpoint
#' @export
azure_store_endpoint <- function(
    endpoint = "https://xxxxxxxxxxxxxx.blob.core.windows.net",
    key = get_environment_key("WLF_BLOB_KEY")
){

  start(lib_azure = TRUE)

  storage_endpoint(
    endpoint = endpoint,
    key=key
  )

}


#' azure_container_lst
#' @description azure_container_lst
#' @export
azure_container_lst <- function(
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  list_storage_containers(end_point)
}


#' azure_container_lst
#' @description azure_container_lst
#' @export
azure_container_names <- function(
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  azure_container_lst(end_point) %>% names()
}



#' azure_container_exists
#' @description azure_container_exists
#' @export
azure_container_exists <- function(
    container,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  container %in% azure_container_names(end_point)
}



#' azure_container_validation
#' @description azure_container_validation
#' @export
azure_container_validation <- function(
    container,
    end_point = azure_store_endpoint(),
    check_negate = TRUE,
    hard_stop = TRUE,
    verbose = TRUE
){
  start(lib_azure = TRUE)

  pass <- TRUE

  if( check_negate ){
    msg <- glue("container '{container}' not found.")

    if( !azure_container_exists(container, end_point) ){
      pass <- FALSE
    }
  }else if( !check_negate ){

    msg <- glue("container '{container}' already exists.")

    if( azure_container_exists(container, end_point) ){
      pass <- FALSE
    }
  }

  if(!verbose) msg <- ""

  if(hard_stop && !pass) stop(msg)

  if(!hard_stop && !pass && verbose) warning(msg)

  return(pass)
}



#' azure_container_get
#' @description azure_container_get
#' @export
azure_container_get <- function(
    container,
    end_point = azure_store_endpoint(),
    hard_stop = TRUE,
    verbose = TRUE
){
  start(lib_azure = TRUE)

  x <- azure_container_validation(container, end_point, hard_stop = hard_stop, verbose = verbose)

  if(!x){
    return(FALSE)
  }else if(x){
    storage_container(end_point, container)
  }
}



#' azure_container_delete
#' @description azure_container_delete
#' @export
azure_container_delete <- function(
    container,
    confirm = TRUE,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  azure_container_validation(container, end_point)

  delete_storage_container(end_point, container, confirm)
}



#' azure_container_create
#' @description azure_container_create
#' @export
azure_container_create <- function(
    container,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  azure_container_validation(container, end_point, check_negate = FALSE)

  create_storage_container(end_point, container)
}



#' azure_container_dir_exists
#' @description azure_container_dir_exists
#' @export
azure_container_dir_exists <- function(
    container,
    dir,
    end_point = azure_store_endpoint(),
    hard_stop = FALSE,
    verbose = TRUE
){
  start(lib_azure = TRUE)

  x <- azure_container_get(container, end_point, hard_stop = hard_stop, verbose = verbose)

  if(isFALSE(x)){
    return(FALSE)
  }else{
    storage_dir_exists(x, dir)
  }

}



#' azure_file_save_rds
#' @description azure_file_save_rds
#' @export
azure_file_save_rds <- function(
    obj,
    container,
    container_path = NULL,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)


  if( is.null(container_path) ){

    container_path <- glue("{object_name(obj)}.rds")

  }else if(right(container_path, 1) == "/" ){

    container_path <- glue("{container_path}{object_name(obj)}.rds")

  }

  container <- azure_container_get(container, end_point)

  storage_save_rds(obj, container, container_path)
}



#' azure_file_load_rds
#' @description azure_file_load_rds
#' @export
azure_file_load_rds <- function(
    container_path,
    container,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  container <- azure_container_get(container, end_point)

  storage_load_rds(container, container_path)
}




#' azure_file_save_rdata
#' @description azure_file_save_rdata
#' @export
azure_file_save_rdata <- function(
    obj,
    container,
    container_path = NULL,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  container <- azure_container_get(container, end_point)

  storage_save_rdata(..., container, container_path)
}



#' azure_file_load_rdata
#' @description azure_file_load_rdata
#' @export
azure_file_load_rdata <- function(
    ...,
    container,
    container_path,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  container <- azure_container_get(container, end_point)

  storage_load_rds(..., container, container_path)
}



#' azure_file_delete
#' @description azure_file_delete
#' @export
azure_file_delete <- function(
    container,
    container_path,
    confirm = TRUE,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)

  container <- azure_container_get(container, end_point)

  delete_storage_file(container, container_path, confirm = confirm)
}



#' azure_file_list
#' @description azure_file_list
#' @export
azure_file_list <- function(
    container = NULL,
    end_point = azure_store_endpoint()
){
  start(lib_azure = TRUE)


  get_container_files <- function(container, end_point){
    azure_container_get(container, end_point) %>%
      list_storage_files()
  }


  if(is.null(container)){
    output <- azure_container_names(end_point = end_point) %>%
      map(
        ~get_container_files(.x, end_point) %>%
          mutate(container = .x, .before = 0)
      ) %>%
      bind_rows()
  }
  else if(!is.null(container)){
    output <- get_container_files(container, end_point)
  }

  return(output)
}



