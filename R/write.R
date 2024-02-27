#' write
#' @description write
#' @export
write <- function(
    x,
    where = c("here", "desktop", "downloads", "onedrive", "file"),
    file = NULL,
    type = c("csv")
){

  type <- match.arg(type)
  where <- match.arg(where)


  if( is.null(file) ){
    file <- work::object_name(x)
    if( file == "x" ){
      file <- deparse(substitute(x))
    }
  }


  where <- switch(
    where,
    "here" = getwd(),
    "desktop" = work::get_path("desktop"),
    "downloads" = work::get_path("downloads"),
    "onedrive" = work::get_path("onedrive"),
    "file" = ""
  )


  if( tolower(tools::file_ext(file)) != type ){

    file <- paste0(file, ".", type)

  }


  if( work::is_truthy(where) ){

    if( work::left(file, 1) == "/" || work::left(file, 1) == "\\" ){

      file <- paste0(where, file)

    }else{

      file <- paste0(where, "/", file)

    }

  }


  if( type == "csv" ){

    write.csv(x = x, file = file, row.names = FALSE, na= "")

  }


}
