#' write
#' @description write
#' @export
write <- function(
    x,
    where = c("here", "desktop", "downloads", "onedrive", "file"),
    file = NULL,
    type = c("csv", "xlsx")
){

  type <- match.arg(type)
  where <- match.arg(where)


  if(
    is_truthy(attr(x, "write_type")) &&
    is_truthy(attr(x, "analysis"))
  ){

    if(
      attr(x, "write_type") == "openxlsx_formatted" &&
      attr(x, "analysis") == "dmd_check"
    ){
      type <- "xlsx"
      if( is.null(file) ){
        file <- paste0("dmd-check-", Sys.Date(), ".xlsx")
      }
    }
  }


  if( is.null(file) ){
    file <- object_name(x)
    if( file == "x" ){
      file <- deparse(substitute(x))
    }
  }


  where <- switch(
    where,
    "here" = getwd(),
    "desktop" = get_path("desktop"),
    "downloads" = get_path("downloads"),
    "onedrive" = get_path("onedrive"),
    "file" = ""
  )


  if( tolower(tools::file_ext(file)) != type ){
    file <- paste0(file, ".", type)
  }


  if( is_truthy(where) ){

    if( left(file, 1) == "/" || left(file, 1) == "\\" ){
      file <- paste0(where, file)
    }else{
      file <- paste0(where, "/", file)
    }

  }


  if( type == "csv" ){
    write.csv(x = x, file = file, row.names = FALSE, na= "")
  }else if( type == "xlsx" ){
    openxlsx::saveWorkbook(wb = x, file = file, overwrite = TRUE)
  }

}
