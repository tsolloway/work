#' field_recode
#' @description field_recode
#' @export
field_recode <- function(
    x, type = c("none", "tb", "t2b", "t3b", "custom"),
    custom_car_recode_syntax = NULL, df = NULL
){

  type <- match.arg(type)


  if( is.vector(x) && is.numeric(x) ){
    #good to go
  }else if( is.character(x) && is.data.frame(df) && x %in% names(df) ){
    x <- df[[x]]
  }else{
    stop("something is wrong with the inputs to var_recode")
  }


  if(type == "none"){


  }else if(type == "tb"){

    top <- x %>% max(na.rm = TRUE)
    recode_syntax <- top %>% paste0(" = 1; NA = NA; else = 0")

    x <- x %>% car::recode(recode_syntax)

  }else if(type == "t2b"){

    top2 <- x %>% .[!is.na(.)] %>% unique() %>% sort() %>% tail(2)
    recode_syntax <- top2 %>% paste0(collapse = ":") %>% paste0(" = 1; NA = NA; else = 0")

    x <- x %>% car::recode(recode_syntax)

  }else if(type == "t3b"){

    top3 <- x %>% .[!is.na(.)] %>% unique() %>% sort() %>% tail(3) %>% .[-2]
    recode_syntax <- top3 %>% paste0(collapse = ":") %>% paste0(" = 1; NA = NA; else = 0")

    x <- x %>% car::recode(recode_syntax)

  }else if(type == "custom"){
    if( is.null(custom_car_recode_syntax) ) stop("We requested a custom recode without providing the syntax")
    x <- x %>% car::recode(custom_car_recode_syntax)
  }

  return(x)

}



#' fields_recode
#' @description fields_recode
#' @export
fields_recode <- function(
    x, type = c("none", "tb", "t2b", "t3b", "custom"),
    custom_car_recode_syntax = NULL
  ){

  type <- match.arg(type)

  if( ncol(x) == 1 || is.vector(x) ){
    x %>% field_recode(type = type, custom_car_recode_syntax = custom_car_recode_syntax)
  }else if( ncol(x) > 1 ){
    x %>% apply(2, function(x)field_recode(x = x, type = type, custom_car_recode_syntax = custom_car_recode_syntax))
  }
}
