#' this_day
#' @description this_day
#' @export
this_day <- function(){
  Sys.Date()
}



#' this_month
#' @description this_month
#' @export
this_month <- function(part = c("start", "end")){

  part <- match.arg(part)

  if(part == "start"){
    this_day() %>% lubridate::floor_date("month")
  }else if(part == "end"){
    this_day() %>% lubridate::ceiling_date("month") - lubridate::day(1) %>% suppressWarnings()
  }
}



#' last_month
#' @description last_month
#' @export
last_month <- function(part = c("start", "end")){

  part <- match.arg(part)

  if(part == "start"){
    (this_month() - lubridate::month(1)) %>% lubridate::floor_date("month")
  }else if(part == "end"){
    this_month() - lubridate::month(1)
  }
}
