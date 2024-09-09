#' today
#' @description today
#' @export
today <- function(){
  lubridate::today()
}



#' this_day
#' @description this_day
#' @export
this_day <- function(){
  lubridate::today() %>% lubridate::day()
}



#' this_month
#' @description this_month
#' @export
this_month <- function(
    part = c("number", "start", "end")
){

  part <- match.arg(part)

  if(part == "number"){
    lubridate::today() %>% lubridate::month()
  }else if(part == "start"){
    lubridate::today() %>% lubridate::floor_date("month")
  }else if(part == "end"){
    lubridate::today() %>% lubridate::ceiling_date("month") - lubridate::day(1) %>% suppressWarnings()
  }
}



#' last_month
#' @description last_month
#' @export
last_month <- function(part = c("number", "start", "end")){

  part <- match.arg(part)

  if(part == "number"){
    lubridate::today() %>% lubridate::month() - 1
  }else if(part == "start"){
    (this_month("start") - lubridate::days(1)) %>% lubridate::floor_date("month")
  }else if(part == "end"){
    (this_month("start") - lubridate::days(1))
  }
}



#' this_day
#' @description this_day
#' @export
this_year <- function(
    part = c("number", "start", "end")
){

  part <- match.arg(part)

  if(part == "number"){
    lubridate::today() %>% lubridate::year()
  }else if(part == "start"){
    lubridate::today() %>% lubridate::floor_date("year")
  }else if(part == "end"){
    lubridate::today() %>% lubridate::ceiling_date("year") - lubridate::day(1) %>% suppressWarnings()
  }
}



#' date_seq
#' @description date_seq
#' @export
date_seq <- function(start, stop, interval = c("day", "month", "quarter", "year"), steps = 1){

  interval <- match.arg(interval)

  interval <- glue("{steps} {interval}")

  seq(
    lubridate::ymd(start),
    lubridate::ymd(stop),
    by = interval
  )
}



#' calendar
#' @description calendar
#' @export
calendar <- function(
    interval = c("day", "month", "quarter", "year"),
    first_year = this_year(),
    last_year = this_year(),
    steps = 1,
    to_now = FALSE
){

  interval <- match.arg(interval)

  date_start <- (glue("{first_year}-01-01"))
  date_end <- (glue("{last_year}-12-31"))


  result <- date_seq(date_start, date_end, interval, steps)


  if(to_now){
    result <- result[result <= today()]
  }


  return(result)
}






