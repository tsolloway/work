#' today
#' @description Returns today's date.
#' @return A `Date` object representing today.
#' @export
today <- function() {
  lubridate::today()
}


#' this_day
#' @description Returns the day of the month for today's date.
#' @return An integer (1–31).
#' @export
this_day <- function() {
  lubridate::day(lubridate::today())
}


#' this_month
#' @description Returns information about the current month.
#' @param part One of `"number"`, `"start"`, or `"end"`.
#' @return The month number or a `Date` for the start or end of the month.
#' @export
this_month <- function(part = c("number", "start", "end")) {
  part <- match.arg(part)

  if (part == "number") {
    lubridate::month(lubridate::today())
  } else if (part == "start") {
    lubridate::floor_date(lubridate::today(), "month")
  } else {
    lubridate::ceiling_date(lubridate::today(), "month") - lubridate::days(1)
  }
}


#' last_month
#' @description Returns information about the previous month.
#' @param part One of `"number"`, `"start"`, or `"end"`.
#' @return The month number or a `Date` for the start or end of the previous month.
#' @export
last_month <- function(part = c("number", "start", "end")) {
  part <- match.arg(part)
  prev_month_date <- lubridate::today() %m-% months(1)

  if (part == "number") {
    lubridate::month(prev_month_date)
  } else if (part == "start") {
    lubridate::floor_date(prev_month_date, "month")
  } else {
    lubridate::ceiling_date(prev_month_date, "month") - lubridate::days(1)
  }
}


#' this_year
#' @description Returns information about the current year.
#' @param part One of `"number"`, `"start"`, or `"end"`.
#' @return The year number or a `Date` for the start or end of the year.
#' @export
this_year <- function(part = c("number", "start", "end")) {
  part <- match.arg(part)

  if (part == "number") {
    lubridate::year(lubridate::today())
  } else if (part == "start") {
    lubridate::floor_date(lubridate::today(), "year")
  } else {
    lubridate::ceiling_date(lubridate::today(), "year") - lubridate::days(1)
  }
}


#' date_seq
#' @description Generate a sequence of dates between two points.
#' @param start Start date (character or Date).
#' @param stop End date (character or Date).
#' @param interval One of `"day"`, `"month"`, `"quarter"`, or `"year"`.
#' @param steps Number of intervals between each date.
#' @return A vector of `Date` objects.
#' @export
date_seq <- function(start, stop, interval = c("day", "month", "quarter", "year"), steps = 1) {
  interval <- match.arg(interval)
  by <- glue::glue("{steps} {interval}")

  seq(
    lubridate::ymd(start),
    lubridate::ymd(stop),
    by = by
  )
}


#' calendar
#' @description Generate a calendar of dates over a range of years.
#' @param interval Interval for sequence ("day", "month", "quarter", "year").
#' @param first_year Starting year.
#' @param last_year Ending year.
#' @param steps Step size.
#' @param to_now If `TRUE`, truncate dates to the current day.
#' @return A vector of `Date` objects.
#' @export
calendar <- function(
    interval = c("day", "month", "quarter", "year"),
    first_year = this_year(),
    last_year = this_year(),
    steps = 1,
    to_now = FALSE
) {
  interval <- match.arg(interval)

  date_start <- glue::glue("{first_year}-01-01")
  date_end <- glue::glue("{last_year}-12-31")

  result <- date_seq(date_start, date_end, interval, steps)

  if (to_now) {
    result <- result[result <= lubridate::today()]
  }

  result
}
