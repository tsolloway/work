#' Create a single Highcharter histogram
#'
#' @description
#' Creates a histogram (or Likert-style column chart) for a single variable using `highcharter`.
#' Automatically detects Likert-type variables based on the number of unique values.
#'
#' @param x Numeric or factor vector.
#' @param title Chart title.
#' @param title_x_axis X-axis title.
#' @param title_y_axis Y-axis title. Defaults to `"Frequency"`.
#' @param likert Logical; force Likert treatment. If `NULL`, automatically determined.
#' @param arbitrary_likert_cutoff Integer; maximum number of unique values to consider Likert. Default 13.
#' @param default_file_name Filename used for exporting. Default `"filename"`.
#' @param width,height Chart size in pixels.
#' @param theme Highcharter theme name; see `hc_theme_picker()`.
#'
#' @return A `highchart` object.
#'
#' @examples
#' \dontrun{
#' histogram_single(1:10)
#' histogram_single(iris$Sepal.Length)
#' }
#'
#' @export
histogram_single <- function(
    x, title = NULL, title_x_axis = NULL, title_y_axis = "Frequency", likert = NULL,
    arbitrary_likert_cutoff = 13, default_file_name = "filename",
    width = NULL, height = NULL,
    theme = c(
      "538", "alone", "bloom", "chalk", "darkunica", "db", "economist",
      "elementary", "ffx", "flat", "flatdark", "ft", "ggplot2", "google",
      "gridlight", "handdrawn", "hcrt", "merge", "monokai", "null",
      "sandsignika", "smpl", "sparkline", "sparkline_vb", "superheroes",
      "tufte","tufte2"
    )
){

  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Package 'highcharter' is required for histogram_single()")
  }

  theme <- match.arg(theme)

  hc <- highcharter::highchart()

  x <- x[!is.na(x)]


  if (is.null(likert)) {
    unique_values <- x %>% unique() %>% length()
    likert <- (unique_values > 0) && (unique_values < arbitrary_likert_cutoff)
  }


  if(likert){

    is_numeric <- is.numeric(x)

    if( !is.factor(x) ) x <- factor(x)

    if(is_numeric) hc <- hc %>% hc_add_series(data = x, type = "areaspline")

    hc <- hc %>% hc_add_series(data = x, type = "column")

  }else if(!likert){

    hc <- hchart(stats::density(x), type = "area")
  }


  hc <- hc %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_xAxis(type = "category", title = list(text = title_x_axis)) %>%
    highcharter::hc_yAxis(title = list(text = title_y_axis)) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_size(width = width, height = height) %>%
    highcharter::hc_exporting(enabled = TRUE, filename = default_file_name) %>%
    highcharter::hc_tooltip(
      formatter = htmlwidgets::JS(
        paste0("function(){
          return ('Value: ' + Highcharts.numberFormat(this.x + 1, 0) +
          '<br> Count: ' + this.y +
          '<br> Percent: ' + Highcharts.numberFormat(this.y/", length(x),"*100)+ '%')}")
      )
    )


  if( !is_nothing(theme) && !isFALSE(theme) ){
    hc <- hc %>% hc_add_theme(hc_theme_picker(theme))
  }


  return(hc)
}



#' Create histograms for one or more variables
#'
#' @description
#' Generates Highcharter histograms for a single variable, or for multiple variables
#' in a data frame. Multiple charts are arranged in a grid.
#'
#' @param x Numeric vector or data frame.
#' @param variables Optional vector of column names if `x` is a data frame.
#' @param title, title_x_axis, title_y_axis Chart titles and axis labels.
#' @param likert Logical; force Likert treatment. Default NULL for auto-detection.
#' @param arbitrary_likert_cutoff Integer; maximum unique values for Likert. Default 13.
#' @param default_file_name Filename used for exporting. Default `"filename"`.
#' @param width,height Chart size in pixels.
#' @param grid_n_col Number of columns when arranging multiple charts. Default 3.
#' @param theme Highcharter theme name; see `hc_theme_picker()`.
#'
#' @return A single Highchart or a grid of Highcharts.
#'
#' @examples
#' \dontrun{
#' hc_histogram(iris$Sepal.Length)
#' hc_histogram(iris[,1:3])
#' }
#'
#' @export
hc_histogram <- function(
    x, variables = NULL, title = NULL, title_x_axis = NULL, title_y_axis = "Frequency", likert = NULL,
    arbitrary_likert_cutoff = 13, default_file_name = "filename",
    width = NULL, height = NULL, grid_n_col = 3,
    theme = c(
      "538", "alone", "bloom", "chalk", "darkunica", "db", "economist",
      "elementary", "ffx", "flat", "flatdark", "ft", "ggplot2", "google",
      "gridlight", "handdrawn", "hcrt", "merge", "monokai", "null",
      "sandsignika", "smpl", "sparkline", "sparkline_vb", "superheroes",
      "tufte","tufte2"
    )
){

  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Package 'highcharter' is required for histogram_single()")
  }


  theme <- match.arg(theme)


  if (!is.null(variables) && is.data.frame(x)) {
    x <- x %>% dplyr::select(dplyr::all_of(variables))
  }



  if (is.data.frame(x) && ncol(x) > 1) {

    purrr::pmap(
      list(x, names(x)),
      ~ histogram_single(
        x = .x, title = .y,
        title_x_axis = title_x_axis, title_y_axis = title_y_axis,
        likert = likert, arbitrary_likert_cutoff = arbitrary_likert_cutoff,
        default_file_name = default_file_name, width = width, height = height,
        theme = theme
      )
    ) %>%
      highcharter::hw_grid(ncol = grid_n_col)

  } else {
    histogram_single(
      x = if (is.data.frame(x)) x[[1]] else x,
      title = title, title_x_axis = title_x_axis, title_y_axis = title_y_axis,
      likert = likert, arbitrary_likert_cutoff = arbitrary_likert_cutoff,
      default_file_name = default_file_name, width = width, height = height,
      theme = theme
    )
  }

}




