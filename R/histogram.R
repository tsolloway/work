#' histogram_single
#' @description histogram_single
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

  require(highcharter)

  x <- x[!is.na(x)]

  if( is.null(likert) || is.na(likert) ){

    unique_values <- x %>% unique() %>% length()

    if( unique_values >= arbitrary_likert_cutoff ){
      likert <- FALSE
    }else if( unique_values < arbitrary_likert_cutoff && unique_values > 0 ){
      likert <- TRUE
    }

  }

  if(likert == TRUE){

    is_numeric <- TRUE
    if( !is.numeric(x) ) is_numeric <- FALSE

    if( !is.factor(x) ) x <- factor(x)

    hc <- highchart()

    if(is_numeric) hc <- hc %>% hc_add_series(data = x, type = "areaspline")

    hc <- hc %>% hc_add_series(data = x, type = "column")

  }else if(likert == FALSE){

    hc <- hchart(density(x), type = "area")
  }

  hc <- hc %>%
    hc_title(text = title) %>%
    hc_xAxis(type = "category", title = list(text = title_x_axis)) %>%
    hc_yAxis(title = list(text = title_y_axis)) %>%
    hc_legend(FALSE) %>%
    hc_size(width = width, height = height) %>%
    hc_exporting(enabled = TRUE, filename = default_file_name) %>%
    hc_tooltip(
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



#' histogram
#' @description histogram
#' @export
histogram <- function(
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

  if( !is.null(variables) && is.data.frame(x) ){

    x <- x %>% dplyr::select(dplyr::all_of(variables))

  }


  if( ncol(x) == 1 || is.numeric(x) ){

    histogram_single(
      x, title = title, title_x_axis = title_x_axis, title_y_axis=title_y_axis, likert = likert,
      arbitrary_likert_cutoff = arbitrary_likert_cutoff, default_file_name = default_file_name,
      width = width, height = height, theme = theme
    )

  }else if( is.data.frame(x) && ncol(x) > 1 ){

    purrr::pmap(
      list(x, names(x)),
      ~ histogram_single(
        x = .x, title = .y,
        title_x_axis = title_x_axis, likert = likert,
        arbitrary_likert_cutoff = arbitrary_likert_cutoff,
        default_file_name = default_file_name,
        width = width, height = height,theme = theme
      )
    ) %>% highcharter::hw_grid(ncol = grid_n_col)

  }

}
