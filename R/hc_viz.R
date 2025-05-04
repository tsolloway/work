#' hc_viz
#' @description hc_viz
#' @export
hc_viz <- function(
    df, xvar, yvar, grp = NULL,
    type = c("spline", "scatter", "area"),
    stack = FALSE,
    chart_title = NULL,
    xaxis_title = NULL,
    yaxis_title = NULL,
    theme = hc_theme_picker("economist"),
    exportable = TRUE,
    regresssion = TRUE,
    regresssion_type = c("linear", "polynomial"),
    regresssion_poly_order = 2,
    regression_line_width = 5,
    regression_line_name = "Trend",
    regression_hide_legend = TRUE,
    tooltip_formatter_instructions = NULL,
    tooltip_shared = FALSE,
    tooltip_round_digit = 2
){

  require(highcharter, quietly = TRUE)

  regresssion_type <- match.arg(regresssion_type)
  type <- match.arg(type)

  if(tooltip_shared || !is.null(tooltip_formatter_instructions) || !is.null(tooltip_round_digit) ){
    custom_tooltip <- TRUE
  }else {
    custom_tooltip <- FALSE
  }


  instructions <- ifelse(
    is.null(grp),
    "hcaes(x = !!xvar, y = !!yvar)",
    "hcaes(x = !!xvar, y = !!yvar, group = !!grp)"
  )


  if(is.null(xaxis_title)) xaxis_title <- xvar %>% gsub("_", " ", .)

  if(is.null(yaxis_title)) yaxis_title <- yvar %>% gsub("_", " ", .)


  hc <- hchart(
      df,
      type,
      eval(parse(text = instructions)),
      regression = regresssion,
      regressionSettings = list(
        type = regresssion_type %>% as.character(),
        dashStyle = "ShortDash",
        order = regresssion_poly_order %>% as.numeric(),
        lineWidth = regression_line_width %>% as.numeric(),
        name = regression_line_name %>% as.character(),
        hideInLegend = regression_hide_legend %>% as.logical()
      )
    ) %>%
    hc_title(text = chart_title) %>%
    hc_xAxis(
      title = list(text = xaxis_title)
    ) %>%
    hc_yAxis(
      title = list(text = yaxis_title)
    )


  if(!is.null(tooltip_formatter_instructions)){
    if(tooltip_formatter_instructions == "option_1"){

      tooltip_shared <- TRUE

      tooltip_formatter_instructions <-
        JS("
        function() {
         console.log(this.points);
         var s = '<b>' + Highcharts.dateFormat('%Y-%m', this.x) + '</b><br/><b>TOTAL: ' + Highcharts.numberFormat(this.points[0].total, 2) + '</b>';
         $.each(this.points, function(i, point) {
         s += '<br/><span style=\"color:' + point.series.color + '\">' + point.series.name + ': ' + Highcharts.numberFormat(point.y, 2) + '</span>';
         });
         return s;
         }
           ")
    }
  }


  if(regresssion) hc <- hc %>% hc_add_dependency("plugins/highcharts-regression.js")

  if(stack) hc <- hc %>% hc_plotOptions(series=list(stacking='normal'))


  if(custom_tooltip){
    if(!is.null(tooltip_formatter_instructions)){
      hc <- hc %>% hc_tooltip(shared = tooltip_shared, formatter = tooltip_formatter_instructions)
    }else if(is.null(tooltip_formatter_instructions))
      hc <- hc %>% hc_tooltip(shared = tooltip_shared, valueDecimals = tooltip_round_digit)
  }


  if(!is.null(theme)) hc <- hc %>% hc_add_theme(theme)

  if(exportable) hc <- hc %>% hc_exporting(enabled = T)

  return(hc)
}



#' hc_spline
#' @description hc_spline
#' @export
hc_spline <- function(
    df, xvar, yvar, grp = NULL,
    stack = FALSE,
    chart_title = NULL,
    xaxis_title = NULL,
    yaxis_title = NULL,
    theme = hc_theme_picker("economist"),
    exportable = TRUE,
    regresssion = TRUE,
    regresssion_type = c("linear", "polynomial"),
    regresssion_poly_order = 2,
    regression_line_width = 5,
    regression_line_name = "Trend",
    regression_hide_legend = TRUE,
    tooltip_formatter_instructions = NULL,
    tooltip_shared = FALSE,
    tooltip_round_digit = 2
){
  hc_viz(
    df = df,
    xvar = xvar,
    yvar = yvar,
    grp = grp,
    type = "spline",
    stack = stack,
    chart_title = chart_title,
    xaxis_title = xaxis_title,
    yaxis_title = yaxis_title,
    theme = theme,
    exportable = exportable,
    regresssion = regresssion,
    regresssion_type = regresssion_type,
    regresssion_poly_order = regresssion_poly_order,
    regression_line_width = regression_line_width,
    regression_line_name = regression_line_name,
    regression_hide_legend = regression_hide_legend,
    tooltip_formatter_instructions = tooltip_formatter_instructions,
    tooltip_shared = tooltip_shared,
    tooltip_round_digit = tooltip_round_digit
  )
}


#' hc_spline
#' @description hc_spline
#' @export
hc_scatter <- function(
    df, xvar, yvar, grp = NULL,
    stack = FALSE,
    chart_title = NULL,
    xaxis_title = NULL,
    yaxis_title = NULL,
    theme = hc_theme_picker("economist"),
    exportable = TRUE,
    regresssion = TRUE,
    regresssion_type = c("linear", "polynomial"),
    regresssion_poly_order = 2,
    regression_line_width = 5,
    regression_line_name = "Trend",
    regression_hide_legend = TRUE,
    tooltip_formatter_instructions = NULL,
    tooltip_shared = FALSE,
    tooltip_round_digit = 2
){
  hc_viz(
    df = df,
    xvar = xvar,
    yvar = yvar,
    grp = grp,
    type = "scatter",
    stack = stack,
    chart_title = chart_title,
    xaxis_title = xaxis_title,
    yaxis_title = yaxis_title,
    theme = theme,
    exportable = exportable,
    regresssion = regresssion,
    regresssion_type = regresssion_type,
    regresssion_poly_order = regresssion_poly_order,
    regression_line_width = regression_line_width,
    regression_line_name = regression_line_name,
    regression_hide_legend = regression_hide_legend,
    tooltip_formatter_instructions = tooltip_formatter_instructions,
    tooltip_shared = tooltip_shared,
    tooltip_round_digit = tooltip_round_digit
  )
}
