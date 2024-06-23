
#' dt_viz_data
#' @description dt_viz_data
#' @export
dt_viz_data <- function(
    df,
    col_factor = NULL,
    col_round = NULL, col_round_digits = 2,
    col_percent = NULL, col_percent_digits = 2,
    style = "bootstrap4",
    hnds_width = NULL, hnds_heigth = NULL
){

  require(DT)

  if(!is.null(col_factor)){
    for( i in col_factor ) {
     df[[i]] <- df[[i]] %>% as.factor()
    }
  }


  dt <- df %>%
    datatable(
      filter = 'top',
      style = style,
      fillContainer = T,
      extensions = c('Scroller', "Buttons"),
      list(dom = 'Bfrtip',
           scrollY = "100%",
           scrollX = "100%",
           scroller = FALSE,
           paging = FALSE
      )
    )


  if(!is.null(col_round)){
    dt <- dt %>%
      formatRound(col_round, digits = col_round_digits)
  }


  if(!is.null(col_percent)){
    dt <- dt %>%
      formatPercentage(col_percent, digits = col_percent_digits)
  }


  return(dt)
}


