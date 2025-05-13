#' bn_community_color
#' @description bn_community_color
#' @export
bn_community_color <- function(grp_max, na_rm = FALSE){


  if(is.vector(grp_max) || is.array(is.vector(grp_max)) || length(grp_max) > 1){
    grp_max <- max(grp_max, na.rm = na_rm)
  }


  pal <- colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))(grp_max)


  pal <- tibble(
    group = grp_max %>% seq() %>% as.numeric(),
    color = pal
  )


  return(pal)
}
