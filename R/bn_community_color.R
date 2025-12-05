#' bn_community_color
#'
#' @description
#' Generates a tibble mapping community group numbers to colors for network visualization.
#' Supports both small (<=8) and large (>8) numbers of communities.
#'
#' @param grp_max Integer or numeric vector. Either the total number of communities or
#' a vector of group labels.
#' @param na_rm Logical (default = FALSE). If TRUE, `NA` values are ignored when determining
#' the maximum group number.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{group}{Group number from 1 to the maximum number of communities.}
#'   \item{color}{Hex color code assigned to that group.}
#' }
#'
#' @details
#' For small numbers of groups (<= 8), the function uses the discrete Set2 palette from `RColorBrewer`.
#' For 9–20 groups, it interpolates Set2. For more than 20 groups, it uses `scales::hue_pal()` for better distinction.
#'
#' @examples
#' bn_community_color(5)
#' bn_community_color(c(1, 2, 3, 4, 5, 6))
#' bn_community_color(25) # works well for large networks
#'
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble tibble
#' @importFrom scales hue_pal
bn_community_color <- function(grp_max, na_rm = FALSE) {

  # Determine max group number if input is a vector
  if(length(grp_max) > 1){
    grp_max <- max(grp_max, na.rm = na_rm)
  }

  # Choose palette
  if(grp_max <= 8){
    pal <- RColorBrewer::brewer.pal(8, "Set2")[1:grp_max]
  } else if(grp_max <= 20){
    pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(grp_max)
  } else {
    pal <- scales::hue_pal()(grp_max)
  }

  # Construct tibble
  dplyr::tibble(
    group = seq_len(grp_max),
    color = pal
  )
}
