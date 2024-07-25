#' table_percent
#' @description table_percent
#' @export
table_percent <- function(x){

  (table(x)/length(x)) %>%
    as.numeric()

}
