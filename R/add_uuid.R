#' add_uuid
#' @description add_uuid
#' @export
add_uuid <- function(df, uuid_name = "uuid"){
  df <- df %>%
    rowwise() %>%
    mutate(
      !!uuid_name := uuid::UUIDgenerate(use.time = FALSE)
    ) %>%
    ungroup()

  if(length(unique(df[[uuid_name]])) != nrow(df)){
    stop("'uuid::UUIDgenerate' not creating trully unique ids")
  }

  return(df)
}
