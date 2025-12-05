#' add_uuid
#'
#' @description Adds a UUID column to a data frame.
#' @param df A data frame or tibble.
#' @param uuid_name Name of the UUID column to add (default: "uuid").
#' @export
add_uuid <- function(df, uuid_name = "uuid") {
  n <- nrow(df)
  uuids <- uuid::UUIDgenerate(n = n, use.time = FALSE)

  if (length(unique(uuids)) != n) {
    stop("UUIDs are not unique. Consider retrying.")
  }

  df[[uuid_name]] <- uuids
  df
}
