# library(RSelenium)
#
# url <- "https://www.glassdoor.com/Reviews/Wilshire-Law-Firm-Reviews-E799887.htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
#
# url_base <- "https://www.glassdoor.com/Reviews/Wilshire-Law-Firm-Reviews-E799887.htm"
#
# get_elements <- function(my_client, value, split_pattern, using = "class name", col_names = NULL){
#
#   my_client$findElements(
#     using = using,
#     value = value
#   ) %>% purrr::map(~{
#     .x$getElementText() %>%
#       unlist() %>%
#       strsplit(split_pattern) %>%
#       unlist()
#   }) %>%
#     do.call(rbind, .) %>%
#     data.frame() %>%
#     stats::setNames( col_names ) %>%
#     tibble::tibble()
# }
#
#
#
#
# rD <- RSelenium::rsDriver(browser="firefox")
# remDr <- rD[["client"]]
#
#
#
# remDr$open()
# remDr$navigate(url_base)
#
# # foo <- get_and_wait_for_element(remDr, value = "SignInButton", using = "id") %>% unlist()
# # foo$clickElement()
# #
#
# remDr$setTimeout(type = "page load", milliseconds=20000)
# remDr$setTimeout(type = "implicit", milliseconds=20000)
#
#
# remDr$findElement(using = "id", value = "SignInButton")$clickElement()
#
# remDr$findElement(using = "id", value = "fishbowlCoRegEmail")$sendKeysToElement(list("tsolloway@wilshirelawfirm.com", key = "enter"))
#
# remDr$findElement(using = "id", value = "fishbowlCoRegPassword")$sendKeysToElement(list("abcdefg12#", key = "enter"))
#
# remDr$findElement(using = "class name", value = "CloseButton")$clickElement()
#
# remDr$refresh()
#
#
# # # click firm rating modal
# # remDr$findElement(using = "class name", value = "mb-md-md")$clickElement()
# #
# # temp <- ""
# # cnt <- 1
# # while(temp == ""){
# #   cnt <- cnt + 1
# #   Sys.sleep(.1)
# #   temp <- remDr$findElement(using = "id", value = "reviewDetailsModal")$getElementText()
# #   if(cnt == 10000) stop("modal never showed")
# # }; rm(temp, cnt)
#
#
# scores <- list(
#   get_elements(
#     my_client = remDr,
#     value = "categoryRating",
#     split_pattern = "\\n([^\\n]+)\\n",
#     col_names = c("type", "score")
#   ),
#   get_elements(
#     my_client = remDr,
#     value = "css-ye96g2",
#     split_pattern = "%\\n",
#     col_names = c("score", "type")
#   )
# ) %>% purrr::list_rbind()
#
#
# scores[["score"]] <- scores[["score"]] %>% as.numeric()
#
#
# remDr$findElement(using = "class name", value = "modal_closeIcon")$clickElement()
#
#
# remDr$findElements(using = "class name", value = "expand-button__expand-button-module__ExpandButton") %>% purrr::walk(~{.x$clickElement()})
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# reviews <- remDr$findElements(
#   using = "class name",
#   value = "review-details__review-details-module__reviewDetails"
# ) %>% purrr::map(~{
#
#   x <- .x$getElementText() %>%
#     unlist() %>% strsplit("\\n") %>% unlist() %>% stringr::str_squish()
#
#   helpful_count <- x %>% tail(3) %>% head(1) %>% as.numeric() %>% suppressWarnings()
#
#   if( !is.numeric(helpful_count) ){
#     helpful_count <- NA
#   }
#
#   pro <- x[
#     dplyr::between(
#       seq(999), match("pros", tolower(x)) + 1, match("cons", tolower(x)) - 1
#     ) %>% which()
#   ] %>% paste0(collapse = " //n ") %>% stringr::str_squish()
#
#
#   con <- x[
#     dplyr::between(
#       seq(999), match("cons", tolower(x)) + 1, match("advice to management", tolower(x)) - 1
#     ) %>% which()
#   ] %>% paste0(collapse = " //n ")  %>% stringr::str_squish()
#
#
#   if( con == "" ){
#     if( !is.na(helpful_count) ){
#
#       con <- x[
#         dplyr::between(
#           seq(999), match("cons", tolower(x)) + 1, length(x) - 3
#         ) %>% which()
#       ] %>% paste0(collapse = " //n ")  %>% stringr::str_squish()
#
#     }else{
#
#       con <- x[
#         dplyr::between(
#           seq(999), match("cons", tolower(x)) + 1, length(x) - 2
#         ) %>% which()
#       ] %>% paste0(collapse = " //n ")  %>% stringr::str_squish()
#     }
#   }
#
#   advice_to_management <- match("advice to management", tolower(x))
#
#   if( !is.na(advice_to_management) ){
#     if( !is.na(helpful_count) ){
#
#       advice_to_management <- x[
#         dplyr::between(
#           seq(999), advice_to_management + 1, length(x) - 3
#         ) %>% which()
#       ] %>% paste0(collapse = " //n ")  %>% stringr::str_squish()
#
#     }else{
#       advice_to_management <- x[
#         dplyr::between(
#           seq(999), advice_to_management + 1, length(x) - 2
#         ) %>% which()
#       ] %>% paste0(collapse = " //n ")  %>% stringr::str_squish()
#     }
#
#   }else if( is.na(advice_to_management) ){
#     advice_to_management <- ""
#   }
#
#
#   data.frame(
#     rating = x[1],
#     date = x[2],
#     title = x[3],
#     reported_role = x[4],
#     experience = x[5],
#     location = x[6],
#     pro = pro,
#     con = con,
#     advice_to_management = advice_to_management,
#     helpful_count = helpful_count
#   )
#
# }) %>% purrr::list_rbind()
#
#
#
# remDr$findElements(
#   using = "class name",
#   value = value
# )
#
# #
