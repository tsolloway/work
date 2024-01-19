#' scrape_glassdoor
#' @description Returns logical if all values in vector are unique
#' @param company character vector of glassdoor company name.
#' @param company_number character vector of glassdoor company number.
#' @param gd_base  character vector of glassdoor address base. Default to "https://www.glassdoor.com/Reviews/".
#' @param url_instructions  character vector of glassdoor url instructions. Default to "sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
#' @export
scrape_glassdoor <- function(
    company = "Wilshrie Law Firm",
    company_number = "E799887",
    gd_base = "https://www.glassdoor.com/Reviews/",
    url_instructions = "sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng",
    sleep_page = 2
  ){

  require(rvest)

  company <- company %>% stringr::str_squish() %>% stringr::str_trim() %>% gsub(" ", "-", .)

  if( tail(gd_base, 1) != "/" ) gd_base <- glue::glue("{gd_base}/")

  url_base <- glue::glue("{gd_base}{company}-Reviews-{company_number}.html")


  con <- url(url_base, "rb")
  page <- con %>% rvest::read_html()


  review_count <- page %>% html_elements(".paginationFooter") %>% html_text2() %>% strsplit(" ") %>%
    unlist() %>% as.integer(stringr::str_extract(., "\\d+")) %>% max(na.rm = T) %>% suppressWarnings()


  page_count <- ceiling(review_count/10)


  url_pages <- purrr::map(seq(page_count), ~glue::glue("{gd_base}{company}-Reviews-{company_number}_P{.x}.html?{url_instructions}"))


  score_overall <- page %>% html_elements(".v2__EIReviewsRatingsStylesV2__ratingNum") %>% html_text2() %>% unique() %>% as.numeric()
  score_rec <- page %>% html_elements("#EmpStats_Recommend") %>% html_text2() %>% gsub("%", "", .) %>% as.numeric()
  score_approve <- page %>% html_elements("#EmpStats_Approve") %>% html_text2() %>% gsub("%", "", .) %>% as.numeric()


  reviews <- purrr::map(url_pages, ~{

    Sys.sleep(sleep_page)
    
    con <- url(.x, "rb")

    x <- con %>% rvest::read_html() %>% html_elements(".empReviews") %>% html_text2()

    dplyr::tibble(
      "rating" = x %>% stringr::str_extract_all("\\d+\\.0") %>% unlist(),
      "date" = x %>% stringr::str_extract_all("\\b\\w{3} \\d{1,2}, \\d{4}\\b") %>% unlist(),
      "pro" = x %>% stringr::str_extract_all("Pros\\n\\n([^\\n]+)\\n\\n") %>%
        unlist() %>% gsub("(Pros\n\n|\n)", "", .) %>% gsub('\"', "'", .) %>%
        gsub('\r', "", .) %>% gsub(" ,", ",", .) %>% stringr::str_squish() %>%
        stringr::str_trim(),
      "con" = x %>% stringr::str_extract_all("Cons\\n\\n([^\\n]+)\\n\\n") %>%
        unlist() %>% gsub("(Cons\n\n|\n)", "", .) %>% gsub('\"', "'", .) %>%
        gsub('\r', "", .) %>% gsub(" ,", ",", .) %>% stringr::str_squish() %>%
        stringr::str_trim()
    )

  }) %>% purrr::list_rbind() %>% dplyr::distinct()


  reviews[["date"]] <- reviews[["date"]] %>% lubridate::mdy()


  output <- tibble::tibble(
    date = Sys.Date(),
    time = Sys.time(),
    score_overall = score_overall,
    score_recommendation_to_friend = score_rec,
    score_ceo_approve = score_approve,
    reviews = list(reviews)
  )


  return(output)
}


