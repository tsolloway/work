#' process_scrape
#' @description Returns logical if all values in vector are unique
#' @param type vector of glassdoor company name.
#' @param company_number vector of glassdoor company number.
#' @param gd_base  vector of glassdoor address base. Default to "https://www.glassdoor.com/Reviews/".
#' @param url_instructions  vector of glassdoor url instructions. Default to "sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
#' @export
process_scrape <- function(type = c("glassdoor"), where = NULL, subtitle = "wilshire-law-firm", ...){
  
  type <- match.arg(type)
  
  
  if( is.null(where) ){
    if( is.null(subtitle) || is.na(subtitle) ){
      path <- glue::glue("{work::get_path('git')}/scrape-{type}")
    }else{
      path <- glue::glue("{work::get_path('git')}/scrape-{type}-{subtitle}")
    }
    
    where <- path
  }
  
  
  where <- where %>% normalizePath(winslash = "/") %>% suppressWarnings()
  
  if( !dir.exists(where) ){
    where %>% dir.create()
  }
  
  
  output <- switch(
    type,
    glassdoor = work::scrape_glassdoor()
  )
  
  
  output[["unique_reviews"]] <- output[["reviews"]]
  
  
  where_files_r <- where %>% list.files(pattern = "\\.rds$", full.names = TRUE)
  
  
  previous_output <- NULL
  if( length(where_files_r) == 1) {
    previous_output <- readRDS(where_files_r)
  }else if( length(where_files_r) > 1) {
    stop("more then one rds file detected in folder.")
  }
  
  
  if(!is.null(previous_output)){
    
    output <- rbind(output, previous_output)
    
    output[["unique_reviews"]][[1]] <- list(
      output[["unique_reviews"]][[1]], 
      output[["unique_reviews"]][[2]]
    ) %>% 
      purrr::list_rbind() %>% dplyr::distinct()
    
  }
  
  
  
  if( is.null(subtitle) || is.na(subtitle) ){
    save_name_r <- glue::glue("{where}/{type}.rds")
    save_name_score <- glue::glue("{where}/{type}-scores.csv")
    save_name_reviews <- glue::glue("{where}/{type}-reviews.csv")
  }else{
    save_name_r <- glue::glue("{where}/{type}-{subtitle}.rds")
    save_name_score <- glue::glue("{where}/{type}-{subtitle}-scores.csv")
    save_name_reviews <- glue::glue("{where}/{type}-{subtitle}-reviews.csv")
  }
  
  
  saveRDS(output, file = save_name_r)
  
  output %>% 
    dplyr::select(date, time, score_overall, score_recommendation_to_friend, score_ceo_approve) %>% 
    write.csv(save_name_score)
  
  output[["unique_reviews"]][[1]] %>% write.csv(save_name_reviews)
  
  return(output)
  
}
