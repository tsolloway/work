#' process_dmd
#' @description process_dmd
#' @export
process_dmd <- function(path, sheet, scrub = TRUE){

  work::start(TRUE)

  df <- work::read_xl(path, sheet = sheet)


  df %>% work::col_check(
    c(
      "dol", "date_signed", "date_settled",
      "pre_lit_settlement", "med_pay", "policy_limit",
      "cm", "attorney", "negotiator",
      "case_no"
    )
  )

   df <- df %>%
     dplyr::rename(case = case_no, case_manager = cm) %>%
     dplyr::filter(is.na(lit_settlement))


  if( scrub ){
    df <- df %>% mutate(
      dol = dol %>% as.Date(origin = "1899-12-30"),
      date_signed = date_signed %>% as.Date(origin = "1899-12-30"),
      date_settled = date_settled %>% as.Date(origin = "1899-12-30"),

      settlement_any =  df %>% dplyr::select(pre_lit_settlement, med_pay)  %>% rowSums(na.rm = T),

      policy_limit = policy_limit %>% work::translate_limits(),

      case_manager = case_manager %>% work::translate_names("cm"),
      attorney = attorney %>% work::translate_names("attorney"),
      negotiator = negotiator %>% work::translate_names("negotiator")
    )
  }


  return(df)
}
