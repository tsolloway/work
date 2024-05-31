#' lit_correct_date_filed_mutate
#' @description correct date filed dplyr::mutate
lit_correct_date_filed_mutate <- function(.data){

  .data %>%
    rowwise() %>%
    mutate(

      lit_date_filed = lit_date_filed %>% as.Date(),
      lit_date_complaint_was_filed  = lit_date_complaint_was_filed  %>% as.Date(),
      lit_date_x3p_filed = lit_date_x3p_filed %>% as.Date(),
      lit_date_government_claim_filed = lit_date_government_claim_filed %>% as.Date(),

      date_filed = ifelse(
        all(
          is.na(
            base::c(lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed)
          )
        ),
        as.Date(NA),
        min(
          lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed
          , na.rm = TRUE)
      ) %>% as.Date(),

      date_filed = ifelse(is.infinite(date_filed), NA, date_filed) %>% as.Date()
    ) %>%
    ungroup() %>%
    select(
      - lit_date_filed,
      - lit_date_complaint_was_filed,
      - lit_date_x3p_filed,
      - lit_date_government_claim_filed
    )

}
