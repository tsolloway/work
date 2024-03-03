#' join_dmd_to_lit
#' @description join_dmd_to_lit
#' @export
join_dmd_to_lit <- function(df){

  work::start(TRUE)

  lit_dmd <- df %>% dplyr::select(case) %>% work::lit_get_dmd()

  dmd_cases_not_found <- unique(unlist(df$case))[ ! unique(unlist(df$case)) %in% unique(unlist(lit_dmd$case)) ]



  df <- df %>% dplyr::full_join(lit_dmd, by = "case", suffix = c("", "_lit"))


  resolutions <- df %>% select(case, resolution_data) %>% tidyr::unnest(resolution_data, names_sep = "_resolution") %>%
    dplyr::rename(
      resolution_amount = resolution_data_resolutionresolution_amount,
      date_resolution = resolution_data_resolutiondate_resolution,
      resolution_group = resolution_data_resolutionresolution_group,
      resolution_type = resolution_data_resolutionresolution_type,
      resolution_link = resolution_data_resolutionlink_resolution
    ) %>% select(
      case, resolution_amount, date_resolution,
      resolution_group, resolution_type, resolution_link
    ) %>%
    distinct()



  df_split <- df %>% split(seq(nrow(df)))



  df_split <- df_split %>% map(~{

    data_names <- .x %>% names()

    x <- left_join(.x, resolutions,
                   by = dplyr::join_by(
                     case == case,
                     settlement_any == resolution_amount,
                     date_settled == date_resolution
                   ),
                   keep = T
    )

    x <- x %>% rename(case = case.x) %>% select(-case.y)


    if( nrow(x) > 1 ){
      x[-1, data_names] <- NA
    }else if( nrow(x) == 1 && is.na(x[["resolution_amount"]]) ){
      y <- resolutions %>% filter(case == x[["case"]]) %>% select(-case)

      if( nrow(y) == 1){

        x[, names(y)] <- y

      }else if( nrow(y) > 1){

        y <- y %>% slice(
          (y$date_resolution - x$date_settled) %>% abs() %>% rank(ties.method = "first")
        )

        x <- x %>% work::add_NA_rows( (nrow(y) - 1) )

        x[, names(y)] <- y

      }
    }

    x
  })



  df <- df_split %>% bind_rows() %>%
    mutate(
      name_check = purrr::map2(client_name, client_name_lit, ~{

        ys <- .y %>% strsplit(" ") %>% unlist()
        yh <- ys %>% head(1)
        yt <- ys %>% tail(1)

        grepl(yh, .x, ignore.case = T) && grepl(yt, .x, ignore.case = T)

      }) %>% unlist() %>% work::if_na_return(),

      date_incident_check = (dol == date_incident) %>%  work::if_na_return(),
      ammount_check = (settlement_any == resolution_amount) %>%  work::if_na_return(),
      limit_check = (policy_limit == policy_limit_lit) %>%  work::if_na_return(),
      date_signed_check = (date_signed == date_agreement_signed) %>%  work::if_na_return(),
      date_resolution_check = (date_settled == date_resolution) %>%  work::if_na_return(),
      case_manager_check = (case_manager == case_manager_lit) %>%  work::if_na_return(),
      attorney_check = (attorney == pre_lit_attorney) %>%  work::if_na_return(),
      negotiator_check = (negotiator == negotiator_lit) %>%  work::if_na_return()
    )



  df <- df %>%
    select(
      case, client_name, dol, pre_lit_settlement, policy_limit,
      med_pay, total_settlement, date_signed, date_settled, case_manager,
      attorney, negotiator, row, settlement_any,

      name_check, date_incident_check, ammount_check, limit_check, date_signed_check,
      date_resolution_check, case_manager_check, attorney_check, negotiator_check,

      client_name_lit, practice_area, case_type, policy_limit_lit,
      date_incident, date_agreement_signed, date_filed, type_of_accident,
      severity, commercial, government, source_from_intake,  source_from_matter,
      case_manager_lit, attorney_lit, negotiator_lit,
      pre_lit_attorney, litigation_attorney, principal_attorney,
      resolution_count,

      resolution_amount, date_resolution, resolution_group, resolution_type, resolution_link
    )



  df[
    is.na(df$case),
    c(
      "name_check", "date_incident_check", "ammount_check", "limit_check", "date_signed_check",
      "date_resolution_check", "case_manager_check", "attorney_check", "negotiator_check"
    )
  ] <- NA



  if( length(dmd_cases_not_found) > 0 ){
    return(
      list(
        df = df,
        dmd_cases_not_found = dmd_cases_not_found
      )
    )
  }else{
    return(df)
  }

}




