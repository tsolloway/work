#' lit_get_drops
#' @description queries litify for drop and subout data
#' @export
lit_get_drops <- function(
  start_year = 2021,
  parallel_process = T, add_lead_gen_source = F
){

  work::start(lib_sales_force = TRUE)



  if( parallel_process ){
    start(lib_future = TRUE)

    old_plan <- plan(multisession)

    on.exit(plan(old_plan), add = TRUE)
  }



  df_drops <- function(){

    work::lit_get_data(
      from_object = "litify_pm__Matter__c",
      select_object = c(
        Id, Name, Needles_CaseID__c, litify_pm__Status__c, Display_Name2__c, Litigation_At__c,
        Received_Signed_Agreement__c,  Practice_Area__c,

        Drop_Date__c, Dropped_At__c, Drop_Or_Pending_Drop_At__c,
        Drop_Subout_Date__c, Subout_Date__c, Subout_At__c,
        litify_pm__Closed_Reason__c, Sub_Status__c,

        Lead_Case__c, Lead_Matter__c,
        litify_pm__Companion__r.Lead_Case__c
      ),
      parallel_process = FALSE
    ) %>%
      filter(
        Display_Name2__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
          litify_pm__Status__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
          litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
          litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("duplicate", ignore_case = TRUE), negate = TRUE) |
          !is.na(Received_Signed_Agreement__c)
      ) %>%
      rename_col(
        .select = TRUE,
        .distinct = TRUE,
        id_matter = Id,
        case = Needles_CaseID__c,
        case_name = Display_Name2__c,
        name_matter = Name,

        practice_area = Practice_Area__c,

        source_type = litify_pm__Source_Type__c,
        source_category = litify_pm__Source__r.Category__c,
        marketing_details = Marketing_Details__c,

        date_litigation_at = Litigation_At__c,
        date_signed_agreement = Received_Signed_Agreement__c,

        status = litify_pm__Status__c,
        closed_reason = litify_pm__Closed_Reason__c,


        date_drop_c = Drop_Date__c,
        date_drop_or_pending_drop_c = Drop_Or_Pending_Drop_At__c,
        date_drop_subout_c = Drop_Subout_Date__c,
        date_dropped_at_c = Dropped_At__c,

        sub_status = Sub_Status__c,
        date_subout_at_c = Subout_At__c,
        date_subout_c = Subout_Date__c,

        lead_case = Lead_Case__c,
        lead_matter = Lead_Matter__c,
        companion_lead_case = litify_pm__Companion__r.Lead_Case__c
      ) %>%
      mutate(

        lead_case = lead_case %>% recode("Yes" = TRUE, "No" = FALSE),
        litigation = date_litigation_at %>% is.na() %>% not(),

        practice_area = practice_area %>% recode("Class Action" = "CA", "Employment" = "Employment", "PI" = "PI"),
        practice_area = ifelse(practice_area == "PI" & litigation == TRUE, "PI-Litigation",
                               ifelse(practice_area == "PI" & litigation == FALSE, "PI-Pre-Litigation", practice_area)
        ),

        date_signed_agreement = date_signed_agreement %>% as.Date(),
        date_litigation_at = date_litigation_at %>% as.Date(),

        date_drop_c = date_drop_c %>% as.Date(),
        date_drop_or_pending_drop_c = date_drop_or_pending_drop_c %>% as.Date(),
        date_drop_subout_c = date_drop_subout_c %>% as.Date(),
        date_dropped_at_c = date_dropped_at_c %>% as.Date(),

        date_subout_at_c = date_subout_at_c %>% as.Date(),
        date_subout_c = date_subout_c %>% as.Date(),

        signed_year = date_signed_agreement %>% lubridate::year(),
        signed_month = date_signed_agreement %>% lubridate::month(label = TRUE),

        litigation_start_year = date_litigation_at %>% lubridate::year(),
        litigation_start_month = date_litigation_at %>% lubridate::month(label = TRUE),

        litigation_same_month = (
          (signed_year == litigation_start_year) & (signed_month == litigation_start_month)
        ) %>% work::if_na_return(FALSE),

        closed_reason = closed_reason %>% recode(
          "Dropped" = "dropped",
          "Referred Out" = "referred_out",
          "Settled" = "settled",
          "Subbed-Out" = "subout",
          "Dropped by client" = "dropped",
          "Verdict" = "verdict"
        ),

        status = status %>% recode(
          "Dropped (No Lien)" = "dropped",
          "Dropped" = "dropped",
          "Dropped (Lien )" = "dropped",
          "Dropped In Pro Per (Lien)" = "dropped",
          "Dropped In Pro Per (No Lien)"  = "dropped",
          "Dropped (Lien)"  = "dropped",
          "Dropped MTBR (Lien)"  = "dropped",
          "Dropped MTBR (No Lien)"  = "dropped",

          "Pending Drop (Lien)" = "pending_drop",
          "Pending Drop (No Lien)" = "pending_drop",
          "Pending Drop In Pro Per (Lien)" = "pending_drop",
          "Pending Drop In Pro Per (No Lien) " = "pending_drop",
          "Pending Drop MTBR (Lien)" = "pending_drop",
          "Pending Drop" = "pending_drop",
          "Pending Drop In Pro Per (No Lien)" = "pending_drop",
          "Pending Drop MTBR (No Lien)" = "pending_drop",

          "Sub Out (Lien)" = "subout",
          "Sub Out (No Lien)" = "subout",
          "Subout" = "subout",

          "Pending Sub Out" = "pending_subout",
          "Pending Sub Out (Lien)" = "pending_subout",
          "Pending Sub Out (No Lien)" = "pending_subout",

          "Referral Initiated" = "referral_initiated",
          "Referral Rejected"  = "referral_rejected",
          "Referral Requested" = "referral_requested",
          "Referred Out" = "referred_out",
          "Settled" = "settled",
          "Liens" = "liens",

          "Litigation" = "litigation",
          "Pre-Litigation" = "pre-litigation",
          "Attorney Review" = "attorney_review",
          "Closed" = "closed"
        )
      ) %>%
      rowwise() %>%
      mutate(
        status = ifelse(status == "closed" & !is.na(closed_reason), closed_reason, status)
      ) %>%
      ungroup() %>%
      mutate(
        date_drop = ifelse(status == "dropped", date_drop_subout_c, NA) %>% as.Date(),
        date_subout = date_subout_c,
        date_subout_unauditted = ifelse(status == "subout", date_drop_subout_c, NA) %>% as.Date(),

      ) %>%
      mutate(
        date_drop = ifelse(status == "dropped" & is.na(date_drop), date_dropped_at_c, date_drop) %>% as.Date(),
      ) %>%
      ungroup() %>%
      mutate(
        drop_year = date_drop %>% lubridate::year(),
        drop_month = date_drop %>% lubridate::month(label = TRUE),

        subout_year = date_subout %>% lubridate::year(),
        subout_month = date_subout %>% lubridate::month(label = TRUE),

        drop_same_month = (signed_year == drop_year) & (signed_month == drop_month),
        subout_same_month = (signed_year == subout_year) & (signed_month == subout_month),

        drop_days = (date_drop - date_signed_agreement) %>% as.numeric(),
        subout_days = (date_subout - date_signed_agreement) %>% as.numeric(),

        drop_days_30 = drop_days <= 30,
        drop_days_60 = drop_days <= 60,
        drop_days_90 = drop_days <= 90,
        drop_days_90_180 = drop_days > 90 & drop_days <= 180,
        drop_days_180 = drop_days > 180
      ) %>%
      distinct()
  }



  if( add_lead_gen_source ){

    if( parallel_process ){

      futureAssign("df", df_drops(), seed = NULL)
      futureAssign("sources", lit_get_lead_gen_source(), seed = NULL)

    }else if( !parallel_process ){
      df <- df_drops()
      sources <- lit_get_lead_gen_source()
    }

  }else if( !add_lead_gen_source ){
    df <- df_drops()
  }



  if( add_lead_gen_source ){

    df <- left_join(
      df,
      sources %>% select(
        id_matter,
        matter_source, matter_details,
        intake_source, intake_details,
        source_original, details_original,
        source, details,
        lead_gen_source, sub_lead_gen
      ),
      by = "id_matter",
      relationship = "one-to-one"
    )

  }


  ######################


  drop_summary_month <- function(dfx, ...){

    df_summary <- dfx %>%
      group_by(..., signed_year, signed_month, .drop = FALSE) %>%
      summarise(
        cases_signed = n(),
        drops_signed_month = sum(!is.na(date_drop)),
        subout_signed_month = sum(!is.na(date_subout)),
        lit_cases_signed = litigation_same_month %>% sum(),
        lit_cases_converted = ((litigation_same_month %>% not()) & litigation) %>% sum(),

        drop_days_ave = drop_days %>% mean(na.rm=T),

        drop_days_30_count = drop_days_30 %>% sum(na.rm=T),
        drop_days_60_count = drop_days_60 %>% sum(na.rm=T),
        drop_days_90_count = drop_days_90 %>% sum(na.rm=T),
        drop_days_90_180_count = drop_days_90_180 %>% sum(na.rm=T),
        drop_days_180_count = drop_days_180 %>% sum(na.rm=T),

        drop_days_30_ave = drop_days_30 %>% mean(na.rm=T),
        drop_days_60_ave = drop_days_60 %>% mean(na.rm=T),
        drop_days_90_ave = drop_days_90 %>% mean(na.rm=T),
        drop_days_90_180_ave = drop_days_90_180 %>% mean(na.rm=T),
        drop_days_180_ave = drop_days_180 %>% mean(na.rm=T)
      )  %>%
      suppressMessages() %>%
      ungroup()


    drops_drop_month <- dfx %>%
      filter(
        !is.na(date_drop)
      ) %>%
      group_by(..., drop_year, drop_month, .drop = FALSE) %>%
      summarise(
        drops_drop_month = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    subout_subout_month <- dfx %>%
      filter(
        !is.na(date_subout)
      ) %>%
      group_by(..., subout_year, subout_month, .drop = FALSE) %>%
      summarise(
        subout_subout_month = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    if( length(c(...)) == 0 ){

      summary_drop_sub <- full_join(
        drops_drop_month,
        subout_subout_month,
        by = join_by(
          drop_year == subout_year,
          drop_month == subout_month,
        )
      ) %>%
        mutate(
          drops_drop_month = ifelse(is.na(drops_drop_month), 0, drops_drop_month),
          subout_subout_month = ifelse(is.na(subout_subout_month), 0, subout_subout_month)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          signed_year == drop_year,
          signed_month == drop_month,
        )
      )

    }else if( length(c(...)) == 1 ){

      aditional_args <- c(...)

      summary_drop_sub <- full_join(
        drops_drop_month,
        subout_subout_month,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          drop_year == subout_year,
          drop_month == subout_month,
        )
      ) %>%
        mutate(
          drops_drop_month = ifelse(is.na(drops_drop_month), 0, drops_drop_month),
          subout_subout_month = ifelse(is.na(subout_subout_month), 0, subout_subout_month)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          signed_year == drop_year,
          signed_month == drop_month,
        )
      )

    }else{
      stop("drop join not programmed for, i.e., more than one field in ...")
    }

    df_summary <- df_summary %>%
      select(
        ...,
        signed_year, signed_month,
        cases_signed,
        drops_signed_month, drops_drop_month,
        subout_signed_month, subout_subout_month,

        lit_cases_signed, lit_cases_converted,

        drop_days_30_count, drop_days_60_count, drop_days_90_count,
        drop_days_90_180_count, drop_days_180_count,

        drop_days_ave, drop_days_30_ave, drop_days_60_ave, drop_days_90_ave,
        drop_days_90_180_ave, drop_days_180_ave
      ) %>%
      mutate(
        drops_signed_month_perc = drops_signed_month / cases_signed,
        drops_drop_month_perc = drops_drop_month / cases_signed,
        subout_signed_month_perc = subout_signed_month / cases_signed,
        subout_subout_month_perc = subout_subout_month / cases_signed,
        lit_cases_signed_perc = lit_cases_signed / cases_signed,
        lit_cases_converted_perc = lit_cases_signed / cases_signed,

        drop_days_30_perc = drop_days_30_count / cases_signed,
        drop_days_60_perc = drop_days_60_count / cases_signed,
        drop_days_90_perc = drop_days_90_count / cases_signed,
        drop_days_90_180_perc = drop_days_90_180_count / cases_signed,
        drop_days_180_perc = drop_days_180_count / cases_signed
      )

    df_summary[is.na(df_summary)] <- NA
    df_summary[df_summary == Inf] <- NA


    max_date <- dfx %>%
      select(date_signed_agreement, date_drop, date_subout) %>%
      unlist() %>%
      max(na.rm = T) %>%
      as.Date()


    max_date_year <- max_date %>% lubridate::year()
    max_date_month <- max_date %>% lubridate::month()


    df_summary <- df_summary %>%
      mutate(
        drop = paste0(signed_year, signed_month) %in% paste0(max_date_year, month.abb[seq(max_date_month+1, 12)])
      ) %>%
      filter(!drop) %>%
      select(-drop)


    df_summary <- df_summary %>%
      arrange(..., signed_year, signed_month)


    return(df_summary)
  }


  drop_summary_year <- function(dfx, ...){

    df_summary <- dfx %>%
      group_by(..., signed_year, .drop = FALSE) %>%
      summarise(
        cases_signed = n(),
        drops_signed_year = sum(!is.na(date_drop)),
        subout_signed_year = sum(!is.na(date_subout)),
        drop_days_ave = drop_days %>% mean(na.rm=T),
      )  %>%
      suppressMessages() %>%
      ungroup()


    drops_drop_year <- dfx %>%
      filter(
        !is.na(date_drop)
      ) %>%
      group_by(..., drop_year, .drop = FALSE) %>%
      summarise(
        drops_drop_year = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    subout_subout_year <- dfx %>%
      filter(
        !is.na(date_subout)
      ) %>%
      group_by(..., subout_year, .drop = FALSE) %>%
      summarise(
        subout_subout_year = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    if( length(c(...)) == 0 ){

      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          signed_year == drop_year
        )
      )

    }else if( length(c(...)) == 1 ){

      aditional_args <- c(...)


      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          signed_year == drop_year
        )
      )

    }else if( length(c(...)) == 2 ){

      aditional_args <- c(...)

      aditional_args_1 <- aditional_args[[1]]
      aditional_args_2 <- aditional_args[[2]]

      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          signed_year == drop_year
        )
      )

    }else{
      stop("drop join not programmed for, i.e., more than one field in ...")
    }

    # df_summary <-
    df_summary %>%
      select(
        ...,
        signed_year,
        cases_signed,
        drops_signed_year, drops_drop_year,
        subout_signed_year, subout_subout_year
      ) %>%
      mutate(
        cases_signed = ifelse(is.na(cases_signed), 0, cases_signed),
        drops_signed_year = ifelse(is.na(drops_signed_year), 0, drops_signed_year),
        drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
        subout_signed_year = ifelse(is.na(subout_signed_year), 0, subout_signed_year),
        subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year),

        drops_signed_year_perc = drops_signed_year / cases_signed,
        drops_drop_year_perc = drops_drop_year / cases_signed,
        subout_signed_year_perc = subout_signed_year / cases_signed,
        subout_subout_year_perc = subout_subout_year / cases_signed
      )

    df_summary[is.na(df_summary)] <- NA
    df_summary[df_summary == Inf] <- NA


    df_summary <- df_summary %>%
      arrange(..., signed_year)


    return(df_summary)
  }


  drop_summary <- function(dfx, ..., .by_month = TRUE){
    if(.by_month){
      drop_summary_month(dfx, ...)
    }else if(!.by_month){
      drop_summary_year(dfx, ...)
    }
  }


  ######################


  results <- list(
    df = df,
    summaries = list(),
    functions = list(
      drop_summary_month = drop_summary_month,
      drop_summary_year = drop_summary_year,
      drop_summary = drop_summary
    )
  )


  ######################


  results[["summaries"]][["pi_drop"]] <- df %>%
    filter(
      signed_year >= start_year,
      lead_case,
      practice_area %in% c("PI-Litigation", "PI-Pre-Litigation")
    ) %>%
    drop_summary()



  results[["summaries"]][["pi_drop_all"]] <-
    df %>%
    filter(
      signed_year >= start_year,
      practice_area %in% c("PI-Litigation", "PI-Pre-Litigation")
    ) %>%
    drop_summary()



  if( add_lead_gen_source ){

    results[["summaries"]][["lead_gen_source"]] <-
      df %>%
      filter(
        signed_year >= start_year,
        lead_case,
        practice_area %in% c("PI-Litigation", "PI-Pre-Litigation"),
        !is.na(lead_gen_source)
      ) %>%
      drop_summary(lead_gen_source, .by_month = FALSE) %>%
      arrange(signed_year) %>%
      tidyr::pivot_wider(
        names_from = 'signed_year',
        values_from = c(cases_signed, drops_signed_year, drops_drop_year),
        id_cols = "lead_gen_source",
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source)



    results[["summaries"]][["lead_gen_source_all"]] <-
      df %>%
      filter(
        signed_year >= 2021,
        practice_area %in% c("PI-Litigation", "PI-Pre-Litigation"),
        !is.na(lead_gen_source)
      ) %>%
      drop_summary(lead_gen_source, .by_month = FALSE) %>%
      arrange(signed_year) %>%
      tidyr::pivot_wider(
        names_from = 'signed_year',
        values_from = c(cases_signed, drops_signed_year, drops_drop_year),
        id_cols = "lead_gen_source",
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source)



    results[["summaries"]][["lead_gen_and_sub_source"]] <-
      df %>%
      filter(
        signed_year >= start_year,
        lead_case,
        practice_area %in% c("PI-Litigation", "PI-Pre-Litigation"),
        !is.na(lead_gen_source)
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, .by_month = FALSE) %>%
      arrange(signed_year) %>%
      tidyr::pivot_wider(
        names_from = 'signed_year',
        values_from = c(cases_signed, drops_signed_year, drops_drop_year),
        id_cols = c(lead_gen_source, sub_lead_gen),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen)



    results[["summaries"]][["lead_gen_and_sub_source_all"]] <-
      df %>%
      filter(
        signed_year >= start_year,
        practice_area %in% c("PI-Litigation", "PI-Pre-Litigation"),
        !is.na(lead_gen_source)
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, .by_month = FALSE) %>%
      arrange(signed_year) %>%
      tidyr::pivot_wider(
        names_from = 'signed_year',
        values_from = c(cases_signed, drops_signed_year, drops_drop_year),
        id_cols = c(lead_gen_source, sub_lead_gen),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen)

  }



  return(results)
}

