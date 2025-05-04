#' bn_arc_chisq
#' @description bn_arc_chisq
#' @export
bn_arc_chisq <- function(obj, df, dv = NULL, round_to = 4){


  if("bn" %in% class(obj)){
    obj <- obj %>%
      arcs()
  }

  output <- obj %>%

    as_tibble() %>%

    rowwise() %>%

    mutate(

      mi = table(df[[from]], df[[to]]) %>%
        entropy::mi.plugin(),

      test_statistic = bnlearn::ci.test(
        from, to, data = df, test = "mi"
      ) %>%
        pluck("statistic"),

      pval = bnlearn::ci.test(
        from, to, data = df, test = "mi"
      ) %>%
        pluck("p.value")

    ) %>%
    ungroup()



  if(!is.null(round_to) && is.numeric(round_to)){

    output <- output %>%
      mutate(
        pval = pval %>% round(round_to)
      )

  }



  if(!is.null(dv)){

    output <- list(
      "all" = output,
      "dv" = output %>%
        filter(from == dv),
      "ivs" = output %>%
        filter(from != dv)
    )

  }


  return(output)
}


