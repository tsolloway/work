#' coefficient_lda
#' @description coefficient_lda
#' @export
coefficient_lda <- function(fit, input = NULL, grp = NULL){


  cov_within <- function(input, grp){

    n <- nrow(input)
    glevs <- levels(grp)
    ng <- nlevels(grp)
    Within <- matrix(0, ncol(input), ncol(input))

    for (k in 1:ng) {
      tmp <- grp == glevs[k]
      nk <- sum(tmp)

      Wk <- ((nk - 1)/(n - ng)) * var(input[tmp, ])

      Within <- Within + Wk
    }

    Within
  }

  prior <- fit[["prior"]]

  ng <- prior %>% length()

  GM <- fit[["means"]]


  if(is.null(input)){
    input <- fit[["call"]] %>% paste0() %>% tail(2) %>% head(1) %>% parse(text = .) %>% eval.parent()
  }


  if(is.null(grp)){
    grp <- fit[["call"]] %>% paste0() %>% tail(1) %>% parse(text = .) %>% eval.parent()
  }


  if(!is.factor(grp)){
    grp <- grp %>% as.factor()
  }


  W_inv <- cov_within(input, grp) %>% solve()

  cons <- rep(0, ng)

  Betas <- matrix(0, nrow(W_inv), ng)


  for (k in 1:ng) {
    cons[k] <- (-1/2) * GM[k, ] %*% W_inv %*% GM[k, ] + log(prior[k])
    Betas[, k] <- t(GM[k, ]) %*% W_inv
  }


  result <- rbind(cons, Betas) %>%
    as.data.frame() %>%
    setNames(glue::glue("seg_{names(prior)}")) %>%
    dplyr::mutate(
      variable = c("constant", colnames(GM))
    ) %>%
    dplyr::relocate(variable, .before = 1) %>%
    tibble::as_tibble()


  result

}
