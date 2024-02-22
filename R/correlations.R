#' correlations
#' @description correlations
#' @export
correlations <- function(
    df, dvs = NULL, ivs = NULL, cluster = TRUE,
    hclust_method = c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
    r_round = 2, p_round = 3, add_visualization = TRUE){

  hclust_method <- match.arg(hclust_method)

  output <- list()


  if( !is.null(dvs) || !is.null(ivs) ){
    df <- df %>% dplyr::select(dplyr::all_of(c(dvs, ivs)))
  }

  df <- df %>% dplyr::select_if(is.numeric)


  cor_table <- df %>% as.matrix() %>% Hmisc::rcorr()

  if( cluster ){

    # cluster the correlation matrix to reorder it / make it more readable.

    cluster = as.dist(1 - cor_table[["r"]]) %>% stats::hclust(method = hclust_method)

    cluster_order <- cluster[["order"]]

    if( !is.null(dvs) ){
      cluster_order <- colnames(cor_table[["r"]])[cluster_order]
      cluster_order <- c(dvs, setdiff(cluster_order, dvs))
      cluster_order <- match(colnames(cor_table[["r"]]), cluster_order)
    }

    cor_table[["r"]] = cor_table[["r"]][cluster_order, cluster_order]
    cor_table[["n"]] = cor_table[["n"]][cluster_order, cluster_order]
    cor_table[["P"]] = cor_table[["P"]][cluster_order, cluster_order]
  }


  output[["r"]] <- cor_table[["r"]] %>% round(r_round)
  output[["p"]] <- cor_table[["P"]] %>% round(p_round)
  output[["n"]] <- cor_table[["n"]]


  output[["academic"]] <- output[["p"]] %>% car::recode("lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*'; 0.05:0.1='^'; else=''")
  output[["academic"]] <- paste0(output[["r"]], output[["academic"]]) %>% matrix(nrow = nrow(output[["r"]])) %>% data.frame()

  colnames(output[["academic"]]) <- colnames(output[["r"]])
  rownames(output[["academic"]]) <- rownames(output[["r"]])


  if( add_visualization ){

    cor_table[["r"]] <- output[["r"]]
    output <- output %>% purrr::map(tibble::as_tibble)

    output[["plot"]] <- corrplot::corrplot.mixed(cor_table[["r"]])
    output[["network"]] <- cor_table[["r"]] %>% corrr::network_plot(min_cor = .2)

  }else{
    output <- output %>% purrr::map(tibble::as_tibble)
  }

  return(output)
}


