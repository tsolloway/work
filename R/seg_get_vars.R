#' seg_get_vars
#' @description seg_get_vars
#' @export
seg_get_vars <- function(
    seg,
    blocks = NULL,
    type = c("both", "polars", "profiles"),
    .return = c("all", "profiles", "rs", "rescales", "sources", "blocks")
){

  type <- match.arg(type)
  .return <- match.arg(.return)

  blocks_polar <- seg %>% seg_get_vars_polars(.return = "blocks")
  blocks_profile <- seg %>% seg_get_vars_profiles(.return = "blocks")

  if(type == "both" && .return == "blocks"){
    return(c(blocks_polar, blocks_profile))
  }else if(type == "polars" && .return == "blocks"){
    return(blocks_polar)
  }else if(type == "profiles" && .return == "blocks"){
    return(blocks_profile)
  }


  if(type == "both"){
    c(
      seg_get_vars_polars(seg = seg, blocks = blocks, .return = .return),
      seg_get_vars_profiles(seg = seg, blocks = blocks, .return = .return),
    )
  }else if(type == "polars"){

    seg_get_vars_polars(seg = seg, blocks = blocks, .return = .return)

  }else if(type == "profiles"){

    seg_get_vars_profiles(seg = seg, blocks = blocks, .return = .return)

  }

}




#' seg_get_vars_polars
#' @description seg_get_vars_polars
#' @export
seg_get_vars_polars <- function(
    seg,
    blocks = NULL,
    .return = c("all", "profiles", "rs", "rescales", "sources", "blocks")
){

  .return <- match.arg(.return)

  all_blocks <- seg[["shell"]][["polars"]] %>%
    dplyr::select(prefix) %>%
    unlist() %>%
    setNames(NULL)

  if(.return == "blocks") return(all_blocks)


  if(is.null(blocks)){

    blocks <- all_blocks

  }else{
    if(!all(blocks %in% all_blocks)) stop("Some blocks not in profiles")
  }



  polar_profiles <- seg[["shell"]][["polars"]] %>%
    dplyr::filter(
      prefix %in% blocks
    ) %>%
    dplyr::select(vars) %>%
    tidyr::unnest(vars) %>%
    dplyr::select(var) %>%
    unlist() %>%
    setNames(NULL)


  polar_table <- seg[["input_sheet"]][["input_table"]] %>%
    dplyr::select(source_var, profile_var, rs_var) %>%
    dplyr::filter(profile_var %in% polar_profiles)


  if(.return == "all"){
    polar_table
  }else{
    switch(
      .return,
      profiles = polar_table %>% dplyr::select(profile_var),
      rs = polar_table %>% dplyr::select(rs_var),
      rescales = polar_table %>% dplyr::select(rs_var),
      sources = polar_table %>% dplyr::select(source_var)
    ) %>%
      unlist() %>%
      setNames(NULL)
  }

}




#' seg_get_vars_profiles
#' @description seg_get_vars_profiles
#' @export
seg_get_vars_profiles <- function(
    seg,
    blocks = NULL,
    .return = c("all", "blocks")
){

  .return <- match.arg(.return)

  all_blocks <- seg[["shell"]][["profiles"]] %>%
    dplyr::select(prefix) %>%
    unlist() %>%
    setNames(NULL)


  if(.return == "blocks") return(all_blocks)


  if(is.null(blocks)){

    blocks <- all_blocks

  }else{
    if(!all(blocks %in% all_blocks)) stop("Some blocks not in profiles")
  }


  seg[["shell"]][["profiles"]] %>%
    dplyr::filter(
      prefix %in% blocks
    ) %>%
    dplyr::select(vars) %>%
    tidyr::unnest(vars) %>%
    dplyr::select(var) %>%
    unlist() %>%
    setNames(NULL)

}




