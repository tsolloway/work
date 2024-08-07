#' seg_do_spec
#' @description seg_do_spec
#' @export
seg_do_spec <- function(seg, debug = FALSE){

  if(is.null(seg[["data"]][["original"]])){
    stop("No data. Run get_data first.")

  }else if(!is.null(seg[["data"]][["original"]])){

    df <- seg[["data"]][["original"]]
  }


  if(is.null(seg[["spec"]][["polars"]]) || is.null(seg[["spec"]][["profiles"]])){

    stop("No specs Run get_spec first.")

  }else if(!is.null(seg[["spec"]][["polars"]]) && !is.null(seg[["spec"]][["profiles"]])){

    spec_polars <- seg[["spec"]][["polars"]]

    spec_profiles <- seg[["spec"]][["profiles"]]
  }


  if(!is.null(seg[["meta"]][["weight_variable"]])){
    stop("Weighting not programmed.  Fix this before doing this seg please.")
  }



  #########################
  # internal functions
  #########################

  execute_syntax <- function(df, spec_type, debug = FALSE){

    x <- spec_type %>% tidyr::unnest(cols = vars)

    x <- x %>%
      select(syntax) %>%
      unlist() %>%
      set_names(
        x %>% select(var) %>% unlist()
      ) %>%
      remove_empty()

    if(debug){
      for(i in seq(length(x))){
        tryCatch({
          df <- df %>%
            rowwise() %>%
            mutate(
              !!!rlang::parse_exprs(x[i])
            )
        }, error=function(e){
          print(i)
          print(names(x)[i])
          print(x[i])
        })
      }
    }

    df %>%
      rowwise() %>%
      mutate(
        !!!rlang::parse_exprs(x)
      ) %>%
      ungroup()
  }


  spec_shell <- function(df, spec_type){
    spec_type %>%
      tidyr::unnest(col=vars) %>%
      filter(!is.na(var)) %>%
      select(-syntax, -source_var) %>%
      tidyr::nest(.by=c("block", "prefix", "block_label"), .key = "vars")
  }


  df <- df %>%
    execute_syntax(spec_polars, debug) %>%
    execute_syntax(spec_profiles, debug)


  if(!debug){

    inputs <- spec_polars %>%
      tidyr::unnest(col = vars) %>%
      mutate(
        rs_var = glue("rs_{source_var}")
      ) %>%
      rename_col(
        .select = T,
        source_var = source_var,
        profile_var = var,
        rs_var = rs_var,
        label = label,
        source_label = source_label,
        right_label = right_label,
        left_label = left_label
      )


    df <- df %>% mutate(
      across(
        .cols = inputs[["source_var"]],
        .fns = ~ .x %>% case_match(1 ~ -4, 2 ~ -2, 3 ~ 2, 4 ~ 4),
        .names = "rs_{.col}"
      )
    )

    seg[["data"]][["with_shell"]] <- df
    seg[["shell"]][["polars"]] <- df %>% spec_shell(spec_polars)
    seg[["shell"]][["profiles"]] <- df %>% spec_shell(spec_profiles)
    seg[["spec"]][["polars_table"]] <- inputs

  }

  return(seg)
}
