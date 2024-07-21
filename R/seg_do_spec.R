#' seg_do_spec
#' @description seg_do_spec
#' @export
seg_do_spec <- function(seg, debug = FALSE){

  if(is.null(seg[["df"]])){
    stop("No data. Run get_data first.")
  }else if(!is.null(seg[["df"]])){
    df <- seg[["df"]]
  }

  if(is.null(seg[["spec"]][["polars"]]) || is.null(seg[["spec"]][["profiles"]])){
    stop("No specs Run get_spec first.")
  }else if(!is.null(seg[["spec"]][["polars"]]) && !is.null(seg[["spec"]][["profiles"]])){
    spec_polars <- seg[["spec"]][["polars"]]
    spec_profiles <- seg[["spec"]][["profiles"]]
  }

  if(!is.null(seg[["meta"]][["weight_variable"]])) stop("Weighting not programmed.  Fix this before doing this seg please.")


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
        factor_var = glue("rs_factor_{source_var}")
      ) %>%
      rename_col(
        .select = T,
        source_var = source_var,
        profile_var = var,
        factor_var = factor_var,
        label = label
      )


    df <- df %>% mutate(
      across(
        .cols = inputs[["source_var"]],
        .fns = ~ .x %>% case_match(1 ~ -4, 2 ~ -2, 3 ~ 2, 4 ~ 4),
        .names = "rs_factor_{.col}"
      )
    )

    seg[["df"]] <- df
    seg[["shell"]][["polars"]] <- df %>% spec_shell(spec_polars)
    seg[["shell"]][["profiles"]] <- df %>% spec_shell(spec_profiles)
    seg[["input_table"]] <- inputs

  }

  return(seg)
}
