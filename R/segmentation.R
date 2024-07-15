#' seg_init
#' @description seg_init
#' @export
seg_init <- function(
    folder_path = getwd()
){

  result <- list()
  class(result) <- append(class(result), "analytic_segmentation")

  folder_path <- folder_path %>% normalizePath()
  folder_name <- folder_path %>% basename()

  folder_data_path <- glue::glue("{folder_path}/1. Data and Syntax") %>% normalizePath() %>% suppressWarnings()
  folder_process_path <- glue::glue("{folder_path}/2. Specs, Input and FA") %>% normalizePath() %>% suppressWarnings()
  folder_solution_path <- glue::glue("{folder_path}/3. Solutions") %>% normalizePath() %>% suppressWarnings()


  if( dir.exists(folder_data_path) ) unlink(folder_data_path, recursive = TRUE)
  if( dir.exists(folder_process_path) ) unlink(folder_process_path, recursive = TRUE)
  if( dir.exists(folder_solution_path) ) unlink(folder_solution_path, recursive = TRUE)

  dir.create(folder_data_path)
  dir.create(folder_process_path)
  dir.create(folder_solution_path)

  # copy tool templates to folder here

  result[["paths"]] <- list(
    "folders" = list(
      "parent" = folder_path,
      "data" = folder_data_path,
      "process" = folder_process_path,
      "solution" = folder_solution_path
    ),
    "files" = list(
      "data" = NA,
      "spec" = NA,
      "shell" = NA,
      "input" = NA,
      "FA" = NA
    )
  )

  result[["meta"]] <- list(
    "analytic" = "segmentation",
    "folder_name" = folder_name
  )

  return(result)
}


#' seg_get_data
#' @description seg_get_data
#' @export
seg_get_data <- function(seg, data_path, weight = NULL){

  seg[["paths"]][["files"]][["data"]] <- data_path %>% normalizePath(mustWork = TRUE)

  seg[["df"]] <- read_xl(data_path, clean_col_names = FALSE)

  if(!is.null(weight)){
    seg[["meta"]][["weight_variable"]] <- weight
  }

  seg
}


#' seg_get_spec
#' @description seg_get_spec
#' @export
seg_get_spec <- function(seg, spec_path = NULL, exeute = TRUE, execute_debug = FALSE){

  require(openxlsx)
  require(stringr)

  if(is.null(spec_path)){
    spec_path <- find_file_in_dir("spec", "xlsx")
  }

  if(length(spec_path) != 1) stop("More than 1 spec path identified. Please fix.")

  seg[["paths"]][["files"]][["spec"]] <- spec_path %>% normalizePath(mustWork = TRUE)


  #########################
  # internal functions
  #########################

  clean_sheets <- function(spec_path, sheet = c("Polars", "Profiles"), startRow = 3){
    sheet <- match.arg(sheet)

    sheet <- loadWorkbook(spec_path) %>%
      readWorkbook(sheet = sheet, startRow = 3) %>%
      as_tibble() %>%
      select(-Notes) %>%
      names_clean()

    blocks <- sheet %>%
      filter(!is.na(block)) %>%
      select(block, var, label) %>%
      set_names(c("block", "prefix", "block_label"))

    sheet <- sheet %>%
      filter(is.na(block)) %>%
      select(var, label, source_var, syntax) %>%
      mutate(
        syntax = syntax %>%
          str_squish() %>%
          str_replace("&gt;", ">") %>%
          str_replace("&lt;", "<") %>%
          str_replace(fixed(",)"), ")")
      )


    blocks <- blocks %>%
      mutate(
        vars = prefix %>% map(~{
          grep(.x, sheet$var) %>%
            slice(sheet, .) %>%
            mutate(
              var = var %>% case_match(.x~NA, .default = var)
            )

        })
      )
  }


  spec_polars <- spec_path %>% clean_sheets("Polars")
  spec_profiles <- spec_path %>% clean_sheets("Profiles")


  seg[["spec"]] <- list(
    "polars" = spec_path %>% clean_sheets("Polars"),
    "profiles" = spec_path %>% clean_sheets("Profiles")
  )


  if(exeute){
    if(is.null(seg[["df"]])){
      warning("No data. Run get_data first. Will not execute")
    }else if(!is.null(seg[["df"]])){
      seg <- seg %>% seg_do_spec(debug = execute_debug)
    }
  }


  return(seg)
}


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


  seg
}
