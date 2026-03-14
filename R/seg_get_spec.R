#' seg_get_spec
#'
#' @description Reads a segmentation spec Excel workbook (Polars + Profiles
#'   sheets), parses the variable definitions, and optionally executes the spec
#'   syntax to create shell variables in the data.
#'
#' @param seg A seg object with data already loaded via [seg_get_data()].
#' @param spec_path Character. Path to the spec Excel file. If `NULL`, uses the
#'   path stored in `seg[["paths"]][["files"]][["spec"]]` or searches the
#'   working directory.
#' @param execute Logical. If `TRUE` (default), runs [seg_do_spec()] to create
#'   shell variables from the spec syntax.
#' @param execute_debug Logical. If `TRUE`, prints debug info during spec
#'   execution (default: `FALSE`).
#'
#' @return The seg object with `seg[["spec"]]` populated (polars table, profiles
#'   table, shell definitions) and, if `execute = TRUE`,
#'   `seg[["data"]][["with_shell"]]` containing the recoded variables.
#'
#' @export
seg_get_spec <- function(seg, spec_path = NULL, execute = TRUE, execute_debug = FALSE){

  work::start()

  if(is.null(spec_path)){
    spec_path <- seg[["paths"]][["files"]][["spec"]]
    if(is.null(spec_path)){
      spec_path <- find_file_in_dir("spec", "xlsx")
    }
  }

  if(length(spec_path) != 1) stop("More than 1 spec path identified. Please fix.")

  seg[["paths"]][["files"]][["spec"]] <- spec_path %>% normalizePath(mustWork = TRUE)


  #########################
  # internal functions
  #########################

  clean_sheets <- function(spec_path, sheet = c("Polars", "Profiles"), startRow = 3){
    sheet <- match.arg(sheet)

    sheet <- openxlsx::loadWorkbook(spec_path) %>%
      openxlsx::readWorkbook(sheet = sheet, startRow = 3) %>%
      as_tibble() %>%
      select(-Notes) %>%
      names_clean()

    blocks <- sheet %>%
      filter(!is.na(block)) %>%
      select(block, var, label) %>%
      set_names(c("block", "prefix", "block_label"))

    sheet <- sheet %>%
      filter(is.na(block)) %>%
      select(any_of(c("var", "label", "source_var", "syntax", "source_label", "opposite_label", "left_label", "right_label"))) %>%
      mutate(
        syntax = syntax %>%
          stringr::str_squish() %>%
          stringr::str_replace("&gt;", ">") %>%
          stringr::str_replace("&lt;", "<") %>%
          stringr::str_replace_all("&amp;", "&") %>%
          stringr::str_replace(stringr::fixed(",)"), ")"),
        label = label %>%
          stringr::str_squish() %>%
          stringr::str_replace_all("&amp;", "&")
      )


    if("source_label" %in% names(sheet)){
      sheet <- sheet %>%
        mutate(
          source_label = source_label %>%
            stringr::str_squish() %>%
            stringr::str_replace("&amp;", "&")
        )
    }


    if("opposite_label" %in% names(sheet)){
      sheet <- sheet %>%
        mutate(
          opposite_label = opposite_label %>%
            stringr::str_squish() %>%
            stringr::str_replace("&amp;", "&")
        )
    }


    blocks <- blocks %>%
      mutate(
        vars = prefix %>% map(~{
          grep(glue("^{.x}0|^{.x}1|^{.x}2|^{.x}3|^{.x}4|^{.x}5"), sheet$var) %>%
            slice(sheet, .) %>%
            mutate(
              var = var %>% dplyr::replace_values(.x ~ NA)
            )
        })
      )
  }


  seg[["spec"]] <- list(
    "polars" = spec_path %>% clean_sheets("Polars"),
    "profiles" = spec_path %>% clean_sheets("Profiles")
  )



  # check for duplicate var names across polars and profiles
  all_vars <- c(
    bind_rows(seg[["spec"]][["polars"]][["vars"]])[["var"]],
    bind_rows(seg[["spec"]][["profiles"]][["vars"]])[["var"]]
  ) %>% stats::na.omit()

  dup_vars <- all_vars[duplicated(all_vars)] %>% unique()

  if(length(dup_vars) > 0){
    stop(
      glue("Duplicate variable names found in spec: {paste(dup_vars, collapse = ', ')}"),
      call. = FALSE
    )
  }


  #check to see if we need to collapse any inputs
  if(any(grepl(",", bind_rows(seg[["spec"]][["polars"]][["vars"]])[["source_var"]]))){

    spec_profiles <- seg[["spec"]][["polars"]]

    all_coalesce <- list()

    for(j in seq_along(spec_profiles[["vars"]])){
      x <- spec_profiles[["vars"]][[j]]

      if(x[["source_var"]] %>% grepl(",", .) %>% any()){

        source_var_collapsed <- x[["source_var"]] %>% gsub(" ", "", .) %>% gsub(",", "_", .)

        coalesce_instructions <- setNames(
          lapply(x[["source_var"]], function(var_pair) {
            vars <- strsplit(var_pair, ",\\s*")[[1]]
            rlang::expr(coalesce(!!!rlang::syms(vars)))
          }),
          source_var_collapsed
        )

        all_coalesce <- c(all_coalesce, coalesce_instructions)

        x[["syntax"]] <- pmap_chr(
          list(
            x[["source_var"]],
            source_var_collapsed,
            x[["syntax"]]
          ),
          function(x,y,z)gsub(x, y, z)
        )

        x[["source_var"]] <- source_var_collapsed

        spec_profiles[["vars"]][[j]] <- x
      }
    }

    if(length(all_coalesce) > 0){
      seg[["data"]][["original"]] <- seg[["data"]][["original"]] %>%
        mutate(!!!all_coalesce)
    }

    seg[["spec"]][["polars"]] <- spec_profiles
  }



  if(execute){

    if(is.null(seg[["data"]][["original"]])){

      warning("No data. Run get_data first. Will not execute")

    }else if(!is.null(seg[["data"]][["original"]])){

      seg <- seg %>% seg_do_spec(debug = execute_debug)
    }

  }


  return(seg)
}
