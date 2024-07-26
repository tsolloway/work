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
      select(any_of(c("var", "label", "source_var", "syntax", "source_label"))) %>%
      mutate(
        syntax = syntax %>%
          str_squish() %>%
          str_replace("&gt;", ">") %>%
          str_replace("&lt;", "<") %>%
          str_replace(fixed(",)"), ")"),
        label = label %>%
          str_squish() %>%
          str_replace_all("&amp;", "&")
      )


    if("source_label" %in% names(sheet)){
      sheet <- sheet %>%
        mutate(
          source_label = source_label %>%
          str_squish() %>%
          str_replace("&amp;", "&")
        )
    }


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

    if(is.null(seg[["data"]][["original"]])){

      warning("No data. Run get_data first. Will not execute")

    }else if(!is.null(seg[["data"]][["original"]])){

      seg <- seg %>% seg_do_spec(debug = execute_debug)
    }

  }


  return(seg)
}
