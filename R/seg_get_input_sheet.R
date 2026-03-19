#' seg_get_input_sheet
#'
#' @description Reads a completed segmentation input sheet Excel file and
#'   extracts the variable selections for each solution (Source, Profile, RS).
#'
#' @param seg A seg object with path information.
#' @param file_location Character. Path to the input sheet Excel file. Defaults
#'   to `seg[["paths"]][["files"]][["input"]]`.
#' @param row_start Integer. Row where the data table begins in the Inputs sheet
#'   (default: `6`).
#'
#' @return The seg object with `seg[["solutions"]][["names"]]` (solution
#'   letters) and `seg[["solutions"]][["inputs"]]` (list of Source/Profile/RS
#'   variable vectors per solution) populated.
#'
#' @export
seg_get_input_sheet <- function(seg, file_location = NULL, row_start = 6){

  if(is.null(file_location)){
    file_location <- seg[["paths"]][["files"]][["input"]]
  }


  input_table <- openxlsx::readWorkbook(
    file_location, sheet = "Inputs", startRow = row_start
  ) %>%
    dplyr::select(dplyr::any_of(c("Source", "Profile", "RS", LETTERS))) %>%
    tibble::as_tibble() %>%
    janitor::remove_empty("cols")


  solutions_letters <- LETTERS[LETTERS %in% colnames(input_table)]

  input_table <- input_table %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(solutions_letters), ~!is.na(.x)))


  solution_inputs <- purrr::map(
    c("Source", "Profile", "RS") %>% rlang::set_names(., .),
    function(y){
      input_table %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(solutions_letters),
          function(x) ifelse(x, input_table[[y]], NA)
        )) %>%
        dplyr::select(dplyr::any_of(solutions_letters)) %>%
        as.list() %>%
        purrr::map(remove_na)
    }
  ) %>% purrr::transpose()


  seg[["solutions"]][["names"]] <- solutions_letters

  seg[["solutions"]][["inputs"]] <- solution_inputs


  return(seg)

}
