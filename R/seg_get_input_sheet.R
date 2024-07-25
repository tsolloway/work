#' seg_get_input_sheet
#' @description seg_get_input_sheet
#' @export
seg_get_input_sheet <- function(seg, file_location = NULL, row_start = 6){

  require(openxlsx)

  if(is.null(file_location)){
    file_location <- seg[["paths"]][["files"]][["input"]]
  }


  input_table <- readWorkbook(
    file_location, sheet = "Inputs", startRow = row_start
  ) %>%
    select(any_of(c("Source", "Profile", "RS", LETTERS))) %>%
    as_tibble() %>%
    janitor::remove_empty("cols")


  solutions_letters <- LETTERS[LETTERS %in% colnames(input_table)]

  input_table <- input_table %>%
    mutate_at(.vars = solutions_letters, ~!is.na(.x))


  solution_inputs <- map(
    c("Source", "Profile", "RS") %>% setNames(., .),
    function(y){
      input_table %>%
        mutate_at(
          .vars = solutions_letters,
          function(x)ifelse(x, input_table[[y]], NA)
        ) %>%
        select(any_of(solutions_letters)) %>%
        as.list() %>%
        map(remove_na)
    }
  ) %>% transpose()


  seg[["solutions"]][["names"]] <- solutions_letters

  seg[["solutions"]][["inputs"]] <- solution_inputs


  return(seg)

}
