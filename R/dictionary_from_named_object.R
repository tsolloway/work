#' dictionary_from_named_object
#' @description dictionary_from_named_object
#' @export
dictionary_from_named_object <- function(dictionary){


  if(is.null(dictionary)){
    return(NULL)
  }


  if(is.data.frame(dictionary) || tibble::is_tibble(dictionary)){

    dictionary <- dictionary %>%
      mutate(
        label = label %>% stringr::str_squish()
      )

    return(dictionary)
  }



  if(is.list(dictionary) && !tibble::is_tibble(dictionary)){
    dictionary <- dictionary %>% map_chr(c)
  }



  if(is.character(dictionary)){

    if(is.null(names(dictionary))){
      names(dictionary) <- dictionary
    }

    dictionary <- tibble(var = names(dictionary), label = dictionary)

  }



  dictionary <- dictionary %>%
    mutate(
      label = label %>% stringr::str_squish()
    )

  return(dictionary)
}
