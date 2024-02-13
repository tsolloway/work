#' legoland
#' @description Returns legoland passholder codes
#' @param x character
#' @export
legoland <- function(
    x = c("all", "sean", "alina", "tyler", "guestservices", "services")
){

  x <- match.arg(x)


  output <- c(
    "sean" = "305081232670345718",
    "alina" = "305081232626877248",
    "tyler" = "305081232646543635"
  )



  if( x == "all" ){

    return(output)

  }else if( x == "guestservices" || x == "services"){

    return(7607860034)

  }else if( x != "all" && x != "guestservices" && x != "services"){

    return(output[[x]])

  }

}



