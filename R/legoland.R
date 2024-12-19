#' legoland
#' @description Returns legoland passholder codes
#' @param x character
#' @export
legoland <- function(
    x = c("all", "sean", "alina", "tyler", "guestservices", "services")
){

  x <- match.arg(x)


  output <- c(
    "sean" =  "305081229156563376",
    "mia" =   "308050130652355432",
    "alina" = "305081229161716186",
    "tyler" = "305081229110083017"
  )



  if( x == "all" ){

    return(output)

  }else if( x == "guestservices" || x == "services"){

    return(7607860034)

  }else if( x != "all" && x != "guestservices" && x != "services"){

    return(output[[x]])

  }

}



