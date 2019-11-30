#' @title unix_date
#' @description a function that convert a date in unix format, from unix to date, or from date/unix to unix in milliseconds.
#' @param x a character vector
#' @param from if:
#'        - true (default): conversion from unix to date
#'        - false         : conversion from date to unix
#' @param milliseconds if:
#'        - true          : conversion in milliseconds
#'
#'@name unix_date
#'@rdname unix_date
#'@return date converted from or to unix format / milliseconds
#'
#' @examples
#'  unix_date("2019-11-22 19:20:05", from = FALSE)
#'  unix_date( "2019-04-01" , from = FALSE, milliseconds = FALSE)
#'  unix_date( "2019-04-01" , from = FALSE, milliseconds = TRUE)
#'  unix_date("1574446805",from = TRUE, milliseconds = TRUE)
#'  unix_date("1574446805",from = TRUE)
#'
#' @export

unix_date <- function(x, from = TRUE, milliseconds = FALSE) {

  conv_unix <- vector("character", 1)
  if ( purrr::is_empty(x) ) { return(NA) }

  # from unix to date
  if ( from ){

    if ( lubridate::is.Date(x)) { return(as.character(x)) }

    if ( milliseconds ){

      return(format( as.integer(x)*1000, scientific = FALSE ) )

    }

    conv_unix <- as.POSIXct( as.integer(x), origin = "1970-01-01")
    conv_unix <- as.character(conv_unix)

  } else {

    # from date to unix
    if ( is.integer(x)) { return(as.character(x)) }

    conv_unix <- format_date(x)
    conv_unix <- as.POSIXct(conv_unix, origin = "1970-01-01")
    conv_unix <- format(as.integer(conv_unix), scientific = F )

    if ( milliseconds ){

    conv_unix <- format(as.integer(conv_unix)*1000, scientific = F)

    }

  }

  return(as.character(conv_unix))

}


#' @title format_date
#' @description a function for formatting a date format %d-%m-%Y
#' @param x a date
#' @name format_date
#' @rdname format_date
#' @return data in character format %Y-%m-%d
#' @examples
#'
#'  format_date("22-11-2019")
#'  format_date("2019-11-22 19:20:05")
#'
#'@export

format_date <- function(x){

  try.ls <- list(
    dmy.try = lubridate::ymd(x, quiet = T),
    ymd.try = lubridate::dmy(x, quiet = T),
    pos.try = as.POSIXct(as.Date(x), origin = "1970-01-01")
  )

  try.out <- try.ls[ !is.na(try.ls) ] [[1]]

  if( purrr::is_empty(try.out)){

    return(NA)

  } else {

    return(try.out)

  }

}

