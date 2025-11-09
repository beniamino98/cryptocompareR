#' @title as_unix
#' @description This function convert a Date into Unix format: an integer denoting the number of seconds from the origin date, usually "1970-01-01".
#' @param x character or Date, in the format "yyyy-mm-dd" or "dd-mm-yyyy". In alternative it is possible to specify the Date also with the time in the format "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss"
#' @param milliseconds logical, if TRUE the output will be converted in milliseconds, otherwise it will be in seconds.
#' @param origin character denoting the origin date.
#' @examples
#' as_unix("2023-01-01")
#' as_unix("01-01-2023")
#' as_unix("2023-01-01 10:23:23")
#' as_unix("01-01-2023 10:23:23")
#' @importFrom lubridate ymd dmy ymd_hms dmy_hms
#' @name as_unix
#' @rdname as_unix
#' @return integer character 
#' @export

as_unix  <- function(x = NULL, milliseconds = FALSE, origin = "1970-01-01") {

  conversion <- NULL

  # Check if it is already Numeric
  if(is.numeric(x)) {

    warning("x is already an integer")

    return(x)
  }

  # try different formats
  conversion <- as.character(lubridate::ymd(x, quiet = TRUE))
  conversion <- ifelse(is.na(conversion), as.character(lubridate::dmy(x, quiet = TRUE)), conversion)
  conversion <- ifelse(is.na(conversion), as.character(lubridate::ymd_hms(x, quiet = TRUE)), conversion)
  conversion <- ifelse(is.na(conversion), as.character(lubridate::dmy_hms(x, quiet = TRUE)), conversion)

  if(is.na(conversion)){

    return(NA_integer_)

  }

  # Standard conversion to posixct
  conversion <- as.POSIXct(conversion, origin = origin)

  # conversion in milliseconds
  if (milliseconds){
    conversion <- as.integer(conversion)*1000
  } else {
    conversion <- as.integer(conversion)
  }

  conversion <- format(conversion, scientific = FALSE)

  return(conversion)

}
