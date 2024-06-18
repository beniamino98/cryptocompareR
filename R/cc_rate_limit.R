#' Cryptocompare Rate limit
#' 
#' Find out how many calls you have left in the current month, day, hour, minute and second
#' 
#' @param hour Logical. Default is `FALSE`. If `TRUE` will return the number of calls done and left in the current hour. 
#' 
#' @usage 
#' cc_rate_limit(hour = FALSE)
#' 
#' @examples
#' 
#' cc_rate_limit(hour = FALSE)
#' cc_rate_limit(hour = TRUE)
#' 
#' @export
#' 
#' @rdname cc_rate_limit
#' @name cc_rate_limit

 cc_rate_limit <- function(hour = FALSE) {
   
   if (hour) {
     # GET call 
     response <- cryptocompare_api(path = c("stats", "rate", "hour", "limit"), query = NULL)
     # Output 
     calls_made <- dplyr::bind_cols(calls = "Made", dplyr::bind_rows(response$CallsMade))
     calls_left <- dplyr::bind_cols(calls = "Left", dplyr::bind_rows(response$CallsLeft))
     output <- dplyr::bind_rows(calls_made, calls_left)
   } else {
     # GET call 
     response <- cryptocompare_api(path = c("stats", "rate", "limit"), query = NULL)$Data
     # Output 
     calls_made <- dplyr::bind_cols(calls = "Made", dplyr::bind_rows(response$calls_made))
     calls_left <- dplyr::bind_cols(calls = "Left", dplyr::bind_rows(response$calls_left))
     output <- dplyr::bind_rows(calls_made, calls_left)
   }
    
   return(response)
}
 
