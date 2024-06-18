#' Staking Rate Historical Day
#' 
#' Ethereum staking interest rate, from when the Beacon chain was active.

#' @param start character or Date, the starting date for importation.
#' @param end character or Date, the end date for importation.
#' @param api_key character

#' @return tibble
#' 
#' @examples
#' # Api key is required
#' \dontrun{
#' yourapikey <- "yourapikey"
#' cc_eth_staking(start = "2022-01-01", end = "2023-01-01", api_key = yourapikey)
#' cc_eth_staking(start = "2021-01-01", end = "2022-01-01", api_key = yourapikey)
#' cc_eth_staking(start = "2023-01-01", end = "2023-01-19", api_key = yourapikey)
#' }
#' 
#' @details 
#' The Ether staking rate is powered by Attestant, a company dedicated to the business of staking. 
#' They provide a non-custodial Ethereum 2 managed staking service, giving customers the ability to stake 
#' their Ether using Attestant’s infrastructure while always retaining full control of their assets. 
#' You can only use this endpoint with a valid api_key. 
#' The Ether rate is generally updated daily at 11:00 am UTC for the previous day.
#' 
#' @export
#' 
#' @name cc_eth_staking
#' @rdname cc_eth_staking

cc_eth_staking <- function(start = NULL, end = NULL, api_key = NULL){

  symbol <- "ETH"
  # Check "api_key" argument 
  if (missing(api_key) || is.null(api_key)) {
    if (!quiet) {
      msg <- paste0('An "api_key" is required to reach this endpoint!')
      cli::cli_alert_danger(msg)
      return(NULL)
    }
  } 

  # creating end date for daily and hourly data
  if (is.null(end)) {
    end_date <- as_unix(Sys.Date())
    api_path <- c("data", "blockchain", "staking", "latest")
    api_query <- list(fsym = symbol, api_key = api_key)

  } else {
    end_date <- as_unix(as.Date(end))
    api_path <- c("data", "blockchain", "staking", "histoday")
    api_query <- list(fsym = symbol, toTs = end_date, limit = 2000, api_key = api_key)
  }
  
  # GET call 
  response <-  cryptocompare_api(path = api_path, query = api_query)
  if (is.null(end)) {
    response <- response$Data
    output <- dplyr::bind_cols(response)
    output <- dplyr::mutate(output, 
                            Symbol = symbol, 
                            Rate = rate, 
                            Date = as.POSIXct(issued_ts, origin = "1970-01-01"))
    output <- dplyr::select(output, Date, Symbol, Rate)
  } else {
    response <- response$Data$Data
    output <- dplyr::mutate(response, 
                            Symbol = symbol,
                            Date = as.POSIXct(issued_ts, origin = "1970-01-01"))
    output <- dplyr::select(output, Date, Symbol, Rate = "rate")
    output <- dplyr::as_tibble(output)           
  }

  if (!is.null(start)) {
    output <- dplyr::filter(output, Date >= as.Date(start))
  }

  if (!is.null(end)) {
    output <- dplyr::filter(output, Date <= as.Date(end))
  }

  return(output)
}

