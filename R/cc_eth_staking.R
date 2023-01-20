
#' @title cc_eth_staking
#' @description Ethereum staking interest rate, from when the Beacon chain was active. In order to access to this endpoint a `free api key is needed`.
#' @param start character or Date, the starting date for importation.
#' @param end character or Date, the end date for importation.
#' @param api_key character
#' @return tibble
#' @examples
#'
#' # Api key is required
#'
#' yourapikey <- "yourapikey"
#'
#' cc_eth_staking(start = "2022-01-01", end = "2023-01-01", api_key = yourapikey)
#' cc_eth_staking(start = "2021-01-01", end = "2022-01-01", api_key = yourapikey)
#' cc_eth_staking(start = "2023-01-01", end = "2023-01-19", api_key = yourapikey)
#'
#' @name cc_eth_staking
#' @rdname cc_eth_staking
#' @export

cc_eth_staking <- function(start = NULL, end = NULL, api_key = NULL){

  symbol = "ETH"

  # Stop message
  if(is.null(api_key)){
    warning("Provide a valid api_key to access to this endpoint!")
    return(NULL)
  }

  # creating end date for daily and hourly data
  if(is.null(end)){

    end_date <- as_unix(Sys.Date())
    api_path <- c("data", "blockchain", "staking", "latest")
    api_query <- list(fsym = symbol, api_key = api_key)

  } else {
    end_date <- as_unix(as.Date(end))
    api_path <- c("data", "blockchain", "staking", "histoday")
    api_query <- list(fsym = symbol, toTs = end_date, limit = 2000, api_key = api_key)
  }

  api_response <-  cryptocompare_api(path = api_path, query = api_query)

  if(is.null(end)){

    # cleaning
    api_response <- api_response$Data
    api_response <- dplyr::bind_cols(api_response)   # conversion to tibble

    # create new variables and convert time index
    api_response <- dplyr::mutate(api_response, Symbol = symbol, Rate = rate,  Date = as.POSIXct(issued_ts, origin = "1970-01-01"))
    api_response <- dplyr::select(api_response, Date, Symbol, Rate)

  } else {

    # cleaning
    api_response <- api_response$Data$Data
    api_response <- dplyr::mutate(api_response, Symbol = symbol,
                                  Date = as.POSIXct(issued_ts, origin = "1970-01-01"))

    api_response <- dplyr::select(api_response, Date, Symbol, Rate = "rate")
    api_response <- dplyr::as_tibble(api_response)           # conversion to tibble

  }

  if(!is.null(start)){
    api_response = dplyr::filter(api_response, Date >= as.Date(start))
  }

  if(!is.null(end)){
    api_response = dplyr::filter(api_response, Date <= as.Date(end))
  }

  return(api_response)

}

