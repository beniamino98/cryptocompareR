#' @title cc_blockchain_historical
#' @name cc_blockchain_historical
#' @rdname cc_blockchain_historical
#' 
#' @description Blockchain historical data, such as the number of transactions, active wallets or mining rewards are difficult to retrieve in a readable format. Cryptocompare simplify
#' substantially the procedure of download, aggregate and summaries the data for each day. With this function it is possible to obtain the historical information for most of the blockchain.
#' The data available for the Bitcoin's blockchain starts from 2017. In order to access to this function a `free api key is needed`.
#'
#' @param symbol character, the symbol of interest.
#' @param start character or Date, the start date for importation.
#' @param end character or Date, the end date for importation.
#' @param api_key character
#' 
#' @examples
#' # Api key is required
#' \dontrun{
#' yourapikey <- "yourapikey"
#'
#' cc_blockchain_historical(symbol = "BTC", start = "2017-01-01",
#'                          end = "2023-01-01", api_key = yourapikey)
#'
#' cc_blockchain_historical(symbol = "ETH", start = "2017-01-01",
#'                          end = "2023-01-01", api_key = yourapikey)
#'
#' cc_blockchain_historical(symbol = "DOGE", start = "2017-01-01",
#'                          end = "2023-01-01", api_key = yourapikey)
#' }
#' @return A `tibble`.
#' @export

cc_blockchain_historical <- function(symbol = NULL, start = NULL, end = NULL, api_key = NULL){

  if (is.null(api_key)) {
    warning("Please provide a valid api_key to access to this endpoint.")
    return(NULL)
  }

  # Default Parameters
  end_date <- ifelse(is.null(end), as.character(Sys.Date()), as.character(end))
  end_date <- as.Date(end_date)

  start_date <- ifelse(is.null(start), as.character(Sys.Date() - 2000), as.character(start))
  start_date <- as.Date(start_date)

  # save function to avoid errors
  safe_import <- purrr::safely(cc_api_blockchain_historical)

  # Loop 
  i <- 1
  historical_data <- list()
  last_date <- end_date
  loop_condition <- as.Date(start_date) < as.Date(last_date)
  while (loop_condition) {
    # GET call 
    historical_data[[i]] <- safe_import(symbol = symbol, end = last_date, api_key = api_key)$result
    # Break condition: response is empty 
    break_condition <- is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0
    if (break_condition) {
      break
    }
    # Update last_date (minimum date)
    last_date <- min(historical_data[[i]]$Date)
    # Break condition: last_date is lower or equal than start_date 
    loop_condition <- as.Date(start_date) < as.Date(last_date)
    i <- i + 1
  }
  
  # Output 
  historical_data <- dplyr::bind_rows(historical_data)
  historical_data <- unique(historical_data)
  historical_data <- dplyr::arrange(historical_data, Date)
  historical_data <- dplyr::filter(historical_data, Date >= start_date & Date <= end_date)

  return(historical_data)
}


# Single Api Call 
cc_api_blockchain_historical <- function(symbol = NULL, end = NULL, api_key = NULL){

  # Default Parameters
  end_date <- ifelse(is.null(end), as.character(Sys.Date()), as.character(end))
  end_date <- as.Date(end_date)
  end_date <- as_unix(end_date)

  # Path, Query & Response
  api_path <- c("data", "blockchain", "histo", "day")
  api_query <- list(fsym = symbol, toTs = end_date, limit = 2000, api_key = api_key)
  api_response <- cryptocompare_api(path = api_path, query = api_query)

  if(is.null(api_response)){
    warning("Errors in cyptocompare_api function")
    return(NULL)
  }

  # cleaning
  api_response <- api_response$Data$Data
  # remove duplicated for safety
  api_response <- api_response[!duplicated(api_response),]
  api_response <- dplyr::as_tibble(api_response)          
  # create new variables and convert time index
  api_response <- dplyr::mutate(api_response,
                                Symbol = symbol,
                                Id = id,
                                Date = as.POSIXct(time, origin = "1970-01-01"))

  # reorder and rename the variables
  api_response <- dplyr::select(api_response, Date, Symbol, dplyr::everything())
  api_response <- dplyr::select(api_response, -time, -id, -symbol)
  api_response <- dplyr::mutate(api_response, Date = as.Date(Date))
  api_response <- dplyr::arrange(api_response, Date)

  return(api_response)
}

