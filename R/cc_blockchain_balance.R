#' @title cc_blockchain_balance
#' @name cc_blockchain_balance
#' @rdname cc_blockchain_balance
#' 
#' @description historical balance for Bitcoin blockchain only. The output will be a tibble containing the total Bitcoin held and the number of wallet by different amounts range.
#' In order to access to this endpoint a `free api key is needed`.
#'
#' @param start character or Date, the start date for importation.
#' @param end character or Date, the end date for importation.
#' @param api_key character
#' 
#' @details The output of the function will be a tibble with 6 columns:
#' \itemize{
#'  \item `Date`: the observation date.
#'  \item `Symbol`: the symbol of interest.
#'  \item `From`: the minimum amount of Bitcoin held by some wallets.
#'  \item `To`: the maximum amount of Bitcoin held by some wallets.
#'  \item `Amount`: the total number of Bitcoin held by wallets having a quantity of Bitcoin between From and To.
#'  \item `Wallets`: the total number of Wallets having a quantity of Bitcoin between From and To.
#' }
#' @examples
#'
#' # Api key is required
#' \dontrun{
#' yourapikey <- "yourapikey"
#' cc_blockchain_balance(start = "2017-01-01",
#'                       end = "2023-01-01",
#'                       api_key = yourapikey)
#'}
#' @return tibble
#' @export

cc_blockchain_balance <- function(start = NULL, end = NULL, api_key = NULL){

  if (is.null(api_key)) {
    warning("Access Denited: provide a valid api_key to access to this endpoint.")
    return(NULL)
  }
  
  # default end date 
  if (is.null(end)) {
    end_date <- Sys.Date()
  } else {
    end_date <- as.Date(end)
  }
  
  # default start date 
  if (is.null(end)) {
    start_date <- Sys.Date()-2000
  } else {
    start_date <- as.Date(start)
  }

  # Loop to get all the data
  i <- 1
  last_date <- end_date
  historical_data <- list()
  safe_import <- purrr::safely(cc_api_blockchain_balance)  # save version to avoid errors
  
  while (as.Date(start_date) < as.Date(last_date)) {

    historical_data[[i]] <- safe_import(end = last_date, api_key = api_key)$result

    # break control
    if (is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0) {
      break
    }

    last_date <- historical_data[[i]]$Date[nrow(historical_data[[i]])]
    i <- i + 1
  }

  historical_data <- dplyr::bind_rows(historical_data)
  historical_data <- unique(historical_data)
  historical_data <- dplyr::arrange(historical_data, Date)

  # if selected a range filter since we import always 2000
  historical_data <- dplyr::filter(historical_data, Date >= start_date & Date <= end_date )

  return(historical_data)
}

# Single Api Call version
cc_api_blockchain_balance <- function(end = NULL, api_key = NULL){

  # actually this endpoint is available only for BTC
  symbol <- "BTC"
  api_path <- c("data", "blockchain", "balancedistribution")
  
  # Path, Query & Response
  if(is.null(end)){
    api_path <- c(api_path, "latest")
    api_query <- list(fsym = symbol, api_key = api_key)
    api_response <- cryptocompare_api(path = api_path, query = api_query)
  } else {
    end_date <- as_unix(as.Date(end))
    api_path <- c(api_path, "histo", "day")
    api_query <- list(fsym = symbol, toTs = end_date, limit = 2000, api_key = api_key)
    api_response <- cryptocompare_api(path = api_path, query = api_query)
  }

  # warning message
  if(is.null(api_response)){
    warning("Some error in the importation, the response is NULL")
    return(NULL)
  }

  if(is.null(end_date)){
    api_response <- api_response$Data$balance_distribution
    # create new variables and convert time index
    api_response <- dplyr::mutate(api_response, Date = Sys.Date())

  } else {
    api_response <- api_response$Data$Data
    api_response <- purrr::map2_df(api_response$balance_distribution,
                                   api_response$time, ~dplyr::bind_cols(.x, Date = as.POSIXct(.y, origin = "1970-01-01")) )

  }

  # final conversion to tibble
  api_response <- dplyr::as_tibble(api_response)

  # create new column for the symbol
  api_response <- dplyr::mutate(api_response, Symbol = symbol)

  # reorder and rename the columns
  api_response <- dplyr::select(api_response, Date, Symbol, From = "from", To = "to", 
                                Amount = "totalVolume", Wallets = "addressesCount" )
  # arrange the Date (descending)
  api_response <- dplyr::arrange(api_response, Date)

  return(api_response)

}
