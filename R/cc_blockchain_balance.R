#' @title cc_blockchain_balance
#' @name cc_blockchain_balance
#' @rdname cc_blockchain_balance
#' 
#' @description historical balance for Bitcoin blockchain only. The output will be a tibble containing the total Bitcoin held and the number of wallet by different amounts range.
#' In order to access to this endpoint a `free api key is needed`.
#'
#' @param from character or Date, the start date for importation.
#' @param to character or Date, the end date for importation.
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
#' cc_blockchain_balance(from = "2017-01-01",
#'                       to = "2023-01-01",
#'                       api_key = yourapikey)
#'}
#' @return tibble
#' @export

cc_blockchain_balance <- function(from = NULL, to = NULL, api_key = NULL){

  # Check "api_key" argument 
  check_api_key <- missing(api_key) || is.null(api_key)
  if (check_api_key) {
    msg <- "Access Denited: provide a valid api_key to access to this endpoint."
    cli::cli_alert_danger(msg)
    return(NULL)
  }
  
  
  # Check "to" argument 
  check_to <- missing(to) || is.null(to)
  if (check_to) {
    to_date <- Sys.Date()
  } else {
    to_date <- as.Date(to)
  }
  
  # Check "from" argument 
  check_from <- missing(from) || is.null(from)
  if (check_from) {
    from_date <- Sys.Date() - 2000
  } else {
    from_date <- as.Date(from)
  }
  
  i <- 1
  historical_data <- list()
  # Safe function to avoid errors
  safe_import <- purrr::safely(cc_api_blockchain_balance)  
  last_date <- to_date
  loop_condition <- as.Date(from_date) < as.Date(last_date)
  while(loop_condition) {

    historical_data[[i]] <- safe_import(to = last_date, api_key = api_key)$result

    # Break condition
    break_condition <- is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0
    if (break_condition) {
      break
    } else {
      # Update last date 
      last_date <- historical_data[[i]]$date[nrow(historical_data[[i]])]
    }
    # Check loop condition 
    loop_condition <- as.Date(from_date) < as.Date(last_date)
    i <- i + 1
  }

  historical_data <- dplyr::bind_rows(historical_data)
  historical_data <- unique(historical_data)
  # Filter to be exactly in the ["from"-"to"] date range
  idx_date <- historical_data$date >= from_date & historical_data$date <= to_date
  historical_data <- historical_data[idx_date,]
  # Arrange with respect to "Date" column (ascending)
  idx_order <- order(historical_data$date, decreasing = FALSE)
  historical_data <- historical_data[idx_order,]

  return(historical_data)
}

cc_api_blockchain_balance <- function(to = NULL, api_key = NULL){

  ## actually this endpoint is available only for BTC
  symbol <- "BTC"
  api_path <- c("data", "blockchain", "balancedistribution")
  
  # API path and query
  if (is.null(to)) {
    api_path <- c(api_path, "latest")
    api_query <- list(fsym = symbol, api_key = api_key)
    to_date <- NULL
  } else {
    to_date <- as_unix(as.Date(to))
    api_path <- c(api_path, "histo", "day")
    api_query <- list(fsym = symbol, toTs = to_date, limit = 2000, api_key = api_key)
  }
  
  api_response <- cryptocompare_api(path = api_path, query = api_query)
  
  # warning message
  check_empty <- purrr::is_empty(api_response)
  if (check_empty) {
    cli::cli_alert_danger("Error in GET call, response is empty")
    return(NULL)
  }
  
  # Create date variable 
  if (is.null(to_date)) {
    api_response <- api_response$Data$balance_distribution
    api_response$date <- Sys.Date()
  } else {
    api_response <- api_response$Data$Data
    api_response <- purrr::map2_df(api_response$balance_distribution,
                                   api_response$time, ~dplyr::bind_cols(.x, date = as.POSIXct(.y, origin = "1970-01-01")))
  }
  
  # Convert to tibble 
  api_response <- dplyr::as_tibble(api_response)
  api_response$symbol <- symbol
  # Reorder and rename columns
  colnames(api_response) <- c("to", "from", "amount", "wallets", "date", "symbol")
  idx_columns <- c("date", "symbol", "from", "to", "amount", "wallets")
  api_response <- api_response[,idx_columns]
  # Arrange by date (ascending)
  idx_order <- order(api_response$date, decreasing = FALSE)
  api_response <- api_response[idx_order,]

  return(api_response)
}
