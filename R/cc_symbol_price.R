#' Symbols Price
#' 
#' Get the current price of any cryptocurrency in any other currency that you need.
#' 
#' @param symbol Character vector. The cryptocurrency symbol of interest, e.g. `"BTC"`, `"ETH"`, `"BNB"`. 
#' Default is `NULL`. 
#' 
#' @param exchange Character vector. The exchange to obtain data from. 
#' If `NULL`, the default, will be returned a global mean between all the exchanges.
#' 
#' @param currency Character vector. Currency symbols to convert into, e.g. `"USD"`, `"USDT"`, `"BNB"`, `"EUR"`.
#' If `NULL`, the default, will be used `"USD"`. 
#' 
#' @param api_key character
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' cc_symbol_price(symbol = NULL, 
#'                 exchange = NULL, 
#'                 currency = "USD", 
#'                 api_key = NULL)
#' 
#' @return Return a `tibble`. The number of columns will depends on the lenght of `currency` parameter.  
#' There are 3 columns always present: 
#' - `date`: \code{\link[=POSIXt-class]{POSIXt}}, time of the snapshot.
#' - `exchange`: Character, reference exchange. If `exchange` is `NULL` will be classified as `Global`. 
#' - `symbol`: Character, tcryptocurrency symbol of interest. 
#' 
#' @details If the crypto does not trade directly into the `symbol` requested, `BTC` will be used for conversion. 
#' If the opposite pair trades we invert it, e.g. BTC-XMR. 
#' 
#' @examples
#' # Mean price for BTCUSD between all the exchanges
#' cc_symbol_price(symbol = "BTC", exchange = NULL, currency = "USD")
#'
#' # Mean price for BTCUSD and ETHUSD between all the exchanges
#' cc_symbol_price(symbol = c("BTC", "ETH"), exchange = NULL, currency = "USD")
#'
#' # Mean price for BTCUSDT, ETHUSDT on Binance
#' cc_symbol_price(symbol = c("BTC", "ETH"),
#'                 exchange = "Binance",
#'                 currency = c("USDT"))
#'                 
#' @export
#'                 
#' @name cc_symbol_price
#' @rdname cc_symbol_price

cc_symbol_price <- function(symbol, currency, exchange = NULL, api_key = NULL, quiet = FALSE){
  
  # Check "symbol" argument 
  if (missing(symbol) || is.null(symbol)) {
    if (!quiet) {
      wrn <- paste0('The "symbol" argument is missing with no default argument.')
      cli::cli_abort(wrn)
    }
  } else {
    symbol <- toupper(symbol)
  }
  
  # Check "currency" argument 
  if (missing(currency) || is.null(currency)) {
    currency <- "USD"
    if (!quiet) {
      wrn <- paste0('The "currency" argument is missing, default is ', '"', currency, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    currency <- toupper(currency)
  }
  
  # Multiple exchanges are implemented 
  check_exchange <- is.null(exchange)
  if (check_exchange) {
    out_data <- cc_api_symbol_price(symbol = symbol, 
                                    currency = currency, 
                                    exchange = NULL, 
                                    api_key = api_key, 
                                    quiet = quiet)
  } else {
    out_data <- purrr::map_df(exchange,
                              ~cc_api_symbol_price(symbol = symbol, 
                                                   currency = currency, 
                                                   exchange = .x, 
                                                   api_key = api_key, 
                                                   quiet = quiet))
  }
  
  return(out_data)
}


# API Function: allow for multiple symbols and currencies but not for multiple exchanges
cc_api_symbol_price <- function(symbol = NULL, currency = "USD", exchange = NULL, api_key = NULL, quiet = FALSE){

  # Check "symbol" and "currency" arguments 
  if (is.null(symbol) | is.null(currency)) {
    if (!quiet) {
      msg <- paste0("Provide at least a `symbol` and a `currency`.")
      cli::cli_alert_warning(msg)
    }
    return(NULL)
  }

  # Create multiple symbols argument 
  symbol <- paste0(toupper(symbol), collapse = ",")
  # Create multiple currency argument 
  currency <- paste0(toupper(currency), collapse = ",")
  # GET call
  response <- cryptocompare_api(path = c("data", "pricemulti"), 
                                query = list(fsyms = symbol, tsyms = currency, e = exchange))
  # Output
  output <- dplyr::bind_rows(response[[1]])
  output$symbol <- names(response)[1]
  if (length(response) > 1) {
    for(i in 2:length(response)) {
      new_data <- dplyr::bind_rows(response[[i]])
      new_data$symbol <- names(response)[i]
      output <- dplyr::bind_rows(output, new_data)
    }
  } 
  # Add `date` column
  output$date <- Sys.time()
  # Add `exchange` column
  output$exchange <- ifelse(is.null(exchange), "Global", exchange)
  # Reorder columns 
  output <- dplyr::select(output, date, exchange, symbol, dplyr::everything())

  return(data)
}
