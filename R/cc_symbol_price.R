#' @title cc_symbol_price
#' @description function for obtaining the last price for a symbol or a vector of symbols.
#' @param symbol A string or character vector containing the symbols of interest. (e.g. "BTC", "ETH", "BNB",...)
#' @param exchange A string or character vector containing the exchange to use, if NULL it will be returned a mean between all the exchanges and denoted with "global".
#' @param currency A string or character vector containing the currency in which convert the output (e.g. "USD", "USDT", "BNB", "EUR",...).
#' @param api_key character
#' @return Return a `tibble` object.
#' @examples
#' # Mean Price for the Pair BTC-USD between the exchanges
#' cc_symbol_price(symbol = "BTC", exchange = NULL, currency = "USD")
#'
#' # Mean Price for the Pair BTC-USD and ETH-USD between the exchanges
#' cc_symbol_price(symbol = c("BTC", "ETH"), exchange = NULL, currency = "USD")
#'
#' # Price for the Pair BTC-USDT, ETH-USDT, BTC-BUSD, ETH-BUSD on Binance
#' cc_symbol_price(symbol = c("BTC", "ETH"),
#'                 exchange = "Binance",
#'                 currency = c("USDT", "BUSD"))
#' @name cc_symbol_price
#' @rdname cc_symbol_price
#' @export

cc_symbol_price <- function(symbol = NULL, exchange = NULL, currency = "USD", api_key = NULL){

  if (is.null(exchange)) {
    out_data <- cc_api_symbol_price(symbol = symbol, currency = currency, exchange = exchange, api_key = api_key)
  } else {
    out_data <- purrr::map_df(exchange, ~cc_api_symbol_price(symbol = symbol, currency = currency, exchange = .x, api_key = api_key))
  }
  return(out_data)
}


# Single Function for comunicating with the API: allow multiple symbols and currencies but not multiple exchanges.
cc_api_symbol_price <- function(symbol = NULL, currency = "USD", exchange = NULL, api_key = NULL){

  # inputs control
  if(is.null(symbol) | is.null(currency)){
    warning("You should provide at least a symbol and a currency.")
    return(NULL)
  }

  # multiple symbols argument
  symbol <- paste0(toupper(symbol), collapse = ",")

  # multiple currency argument
  currency <- paste0(toupper(currency), collapse = ",")

  # Api GET call
  response <- cryptocompare_api(path = c("data", "pricemulti"), query = list(fsyms = symbol, tsyms = currency, e = exchange))

  # create a unique dataset
  out_data <- dplyr::mutate(dplyr::bind_rows(response[[1]]), Symbol = names(response)[1])

  if (length(response) > 1) {
    for(i in 2:length(response)) {
      new_data <- dplyr::mutate(dplyr::bind_rows(response[[i]]), Symbol = names(response)[i])
      out_data <- dplyr::bind_rows(out_data, new_data)
    }
  } 

  # add Date and Exchange 
  out_data <- dplyr::mutate(out_data, Date = Sys.time(), Exchange = ifelse(is.null(exchange), "General", exchange))
  # reorder the columns 
  out_data <- dplyr::select(out_data, Date, Exchange, Symbol, dplyr::everything())

  return(out_data)
}
