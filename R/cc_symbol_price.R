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
#' cc_symbol_price(symbol = c("BTC", "ETH"), exchange = NULL, currency = c"USD")
#'
#' # Price for the Pair BTC-USDT, ETH-USDT, BTC-BUSD, ETH-BUSD on Binance
#' cc_symbol_price(symbol = c("BTC", "ETH"),
#'                 exchange = "Binance",
#'                 currency = c("USDT", "BUSD"))
#' @name cc_symbol_price
#' @rdname cc_symbol_price
#' @export

cc_symbol_price <- function(symbol = NULL, exchange = NULL, currency = "USD", api_key = NULL){

  if(is.null(exchange)){

    cc_api_symbol_price(symbol = symbol, currency = currency, exchange = exchange, api_key = api_key)

  } else {

    purrr::map_df(exchange, ~cc_api_symbol_price(symbol = symbol, currency = currency, exchange = .x, api_key = api_key))

  }
}


# Single Function for comunicating with the API: allow multiple symbols and currencies but not multiple exchanges.
cc_api_symbol_price <- function(symbol = NULL, currency = "USD", exchange = NULL, api_key = NULL){

  # control on inputs
  if(is.null(symbol) | is.null(currency)){
    warning("At least one symbol and one currency must be inserted.")
    return(NULL)
  }

  # create multiple symbols argument
  symbol <- toupper(symbol)
  symbol <- paste0(symbol, collapse = ",")

  # create multiple currency argument
  currency <- toupper(currency)
  currency <- paste0(currency, collapse = ",")

  # get the content
  html_content <- cryptocompare_api(path = c("data", "pricemulti"), query = list(fsyms = symbol, tsyms = currency, e = exchange))

  # save the importation time
  importation_time <- Sys.time()

  # create a unique dataset
  html_data <- dplyr::mutate(dplyr::bind_rows(html_content[[1]]), Symbol = names(html_content)[1])

  if(length(html_content) > 1){

    for(i in 2:length(html_content)){

      html_data <- dplyr::bind_rows(html_data, dplyr::mutate(dplyr::bind_rows(html_content[[i]]), Symbol = names(html_content)[i]))

    }

  }

  # adding Time and Exchange and reordering the variables
  html_data <- dplyr::mutate(html_data, Date = importation_time, Exchange = ifelse(is.null(exchange), "General", exchange))
  html_data <- dplyr::select(html_data, Date, Exchange, Symbol, dplyr::everything())

  return(html_data)
}
