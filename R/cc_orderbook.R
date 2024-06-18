#' Exchanges With Order Book Data
#' 
#' Returns all the exchanges we have order book level 2 data. 
#' 
#' @param api_key Character, mandatory. 
#' 
#' @usage 
#' cc_orderbook_exchange(api_key = NULL)
#' 
#' @details 
#' It is possible to access to this endpoint with an `api_key`. 
#' 
#' @export
#' 
#' @rdname cc_orderbook_exchange
#' @name cc_orderbook_exchange

cc_orderbook_exchange <- function(api_key = NULL){

  # Check "api_key" argument 
  if (missing(api_key) || is.null(api_key)) {
    if (!quiet) {
      msg <- paste0('An "api_key" is required to reach this endpoint!')
      cli::cli_alert_danger(msg)
      return(NULL)
    }
  } 
  
  # GET call
  api_paths <- c("data", "ob", "exchanges")
  api_query <- list(api_key = api_key)
  response <- cryptocompare_api(path = api_paths, query = api_query)
  # Output data
  data_out <- purrr::map(response$Data, ~.x$orderBookInternalName)
  data_out <- dplyr::tibble(exchange = names(data_out), internal_name = unlist(data_out))
  
  return(data_out)
}

#' Order Book L1 Top
#' 
#' Returns latest order book Level 1 bid/ask values for the specified symbol, currency and exchange.
#' 
#' @param symbol Character, 
#' @param currency Character
#' @param exchange Character
#' @param api_key Character, mandatory. 
#' @param quiet Logical
#' 
#' @usage 
#' cc_L1_order_book(symbol, 
#'                  currency, 
#'                  exchange = NULL, 
#'                  api_key = NULL, 
#'                  quiet = FALSE)
#' 
#' @details 
#' It is possible to access to this endpoint with an `api_key`. 
#' 
#' @export
#' 
#' @rdname cc_L1_order_book
#' @name cc_L1_order_book

cc_L1_order_book <- function(symbol, currency, exchange = NULL, api_key = NULL, quiet = FALSE){

  # Check "symbol" argument 
  if (missing(symbol) || is.null(symbol)) {
    if (!quiet) {
      msg <- paste0('The "symbol" argument is missing with no default argument.')
      cli::cli_abort(msg)
    }
  } else {
    symbol <- toupper(symbol)
  }
  
  # Check "currency" argument 
  if (missing(currency) || is.null(currency)) {
    currency <- "USD"
    if (!quiet) {
      msg <- paste0('The "currency" argument is missing, default is ', '"', currency, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    currency <- toupper(currency)
  }
  
  # Check "api_key" argument 
  if (missing(api_key) || is.null(api_key)) {
    if (!quiet) {
      msg <- paste0('An "api_key" is required to reach this endpoint!')
      cli::cli_alert_danger(msg)
      return(NULL)
    }
  } 
  
  # GET call
  api_paths <- c("data", "ob", "l1", "top")
  api_query <- list(fsyms = paste0(symbol, collapse = ","), tsyms = paste0(currency, collapse = ","), e = exchange, api_key = api_key)
  response <- cryptocompare_api(path = api_paths, query = api_query)
  if (response$Response == "Error") {
    cli::cli_alert_danger(response$Message)
    return(NULL)
  }
  # Output data
  data_out <- purrr::map2_df(response$Data$RAW[[1]], currency, ~dplyr::bind_cols(currency = .y, dplyr::bind_cols(.x) ))
  data_out <- dplyr::bind_cols(data_out, symbol = symbol[1])
  if (length(symbol) > 1) {
    for(i in 2:length(response$Data$RAW)) {
      data_new <- purrr::map2_df(response$Data$RAW[[i]], currency, ~dplyr::bind_cols(currency = .y, dplyr::bind_cols(.x) ))
      data_new <- dplyr::bind_cols(data_out, symbol = symbol[i])
      data_out <- dplyr::bind_rows(data_out, data_new)
    }
  }
  data_out <- dplyr::mutate(data_out, exchange = exchange, date = Sys.time(), spread = ASK - BID)
  data_out <- dplyr::select(data_out, date, symbol, currency, exchange, bid = "BID", ask = "ASK", spread)
  
  return(data_out)
}


#' Order Book L2 Snapshot
#' 
#' Returns latest order book Level 2 data snapshot for the requested exchange.
#' 
#' @param api_key Character, mandatory. 
#' 
#' @usage 
#' cc_order_book_L2(api_key = NULL)
#' 
#' @details 
#' It is possible to access to this endpoint with an `api_key`. 
#' 
#' @export
#' 
#' @rdname cc_order_book_L2
#' @name cc_order_book_L2

cc_order_book_L2 <- function(api_key = NULL){
  
  # Check "api_key" argument 
  if (missing(api_key) || is.null(api_key)) {
    if (!quiet) {
      msg <- paste0('An "api_key" is required to reach this endpoint!')
      cli::cli_alert_danger(msg)
      return(NULL)
    }
  } 

  # GET call
  api_paths <- c("data", "v2", "ob", "l2", "snapshot")
  api_query <- list(limit = 2000, api_key = api_key)
  response <- cryptocompare_api(path = api_paths, query = api_query)
  if (response$Response == "Error") {
    cli::cli_alert_danger(response$Message)
    return(NULL)
  }
  # Output data
  data_info <- dplyr::tibble(date = Sys.time(), exchange = response$Data$M, symbol = response$Data$FSYM, currency = response$Data$TSYM)
  data_bid <- dplyr::tibble(bid_price = response$Data$BID$P, bid_quantity = response$Data$BID$Q)
  data_ask <- dplyr::tibble(ask_price = response$Data$ASK$P, ask_quantity = response$Data$ASK$Q)
  data_out <- dplyr::bind_cols(data_info, data_bid, data_ask)
  
  return(data_out)
}
