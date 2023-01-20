
cc_orderbook_exchange <- function(api_key = NULL){

  if(is.null(api_key)){
    warning("Need a valid Api Key to reach this endpoint!")
    return(NULL)
  }
  api_paths <- c("data", "ob", "exchanges")
  api_query <- list(api_key = api_key)

  out_data <- cryptocompare_api(path = api_paths, query = api_query)

  out_data <- purrr::map(out_data$Data, ~.x$orderBookInternalName)

  dplyr::tibble(Exchange = names(out_data), OB = unlist(out_data))

}

cc_L1_order_book <- function(symbol = NULL, currency = NULL, exchange = NULL, api_key = NULL){

  if(is.null(api_key)){
    warning("Need a valid Api Key to reach this endpoint!")
    return(NULL)
  }

  query_currency <- paste0(currency, collapse = ",")
  query_symbol   <- paste0(symbol, collapse = ",")

  api_paths <- c("data", "ob", "l1", "top")
  api_query <- list(fsyms = symbol.query, tsyms = currency.query, e = exchange, api_key = api_key)

  out_data <- cryptocompare_api(path = api_paths, query = api_query)

  if(out_data$Response == "Error"){
    warning(out_data$Message)
    return(NULL)
  }

  new_out_data <- purrr::map2_df(out_data$Data$RAW[[1]], currency, ~dplyr::bind_cols(Currency = .y, dplyr::bind_cols(.x) ))
  new_out_data <- dplyr::bind_cols(new_out_data, Symbol = symbol[1])

  if(length(symbol) > 1){
    for(i in 2:length(cc_data$Data$RAW)){

      new_data     <- purrr::map2_df(out_data$Data$RAW[[i]], currency, ~dplyr::bind_cols(Currency = .y, dplyr::bind_cols(.x) ))
      new_data     <- dplyr::bind_cols(new_data, Symbol = symbol[i])
      new_out_data <- dplyr::bind_rows(new_out_data, new_data )
    }
  }


  new_out_data = dplyr::mutate(new_out_data,
                              Exchange = exchange,
                              Date = Sys.Date(),
                              H = lubridate::hour(Sys.time()),
                              M = lubridate::minute(Sys.time()),
                              Spread = ASK-BID,
                              SpreadBID_perc = Spread/BID*100,
                              SpreadASK_perc = Spread/ASK*100
  )

  new_out_data <- dplyr::select(new_out_data, Date, H, M, Symbol, Currency, Exchange, BID, ASK, Spread, SpreadBID_perc, SpreadASK_perc)

  return(new_out_data)
}

cc_order_book_L2 <- function(api_key = NULL){

  if(is.null(api_key)){
    warning("Need a valid Api Key to reach this endpoint!")
    return(NULL)
  }

  # Path, Query & Response
  api.paths = c("data", "v2", "ob", "l2", "snapshot")

  api.query = list(limit = 2000, api_key = api_key)

  api.response <- cryptocompare_api(path = api.paths, query = api.query)

  if(api.response$Response == "Error"){
    warning(api.response$Message)
    return(NULL)
  }

  dplyr::bind_cols(
    dplyr::tibble(
      Date = Sys.time(),
      Exchange = api.response$Data$M,
      Symbol =  api.response$Data$FSYM,
      Currency = api.response$Data$TSYM
    ),

    dplyr::tibble(
      BID_Price = api.response$Data$BID$P,
      BID_Quantity = api.response$Data$BID$Q
    ),

    dplyr::tibble(
      ASK_Price = api.response$Data$ASK$P,
      ASK_Quantity = api.response$Data$ASK$Q
    )
  )

}

# cc_orderbook_exchange(api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813")

# cc_L1_order_book("BTC", currency = "USDT",exchange = "Binance", api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813" )

# cc_order_book_L2(api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813")
