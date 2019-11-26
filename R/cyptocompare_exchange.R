#' @title cryptocompare_exchange
#' @description function to get the data from exchange from cryptocompare api.
#'             It is a fairly versatile function: if only the search parameter is specified,
#'               it will return the exchange data relating to a specific cryptocurrency for all exchanges;
#'             if only the exchange is specified the exchange,
#'               data refer to all the cryptocurrencies exchanged in that exchange;
#'             finally if the search param == "all", and the exchange is not specified, the data will refer to
#'             the entire market (all cryptocurrencies and all exchanges).
#'             Obviously, by specifying both the symbol and the exchange,
#'             the data will be specific to the two parameters entered.
#'
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#'               If NULL, the imported data refer to market data (all_symbols).
#' @param interval temporal interval between the data  ("day", "hour" ). DEFAULT "day"
#' @param exchange reference exchange that you want to use to acquire data, see the all_exchanges function.
#'                If NULL,the imported data refer to market data (all exchanges).
#' @param currency reference currency that you want to use. DEFAUL = "USD"
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key   cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @examples
#' \dontrun{cryptocompare_exchange(search = c("BTC","ETH"), from = "2019-06-01")}
#' \dontrun{cryptocompare_exchange(search = "BTC" , exchange = c("Bitstamp","BitTrex"))}
#' \dontrun{cryptocompare_exchange(search = c("BTC","ETH"), exchange = "Coinbase" )}
#'
#' @export

cryptocompare_exchange <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE){

  search.map <- NA

  if(!is.null(exchange) & !is.null(search) ){

    equalized <-  equalize(search, exchange)
    search   <- equalized$x
    exchange <- equalized$y

    search.map <-  purrr::map2(search,exchange, ~cryptocompare_exchange_r(search = .x,
                                                                          interval = interval,
                                                                          exchange = .y,
                                                                          currency = currency,
                                                                          from = from,
                                                                          to = to,
                                                                          api_key = api_key,
                                                                          quiet = quiet))




  } else if( is.null(exchange) & !is.null(search) ){

    search.map <-  purrr::map(search, ~cryptocompare_exchange_r(search = .x,
                                                                interval = interval,
                                                                exchange = exchange,
                                                                currency = currency,
                                                                from = from,
                                                                to = to,
                                                                api_key = api_key,
                                                                quiet = quiet))


  } else if(!is.null(exchange) &  is.null(search) ){

    search.map <-  purrr::map(exchange, ~cryptocompare_exchange_r(search = search,
                                                                  interval = interval,
                                                                  exchange = .x,
                                                                  currency = currency,
                                                                  from = from,
                                                                  to = to,
                                                                  api_key = api_key,
                                                                  quiet = quiet))

  } else if( is.null(exchange) &  is.null(search) ){

    search.map <- cryptocompare_exchange_r(search = search, interval = interval,
                             exchange = exchange,
                             currency = currency,
                             from = from,
                             to = to,
                             api_key = api_key,
                             quiet = quiet)

    return(search.map)

  }

  search.nofound <- is.na(search.map)

  if(sum(search.nofound ) > 0 ){

    search.nofound.symb <- stringr::str_flatten(search[search.nofound], " ")
    message(search.nofound.symb, "not found!")

  }

  search.map <- search.map[!search.nofound]

  search.map <- dplyr::bind_rows(search.map)


  return(search.map)

}







## recursive version of s method
cryptocompare_exchange_r <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE){


  search.out <- NA
  import.new <- NA

  # controllo from "NULL"
  if( is.null(from)){

    if(interval == "day" ){

      from <- "2008-01-01"

    } else if(interval == "hour"){

      from <- "2014-01-01"

    }

  }

  # controllo to "NULL"
  if( is.null(to) ){

    to <- as.Date(lubridate::now())

  }


  from <- format_date(from)
  to   <- format_date(to)

  safe_import <- purrr::safely(cryptocompare_exchange_s)
  search.out <- safe_import(search, interval = interval, exchange = exchange, currency = currency, end_date = to, api_key = api_key, quiet = quiet )$result

  if( length( search.out ) == 1 && is.na(search.out) )  return(NA)

  i <- 0
  while( dplyr::last(search.out$time)  > from ){

    import.new <- cryptocompare_exchange_s( search = search,
                                            interval = interval,
                                            currency = currency,
                                            exchange = exchange,
                                            end_date = as.Date(dplyr::last(search.out$time)),
                                            api_key = api_key,
                                            quiet = quiet)

    search.out <- dplyr::bind_rows(search.out,import.new)
    search.out <- dplyr::arrange(search.out,dplyr::desc(search.out$time))


    i <- i + 1
    if(i > 200){
      message("something goes wrong in the while loop")
      break
    }

  }

  search.out <- search.out[ as.Date(search.out$time) %in% seq.Date(from,to,1), ]
  search.out <- search.out[!duplicated(search.out$time),]
  search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
  search.out <- dplyr::tbl_df(search.out)

  return(search.out)


}




## basic mathod
cryptocompare_exchange_s <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  end_date = NULL, api_key = NULL, quiet = FALSE){

  search.out <- NA
  search.api <- NA


  search.symbol   <- search_symbol(search)
  search.exchange <- search_exchange(exchange)


  if(is.null(search.symbol)   & !is.null(search))   {return(NA)}
  if(is.null(search.exchange) & !is.null(exchange)) {return(NA)}

  search.supported <- c( histoday = "day", histohour = "hour")
  search.interval  <- match.arg( interval, search.supported )

  # query
  search.query <- list( fsym    = search.symbol,
                        tsym    = currency,
                        e       = search.exchange,
                        limit   = "2000",
                        toTs    = unix_date(end_date, from = F),
                        api_key = api_key )

  # paths
  # general volume for market
  if ( is.null(search.symbol) & is.null(search.exchange)  ){

    search.paths <- c("data", "exchange",  names(search.interval ) )


    # volume for a specific coin
  } else if( !is.null( search.symbol) & is.null(search.exchange)  ){

    search.paths <- c("data", "symbol",  names(search.interval ) )


    # volume for a specific exchange for all coin
  }  else if( is.null(search.symbol) & !is.null(search.exchange)  ){

    search.paths <- c("data", "exchange",  names(search.interval ) )



    # volume for a specific exchange and for a specific coin
  } else if( !is.null(search.symbol) & !is.null(search.exchange)  ){

    search.paths <- c("data","exchange", "symbol",  names(search.interval ) )


  }


  search.api <- Api("cryptocompare", search.paths , search.query,quiet = quiet, json = TRUE)
  if ( purrr::is_empty(search.api$Data) ) { message("empty"); return(NA) }

  search.out <- dplyr::tbl_df(search.api$Data)

  search.out$time <- as.POSIXct(unix_date(search.out$time))
  search.out$interval <- rep(search.interval, nrow(search.out))


  # general volume for market
  if ( is.null(search.symbol) & is.null(search.exchange)  ){

    search.out$exchange <- rep("all_exchange", nrow(search.out))
    search.out$symbol   <- rep("all_symbols", nrow(search.out))


    # volume for a specific coin
  } else if( !is.null(search.symbol) & is.null(search.exchange)  ){

    search.out$exchange <- rep("all_exchange", nrow(search.out))
    search.out$symbol   <- rep(search.symbol, nrow(search.out))


    # volume for a specific exchange for all coin
  }  else if( is.null(search.symbol) & !is.null(search.exchange)  ){

    search.out$exchange <- rep( search.exchange, nrow(search.out))
    search.out$symbol   <- rep("all_symbols", nrow(search.out))

    # volume for a specific exchange and for a specific coin

  } else if( !is.null(search.symbol) & !is.null(search.exchange)  ){

    search.out$exchange <- rep(search.exchange, nrow(search.out))
    search.out$symbol   <- rep(search.symbol, nrow(search.out))
  }


  search.out <- dplyr::select(search.out, "time","symbol","interval", "exchange",dplyr::everything())
  return(search.out)


}


