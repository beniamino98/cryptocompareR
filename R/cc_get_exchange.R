#' @title cc_get_exchange
#' @description Function to obtain exchange data of all cryptocurrency present cryptocompare's Api.
#'              It is possible to obtain data for more than one cryptocurrency per call,
#'              and it is also possible to filter the data using a reference exchange.
#'
#'
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#' @param interval temporal interval between the data can be:
#'              \itemize{
#'               \item `"day"`: daily data  (with "from", default if missing 2008-01-01)
#'               \item `"hour"`: horuly data  (with "from", default if missing 2014-01-01)
#'             }
#'
#' @param exchange reference exchange that you want to use to acquire data, see the all_exchanges function.
#'                  If NULL, the data will be imported for all exchanges.
#' @param currency reference currency that you want to use. DEFAUL = "USD"
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key   cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return a tibble


cc_get_exchange <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE){

  search.map <- NULL

  ## recursive method of s method for importing a data in an interval of dates
  cc_exchange_r <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE){


    search.out <- NULL
    import.new <- NULL
    api_key <- cc_use_key(x = api_key)

    # controllo from "NULL"
    if( is.null(from)){

      if(interval == "day" ){

        from <- "2008-01-01"
        message("default starting date for daily prices: ", from)

      } else if(interval == "hour"){

        from <- "2014-01-01"
        message("default starting date for hourly prices: ", from)

      }

    }

    # controllo to "NULL"
    if( is.null(to) ){

      to <- as.Date(lubridate::now())
      message("default ending date for stock prices: ", to)

    }


    from <- format_date(from)
    to   <- format_date(to)

    safe_import <- purrr::safely(cc_exchange_s)
    search.out  <- safe_import(search, interval = interval, exchange = exchange,
                               currency = currency, end_date = to, api_key = api_key, quiet = quiet )$result


    if( is.null(search.out) ) {
      warning("error during the importation of ", if_null(search, " all"), " symbol",  " in ", if_null(exchange, "all"), " exchanges")
      return(NULL)
    }


    i <- 0
    while( dplyr::last(search.out$time)  > from ){

      i <- i + 1
      if(i > 200){
        message("something goes wrong in the while loop")
        break
      }


      import.new <- safe_import( search = search,
                                 interval = interval,
                                 currency = currency,
                                 exchange = exchange,
                                 end_date = as.Date(dplyr::last(search.out$time)),
                                 api_key = api_key,
                                 quiet = quiet)$result

      if(is.null(import.new)){
        break
      }

      search.out <- dplyr::bind_rows(search.out,import.new)
      search.out <- dplyr::arrange(search.out,dplyr::desc(search.out$time))

    }

    search.out <- search.out[ as.Date(search.out$time) %in% seq.Date(from,to,1), ]
    search.out <- search.out[!duplicated(search.out$time),]
    search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
    search.out <- dplyr::tbl_df(search.out)

    return(search.out)


  }

  ## simple method for importing 2000 elements from cryptocompare api
  cc_exchange_s <- function(search = NULL, interval = "day", exchange = NULL,  currency = "USD",  end_date = NULL, api_key = NULL, quiet = FALSE){

    search.out <- NA
    search.api <- NA


    search.symbol   <- cc_search(search, search = "symbol")
    search.exchange <- cc_search(exchange, search = "exchange")
    api_key <- cc_use_key(x = api_key)

    if(is.null(search.symbol)   & !is.null(search))   {return(NULL)}
    if(is.null(search.exchange) & !is.null(exchange)) {return(NULL)}

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

    if ( purrr::is_empty(search.api$Data) ) { return(NULL) }

    search.out <- dplyr::tbl_df(search.api$Data)

    if(search.interval == "day"){
      search.out$time <- as.Date(unix_date(search.out$time))
    } else {
      search.out$time <- as.POSIXct(unix_date(search.out$time))
    }

    search.out$interval <- search.interval


    # general volume for market
    if ( is.null(search.symbol) & is.null(search.exchange)  ){

      search.out$exchange <- "all_exchange"
      search.out$symbol   <- "all_symbols"


      # volume for a specific coin
    } else if( !is.null(search.symbol) & is.null(search.exchange)  ){

      search.out$exchange <- "all_exchange"
      search.out$symbol   <- search.symbol


      # volume for a specific exchange for all coin
    }  else if( is.null(search.symbol) & !is.null(search.exchange)  ){

      search.out$exchange <- search.exchange
      search.out$symbol   <- "all_symbols"

      # volume for a specific exchange and for a specific coin

    } else if( !is.null(search.symbol) & !is.null(search.exchange)  ){

      search.out$exchange <- search.exchange
      search.out$symbol   <- search.symbol
    }

    search.out <- dplyr::select(search.out, "time","symbol","interval", "exchange",dplyr::everything())

    return(search.out)

  }


  if(!is.null(exchange) & !is.null(search) ){

    equalized <-  equalize(search, exchange)
    search    <-  equalized$x
    exchange  <-  equalized$y

    search.map <-  purrr::map2(search,exchange, ~cc_exchange_r(search = .x,
                                                                          interval = interval,
                                                                          exchange = .y,
                                                                          currency = currency,
                                                                          from = from,
                                                                          to = to,
                                                                          api_key = api_key,
                                                                          quiet = quiet))




  } else if( is.null(exchange) & !is.null(search) ){

    search.map <-  purrr::map(search, ~cc_exchange_r(search = .x,
                                                                interval = interval,
                                                                exchange = exchange,
                                                                currency = currency,
                                                                from = from,
                                                                to = to,
                                                                api_key = api_key,
                                                                quiet = quiet))


  } else if(!is.null(exchange) &  is.null(search) ){

    search.map <-  purrr::map(exchange, ~cc_exchange_r(search = search,
                                                                  interval = interval,
                                                                  exchange = .x,
                                                                  currency = currency,
                                                                  from = from,
                                                                  to = to,
                                                                  api_key = api_key,
                                                                  quiet = quiet))

  } else if( is.null(exchange) &  is.null(search) ){

    search.map <- cc_exchange_r(search = search, interval = interval,
                             exchange = exchange,
                             currency = currency,
                             from = from,
                             to = to,
                             api_key = api_key,
                             quiet = quiet)

    return(search.map)

  } else {

    return(NULL)

  }

  search.nofound <- unlist(purrr::map(search.map, is.null))

  search.map <- search.map[!search.nofound]

  search.map <- dplyr::bind_rows(search.map)

  if(nrow(search.map) == 0){
    return(NULL)
  } else {
    return(search.map)
  }
}











