#' @title cryptocompare_stock
#' @description Function to obtain stock data of all cryptocurrency present cryptocompare's Api.
#'              It is possible to obtain data for more than one cryptocurrency per call,
#'              and it is also possible to filter the data using a reference exchange.
#'              The time interval can be:
#'              daily with data from 2008-01-01,
#'              hourly with data from 2014-01-01
#'              and minute data for the last 7 days.
#'
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#' @param interval temporal interval between the data  ("day", "hour", "minute"). DEFAULT "day"
#' @param exchange reference exchange that you want to use to acquire data, see the all_exchanges function.
#'                If NULL, the data will be imported for all exchanges.
#'
#' @param currency reference currency that you want to use. DEFAUL = "USD"
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key   cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @examples
#' \dontrun{cryptocompare_stock(search = c("XMR","BTC","XRP"))}
#' \dontrun{cryptocompare_stock("XRP", interval = "hour", exchange = "Coinbase")}
#' \dontrun{cryptocompare_stock(c("BTC","ETH"), exchange = c("Coinbase","LocalBitcoins"))}
#'
#' @export



cryptocompare_stock <- function( search = NULL, interval = "day", exchange = NULL, currency = "USD", from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

  search.map <- NA

  if(!is.null(exchange) && length(exchange) > 1 ){
    equalized <-  equalize(search, exchange)
    search   <- equalized$x
    exchange <- equalized$y

    search.map <-  purrr::map2(search, exchange, ~cryptocompare_stock_r(search = .x,
                                                                       interval = interval,
                                                                       exchange = .y,
                                                                       currency = currency,
                                                                       from = from,
                                                                       to = to,
                                                                       api_key = api_key,
                                                                       quiet = quiet,))


  } else {

    search.map <- purrr::map(search, ~cryptocompare_stock_r( search = .x,
                                                             interval = interval,
                                                             currency = currency,
                                                             exchange = exchange,
                                                             from = from,
                                                             to = to,
                                                             api_key = api_key,
                                                             quiet = quiet))

  }

  search.nofound <- is.na(search.map)
  if(sum(search.nofound ) > 0 ){


    search.nofound.symb <- stringr::str_flatten(unique(search[search.nofound]), " ")
    message(search.nofound.symb, " not found!")

  }

  search.map <- search.map[!search.nofound]

  search.map <- dplyr::bind_rows(search.map)

  return(search.map)

}






cryptocompare_stock_r <- function( search = NULL, interval = "day",exchange = NULL, currency = "USD", from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

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

  # controllo interval "minute"
  if(interval == "minute"){

    to   <-  as.Date(lubridate::now())
    from <-  to - 4

  }

  from <- format_date(from)
  to   <- format_date(to)

  safe_import <- purrr::safely(cryptocompare_stock_s)
  search.out <- safe_import(search   = search,
                            interval = interval,
                            exchange = exchange,
                            currency = currency,
                            end_date = to,
                            api_key = api_key,
                            quiet = quiet)$result


  if( length(search.out) == 1 && is.na( search.out ) )  return(NA)

  i <- 0

  while( dplyr::last(search.out$time)  > from ) {

    import.new <- cryptocompare_stock_s( search = search,
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



cryptocompare_stock_s <- function(search = NULL, interval = "day", exchange = NULL, currency = "USD", end_date = NULL, api_key = NULL, quiet = FALSE){

  search.out <- NA
  search.api <- NA
  search.symbol  <- NA
  search.exchange <- NA

  search.symbol   <- search_symbol(search)
  search.exchange <- search_exchange(exchange)

  if(is.null(search.exchange) &  !is.null(exchange) ){ return(NA) }
  if(is.null(search.symbol)   &  !is.null(search) ){ return(NA) }

  search.supported <- c( histoday = "day", histohour = "hour", histominute = "minute")
  search.interval  <- match.arg( interval, search.supported )

  # paths
  search.paths <- c("data", "v2",  names(search.interval ) )
  search.query <- list( fsym    = search.symbol,
                        tsym    = currency,
                        e       = search.exchange,
                        limit   = "2000",
                        toTs    = unix_date(end_date, from = F),
                        api_key = api_key )


  # api response and control
  search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)

  if ( search.api$Response == "Error" ) { return(NA) }

  # adding variables
  search.out <- search.api$Data$Data[,1:6]
  search.out$adj      <- ( search.out$high + search.out$low ) / 2
  search.out$time     <-  as.POSIXct( unix_date(  search.out$time ) )
  search.out$symbol   <-  rep(search, nrow(search.out))
  search.out$exchange <-  ifelse(is.null(search.exchange), "all_exchanges", search.exchange)
  search.out$currency <- currency


  # ordering df
  search.out <- dplyr::select(search.out, "time", "symbol","exchange", "currency", dplyr::everything())
  search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
  search.out <- dplyr::tbl_df(search.out)

  return(search.out)


}


