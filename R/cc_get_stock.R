
#' @title cc_get_stock
#' @description Function to obtain stock data of all cryptocurrency present cryptocompare's Api.
#'              It is possible to obtain data for more than one cryptocurrency per call,
#'              and it is also possible to filter the data using a reference exchange \code{\link{cc_exchange_options}}.
#'
#'
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#' @param interval temporal interval between the data can be:
#'              \itemize{
#'               \item `"day"`: daily data  (with "from", default if missing 2008-01-01)
#'               \item `"hour"`: horuly data  (with "from", default if missing 2014-01-01)
#'               \item `"minute"`: inute data for the last 7 days.
#'
#'             }
#' @param exchange reference exchange that you want to use to acquire data, see the all_exchanges function.
#'                If NULL, the data will be imported for all exchanges.
#'
#' @param currency reference currency that you want to use. DEFAUL = "USD"
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key   cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @name cc_get_stock
#' @rdname cc_get_stock
#' @return tbl



cc_get_stock <- function( search = NULL, interval = "day", exchange = NULL, currency = "USD", from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

  search.map <- NULL
  api_key <- cc_use_key(x = api_key)


  ## recursive method of s method for importing a data in an interval of dates
  cc_stock_r <- function( search = NULL, interval = "day",exchange = NULL, currency = "USD", from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

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

    # controllo interval "minute"
    if(interval == "minute"){

      to   <-  as.Date(lubridate::now())
      from <-  to - 4
      message("default interval for minutes data: ", from, " - ", to )

    }

    from <- format_date(from)
    to   <- format_date(to)

    safe_import <- purrr::safely(cc_stock_s)
    search.out <- safe_import(search   = search,
                              interval = interval,
                              exchange = exchange,
                              currency = currency,
                              end_date = to,
                              api_key  = api_key,
                              quiet    = quiet)$result


    if( is.null(search.out) ) {
      warning("Error during the importation of the symbol: ", search, " in ", if_null(exchange, "all"), " exchanges")
      return(NULL)
    }

    i <- 0

    while( dplyr::last(search.out$time)  > from ) {

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
  cc_stock_s <- function(search = NULL, interval = "day", exchange = NULL, currency = "USD", end_date = NULL, api_key = NULL, quiet = FALSE){

    search.out <- NA
    search.api <- NA
    search.symbol  <- NA
    search.exchange <- NA

    api_key <- cc_use_key(x = api_key)

    search.symbol   <-  cc_search(search, search = "symbol")
    search.exchange <-  cc_search(exchange, search = "exchange")

    if(is.null(search.exchange) &  !is.null(exchange) ){ return(NULL) }
    if(is.null(search.symbol)   &  !is.null(search) )  { return(NULL) }

    search.supported <- c( histoday = "day", histohour = "hour", histominute = "minute")
    search.interval  <- match.arg( interval, search.supported )

    # paths
    search.paths <- c("data", "v2",  names(search.interval ) )
    search.query <- list( fsym    =  search.symbol,
                          tsym    =  currency,
                          e       =  search.exchange,
                          limit   = "2000",
                          toTs    =  unix_date(end_date, from = F),
                          api_key =  api_key )


    # api response and control
    search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)

    if ( search.api$Response == "Error" ) { return(NULL) }

    # adding variables
    search.out <- search.api$Data$Data[,1:6]
    search.out$adj      <- ( search.out$high + search.out$low ) / 2
    search.out$symbol   <-  search.symbol
    search.out$exchange <-  if_null(search.exchange, "all_exchanges")
    search.out$currency <-  currency

    if(search.interval == "day"){

      search.out$time     <-  as.Date( unix_date(  search.out$time ) )

    } else {

      search.out$time     <-  as.POSIXct( unix_date(  search.out$time ) )
    }


    # ordering df
    search.out <- dplyr::select(search.out, "time", "symbol","exchange", "currency", dplyr::everything())
    search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
    search.out <- dplyr::tbl_df(search.out)

    return(search.out)


  }




  if(!is.null(exchange) && length(exchange) > 1 ){

    equalized <-   equalize(search, exchange)
    search    <-   equalized$x
    exchange  <-   equalized$y

    search.map <-  purrr::map2(search, exchange, ~cc_stock_r(search = .x,
                                                                       interval = interval,
                                                                       exchange = .y,
                                                                       currency = currency,
                                                                       from = from,
                                                                       to = to,
                                                                       api_key = api_key,
                                                                       quiet = quiet))


  } else {

    search.map <- purrr::map(search, ~cc_stock_r( search = .x,
                                                             interval = interval,
                                                             currency = currency,
                                                             exchange = exchange,
                                                             from = from,
                                                             to = to,
                                                             api_key = api_key,
                                                             quiet = quiet))

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


