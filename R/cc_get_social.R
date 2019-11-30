#' @title cc_get_social
#' @description function for importing social data from \href{https://www.cryptocompare.com}{Cryptocompare}.
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#' @param interval temporal interval between the data can be:
#'              \itemize{
#'               \item `"day"`: daily data  (with "from", default if missing 2008-01-01)
#'               \item `"hour"`: horuly data  (with "from", default if missing 2014-01-01)
#'             }
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key  cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @examples
#' \donttest{cc_get_social(c("BTC","XMR"), interval = "day", api_key = "yourapikey")}
#' \donttest{cc_get_social(c("BTC","XMR"), interval = "hour", api_key = "yourapikey")}
#' @name cc_get_social
#' @rdname cc_get_social
#' @return a tibble

cc_get_social <- function( search = NULL, interval = "day",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

  search.map <- NULL

  api_key <- cc_use_key(x = api_key)

  if( is.null(api_key) )  {
    warning("need a valid api key")
    return(NULL)
  }


  ## recursive method of s method for importing a data in an interval of dates
  cc_social_r <- function(search = NULL, interval = "day", from = NULL, to = NULL, api_key = NULL, quiet = FALSE){


    search.out <- NULL
    api_key <- cc_use_key(x = api_key)

    # controllo from "NULL"
    if( is.null(from)) {

      if(interval == "day" ){

        from <- "2008-01-01"
        message("default starting date for daily social data: ", from)

      } else if(interval == "hour"){

        from <- "2014-01-01"
        message("default starting date for hourly social data: ", from)

      }

    }

    # controllo to "NULL"
    if( is.null(to) ){

      to <- as.Date(lubridate::now())
      message("default ending date for social data: ", to)

    }

    from <- format_date(from)
    to   <- format_date(to)

    safe_import <- purrr::safely(cc_social_s)
    search.out  <- safe_import(search, interval = interval, end_date = to, api_key = api_key, quiet = quiet )$result

    if( is.null(search.out) ) {
      warning("error during the importation of the symbol: ", search)
      return(NULL)
    }

    i <- 0
    while( dplyr::last(search.out$time)  > from  ){

      i <- i + 1
      if(i > 200) {
        message("something goes wrong in the while loop")
        break
      }

      import.new <- safe_import( search = search,
                                 interval = interval,
                                 end_date = as.Date(dplyr::last(search.out$time)),
                                 api_key = api_key,
                                 quiet = quiet)$result
      if(is.null(safe_import)){
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
  cc_social_s <- function(search = NULL, interval = "day", end_date = NULL, api_key = NULL, quiet = FALSE){

    search.out <- NULL
    search.api <- NULL

    api_key     <- cc_use_key(x = api_key)
    search.id   <- cc_search(search, search = "id")

    if( is.null(search.id) ){ return(NULL) }

    search.supported <- c( `histo/day` = "day", `histo/hour` = "hour")
    search.interval  <- match.arg( interval, search.supported )

    # paths
    search.paths <- c("data", "social","coin", names(search.interval ) )
    search.query <- list( coinId  = search.id,
                          limit   = "2000",
                          toTs    = unix_date(end_date, from = F),
                          api_key = api_key )

    search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)

    if ( search.api$Response == "Error" ) { return(NULL) }

    # adding variables
    search.out <- search.api$Data


    if(search.interval == "day"){

      search.out$time     <-  as.Date( unix_date(  search.out$time ) )

    } else {

      search.out$time     <-  as.POSIXct( unix_date(  search.out$time ) )
    }


    search.out$symbol  <- search
    search.out <- dplyr::select(search.out, "time","symbol", dplyr::everything())
    search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
    search.out <- dplyr::tbl_df(search.out)

    return(search.out)


  }



  search.map <- purrr::map(search, ~cc_social_r( search = .x,
                                                            interval = interval,
                                                            to = to,
                                                            from = from,
                                                            api_key = api_key,
                                                            quiet = quiet))

  search.nofound <- unlist(purrr::map(search.map, is.null))

  if(sum(search.nofound) == length(search.map)){
    return(NULL)
  } else {
    search.map <- search.map[!search.nofound]

    search.map <- dplyr::bind_rows(search.map)
  }
}




