#' @title cryptocompare_social
#' @description ""
#' @param search reference symbol that you want to use to acquire data, see the all_symbol function.
#'
#' @param interval temporal interval between the data  ("day", "hour"). DEFAULT "day"
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key  cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @export

cryptocompare_social <- function( search = NULL, interval = "day",  from = NULL, to = NULL, api_key = NULL, quiet = FALSE  ){

  search.map <- NA

  search.map <- purrr::map(search, ~cryptocompare_social_r( search = .x,
                                                            interval = interval,
                                                            to = to,
                                                            from = from,
                                                            api_key = api_key,
                                                            quiet = quiet))

  search.nofound <- is.na(search.map)

  if(sum(search.nofound ) > 0 ){

    search.nofound.symb <- stringr::str_flatten(search[search.nofound], " ")
    message(search.nofound.symb, "not found!")

  }

  search.map <- search.map[!search.nofound]

  search.map <- dplyr::bind_rows(search.map)

  return(search.map)

}







cryptocompare_social_r <- function(search = NULL, interval = "day", from = NULL, to = NULL, api_key = NULL, quiet = FALSE){


  search.out <- NA

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

  safe_import <- purrr::safely(cryptocompare_social_s)
  search.out <- safe_import(search, interval = interval, end_date = to, api_key = api_key, quiet = quiet )$result

  if( length(search.out) == 1 && is.na( search.out ) )  return(NA)

  i <- 0
  while( dplyr::last(search.out$time)  > from  ){

    import.new <- cryptocompare_social_s( search = search,
                                         interval = interval,
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




cryptocompare_social_s <- function(search = NULL, interval = "day", end_date = NULL, api_key = NULL, quiet = FALSE){

  search.out <- NA
  search.api <- NA

  search.id   <- search_id(search)

  if( is.null(search.id) ) return(NA)
  if( is.null(api_key) ) stop("need a valid api key")

  search.supported <- c( `histo/day` = "day", `histo/hour` = "hour")
  search.interval  <- match.arg( interval, search.supported )


  # paths
  search.paths <- c("data", "social","coin", names(search.interval ) )
  search.query <- list( coinId  = search.id,
                        limit   = "2000",
                        toTs    = unix_date(end_date, from = F),
                        api_key = api_key )


  search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)

  if ( search.api$Response == "Error" ) { return(NA) }

  # adding variables
  search.out <- search.api$Data

  search.out$time     <-  as.POSIXct( unix_date(  search.out$time ) )
  search.out$symbol   <-  rep(search, nrow(search.out))

  search.out <- dplyr::select(search.out, "time","symbol", dplyr::everything())
  search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))
  search.out <- dplyr::tbl_df(search.out)
  return(search.out)


}

