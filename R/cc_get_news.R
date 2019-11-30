#' @title cc_get_news
#' @description function to obtain data on the news available with the cryptocomp API.
#'               It is possible to search all the news items in a time interval,
#'               or filter them by entering the parameter feeds (the source),
#'               or the parameter category .
#'               It is recommended to check the categories and feeds with the
#'               \code{cc_category_option},
#'               \code{cc_feed_option}, functions.
#'
#' @param search reference feed that you want to use to acquire data, see the all_feed function.
#'               If NULL, the news will be imported for all feeds.
#' @param category reference categories that you want to use to acquire data, see the all_categories function.
#'               If NULL, the news will be imported for all categories.
#' @param from start date for import.
#' @param to end date for import.
#' @param api_key cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @name cc_get_news
#' @rdname cc_get_news
#' @return tbl



cc_get_news <- function(search = NULL, category = NULL, from = NULL, to = NULL,  api_key = NULL, quiet = FALSE){

  import.new <- NULL
  search.out <- NULL
  api_key <- cc_use_key(x = api_key)

  ## simple method for importing 2000 elements from cryptocompare api
  cc_news_s <- function(search = NULL, category = NULL, end_date = NULL, api_key = NULL, quiet = FALSE ){

    search.out <- NULL
    search.api <- NULL

    lang <- "EN"
    feeds <- cc_search(search, search = "feed")
    api_key <- cc_use_key(x = api_key)
    category <- cc_search(category, search = "category")


    # paths
    search.paths <-  c("data", "v2", "news/")
    search.query <- list(feeds = feeds,
                         categories = category,
                         lTs = unix_date(end_date, from = F),
                         lang = lang,
                         api_key = api_key )


    search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)


    if ( purrr::is_empty(search.api$Data) ) { return(NULL) }

    search.out <- tidy_list(search.api$Data, quiet = T)

    search.out$time <-  as.POSIXct(unix_date(search.out$published_on))
    search.out <- dplyr::select(search.out, -"published_on")
    search.out <- dplyr::select(search.out, "time",  dplyr::everything())
    search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))

    return(search.out)


  }

  # controllo from "NULL"
  if( is.null(from)){

      from <- as.Date(lubridate::now())-30
      message("default starting date for news: ", from)

  }

  # controllo to "NULL"
  if( is.null(to) ){

    to <- as.Date(lubridate::now())
    message("default ending date for news: ", to)

  }

  from <- format_date(from)
  to   <- format_date(to)

  safe_import <- purrr::safely(cc_news_s)
  search.out <- safe_import( search = search, category = category, end_date = to,
                                      api_key = api_key, quiet = quiet )$result

  if ( is.null(search.out) ) { return(NULL) }

  i <- 0
  while( dplyr::last(search.out$time) > from ) {

    i <- i + 1
    if(i > 1000){
      message("more than","1000","calls into the while loop")
      break
    }

    import.new <- safe_import( search = search,
                               category = category,
                                        end_date = as.Date(dplyr::last(search.out$time)),
                                        api_key = api_key,
                                        quiet = quiet)$result

    if(is.null(import.new)){
      break
    }


    search.out <- dplyr::bind_rows(search.out,import.new)
    search.out <- dplyr::arrange(search.out,dplyr::desc(search.out$time) )


  }


  search.out <- search.out[as.Date(search.out$time) %in% seq.Date(from,to,1), ]

  return(search.out)

}









