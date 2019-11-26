#' @title cryptocompare_news
#' @description function to obtain data on the news available with the cryptocomp API.
#'               It is possible to search all the news items in a time interval,
#'               or filter them by entering the parameter feeds (the source),
#'               or the parameter categories (the category).
#'               It is recommended to check the categories and feeds with the
#'               \code{all_categories},
#'               \code{all_feeds}, functions.
#'
#' @param search reference feed that you want to use to acquire data, see the all_feed function.
#'               If NULL, the news will be imported for all feeds.
#' @param categories reference categories that you want to use to acquire data, see the all_categories function.
#'               If NULL, the news will be imported for all categories.
#' @param from start date for import.
#' @param to end date for import.
#' @param lang reference languages, DEFAULT = "EN".
#' @param api_key cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#'
#' @export

cryptocompare_news <- function(search = NULL, categories = NULL, from = NULL, to = NULL, lang = "EN", api_key = NULL, quiet = FALSE){

  import.new <- NA
  search.out <- NA

  # controllo from "NULL"
  if( is.null(from)){

    from <- as.Date(lubridate::now())-30

  }

  # controllo to "NULL"
  if( is.null(to) ){

    to <- as.Date(lubridate::now())

  }

  from <- format_date(from)
  to   <- format_date(to)

  search.out <- cryptocompare_news_s( search = search, categories = categories, end_date = to, lang =  lang,
                                      api_key = api_key, quiet = quiet )

  if ( length(search.out) == 1 && is.na( search.out ) ) { return(NA) }

  i <- 0
  while( dplyr::last(search.out$time) > from ) {

    import.new <- cryptocompare_news_s( search = search,
                                        categories = categories,
                                        end_date = as.Date(dplyr::last(search.out$time)),
                                        lang = lang,
                                        api_key = api_key,
                                        quiet = quiet)

    search.out <- dplyr::bind_rows(search.out,import.new)
    search.out <- dplyr::arrange(search.out,dplyr::desc(search.out$time) )

    i <- i + 1
    if(i > 1000){
      message("more than","1000","calls into the while loop")
      break
    }

  }


  search.out <- search.out[as.Date(search.out$time) %in% seq.Date(from,to,1), ]

  return(search.out)

}






cryptocompare_news_s <- function(search = NULL, categories = NULL, lang = "EN", end_date = NULL, api_key = NULL, quiet = FALSE ){

  search.out <- NA
  search.api <- NA

  feeds <- search_feed(search)
  categories <- search_categories(categories)
  lang <- match.arg(lang, c("EN"))
  # paths
  search.paths <-  c("data", "v2", "news/")
  search.query <- list(feeds = feeds,
                       categories = categories,
                       lTs = unix_date(end_date, from = F),
                       lang = lang,
                       api_key = api_key )


  search.api <- Api("cryptocompare", search.paths , search.query, quiet = quiet,  json = TRUE)


  if ( purrr::is_empty(search.api$Data) ) { return(NA) }

  search.out <- tidy_list(search.api$Data, quiet = T)


  search.out <- dplyr::mutate(search.out, "time" = as.POSIXct(unix_date(search.out$published_on)))
  search.out <- dplyr::select(search.out, -"published_on")
  search.out <- dplyr::select(search.out, "time", "feed" = "source", dplyr::everything())
  search.out <- dplyr::arrange(search.out, dplyr::desc(search.out$time))


  return(search.out)


}




#' @title cryptocompare_news_categories
#' @description retrieve information about all news categories
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @examples
#' \dontrun{cryptocompare_news_categories()}
#' @export

cryptocompare_news_categories <- function(quiet = FALSE){

  search.api <- NA
  search.out <- NA

  search.api <- Api("cryptocompare",paths = c("data", "news", "categories"),quiet = quiet, query = NULL )
  search.out <- map_tidy(search.api)

  return(search.out)
}


#' @title cryptocompare_news_feeds
#' @description retrieve information about all news feeds
#' @param quiet logical. When TRUE function evalueates without displaying customary messages
#' @return tbl
#' @examples
#' \dontrun{cryptocompare_news_feeds()}
#' @export

cryptocompare_news_feeds <- function(quiet = FALSE){

  search.api <- NA
  search.out <- NA

  search.api <- Api("cryptocompare",paths = c("data", "news", "feeds"),quiet = quiet, query = NULL )
  search.out <- map_tidy(search.api)
  return(search.out)
}







