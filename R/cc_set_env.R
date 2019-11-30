cc_env <- new.env(parent = emptyenv() )

#' @title cc_set_env
#' @description : setting the elements for automatic resarch of inputs in the \code{\link{cc_get}} function.
#' Allows to do not need to call the api every time but just the first one.
#' @param api_key an api key will be added into the environment in order to not type every time the api_key as argument
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @seealso
#' \code{\link{cc_clean_env}}
#' \code{\link{cc_use_key}}
#' @examples
#' \donttest{cc_set_env() }
#' \donttest{cc_set_env(api_key = yourapikey) }
#' @export



cc_set_env <- function(api_key = NULL, quiet = FALSE ){

  first.time <- purrr::is_empty(cc_env)

  if( !exists("coins_id", envir = cc_env ) ){

    coins_id <-  cc_get_general(search = "all_coins", quiet = quiet)

    if(!quiet) message(" Coins Id added to cc_env ! ")

    assign("coins_id", coins_id, envir = cc_env)

  }


  if( !exists("news_categories", envir = cc_env ) ){

    news_categories <-  cc_news_categories(quiet = quiet)

    if(!quiet) message(" News Categories added to cc_env ! ")

    assign("news_categories", news_categories, envir = cc_env)

  }

  if( !exists("news_feeds", envir = cc_env ) ){

    news_feeds <-  cc_news_feeds(quiet = quiet)

    if(!quiet) message(" News Feeds added to cc_env ! ")

    assign("news_feeds", news_feeds, envir = cc_env)

  }

  if( !exists("exchanges", envir = cc_env ) ){

    exchanges <-  cc_get_general(search = "all_exchanges", quiet = quiet)

    if(!quiet) message(" Exchanges availables added to cc_env ! ")

    assign("exchanges", exchanges, envir = cc_env)

  }

  if(!is.null(api_key)){

    if(!exists("api_key", envir = cc_env)){
      cc_env$api_key <- api_key
      message(" ********  api key setted in cc_env !")
    }else {
      message("api key already setted!")
    }
  }

  if(first.time){

    if(!quiet) message("cryptocompare environment created!")

  }

}


#' @title cc_clean_env
#' @description : delete all elements in the environment cc_env.
#' @seealso
#' \code{\link{cc_set_env}}
#' @examples
#' \donttest{cc_clean_env() }
#' @export

cc_clean_env <- function(){

  suppressWarnings(rm("coins_id","news_categories","news_feeds","exchanges","api_key", envir = cc_env))
  message("cc_env cleaned!")

}


#' @title cc_use_key
#' @description  you can use this command to set the api_key global option.
#' @param x character or NULL, if it is a character it will be setted as api key just if api_key slot in cc_env is empty,
#'           otherwise it will return the value of the api key if the slot is not empty, or NULL if x is NULL and the slot is empty.
#' @name cc_use_key
#' @rdname cc_use_key
#' @seealso
#' \code{\link{cc_set_env}}
#'
#' @return  api_key or NULL
#' @examples
#' \donttest{cc_use_key(x = yourapikey)}
#' @export

cc_use_key <- function(x = NULL){

  if(is.null(x) & exists("api_key", envir = cc_env)){
    return(cc_env$api_key)
  } else if(!is.null(x) & !exists("api_key", envir = cc_env)){
    assign("api_key",x, envir = cc_env)
  } else {
    return(x)
  }
}



#' @title cc_search
#' @description  you can use this command to set the api_key global option.
#' @param x character or NULL, if it is a character it will be setted as api key just if api_key slot in cc_env is empty,
#'           otherwise it will return the value of the api key if the slot is not empty, or NULL if x is NULL and the slot is empty.
#'@param search the type of research you want to use, can be :
#'   \itemize{
#'     \item `"id"`: for converting a symbol to  id.
#'     \item `"symbol"`: for searching a symbol.
#'     \item `"exchange"`: for searching an echange.
#'     \item `"feed"`:  for searching a news feed.
#'     \item `"category"`: for searching a news category.
#'   }
#' @seealso
#' \code{\link{cc_set_env}}
#'
#' @return  api_key or NULL
#' @name cc_search
#' @rdname cc_search
#' @examples
#' \donttest{cc_search("BTC", search = "id")}
#' \donttest{cc_search("BTC", search = "symbol")}
#' \donttest{cc_search("Coinbase", search = "exchange")}
#' \donttest{cc_search("yahoo", search = "feed")}
#'


cc_search <- function(x, search = "id"){

  cc_set_env(quiet = TRUE)

  matched.search <- NULL
  match.arg_sf <- purrr::safely(match.arg)
  search.match <- match.arg(search, c("id","exchange","symbol", "feed","category" ) )


  if(search.match == "id"){

    x <- toupper(x)
    matched.search <- purrr::map(x, ~match.arg_sf(.x, cc_env$coins_id$Symbol)$result )
    matched.search <- unlist(matched.search)
    matched.search <- cc_env$coins_id[  cc_env$coins_id$Symbol %in% matched.search, ]$Id

  } else if(search.match == "exchange") {

    x <- stringr::str_replace(x,"^[a-z]|^[A-Z]", toupper(stringr::str_extract(x,"^[a-z]|^[A-Z]")) )

    matched.search <- purrr::map(x, ~match.arg_sf(.x, cc_env$exchanges$InternalName)$result )
    matched.search <- unlist(matched.search)

  } else if(search.match == "symbol") {

    x <- toupper(x)
    matched.search <- purrr::map(x, ~match.arg_sf(.x, cc_env$coins_id$Symbol)$result )
    matched.search <- unlist(matched.search)

  } else if(search.match == "feed") {

    matched.search <- purrr::map(x, ~match.arg_sf(tolower(.x), cc_env$news_feeds$key)$result )
    matched.search <- unlist(matched.search)

  } else if(search.match == "category"){

    x <- stringr::str_replace(x,"^[a-z]|^[A-Z]", toupper(stringr::str_extract(x,"^[a-z]|^[A-Z]")) )

    matched.search <- purrr::map(x, ~match.arg_sf(.x, cc_env$news_categories$categoryName)$result )
    matched.search <- unlist(matched.search)


  }

  if(purrr::is_empty(matched.search)){
    return(NULL)
  } else {
    return(matched.search)
  }

}


#' @title cc_rate_limit
#' @description check the rate limit for the month.
#' @param api_key cryptocompare api key.
#' @return a tibble with information about rate limit
#' @name cc_rate_limit
#' @rdname cc_rate_limit
#' @examples
#' # generic user
#' \donttest{cc_rate_limit()}
#' # authenticate user
#' \donttest{cc_rate_limit(api_key = yourapikey )}
#'
#' @export

cc_rate_limit <- function(api_key = NULL){

  search.api <- NULL
  search.out <- NULL

  api_key <- cc_use_key(x = api_key)

  search.api <- Api("cryptocompare",paths = c("stats", "rate","limit"), query = list(api_key = api_key))

  search.out <- map_tidy(search.api$Data, binded = TRUE)
  search.out <- tibble::add_column(search.out, call =  c("made", "left"), .before = "second")

  return(search.out)

}



#' @title cc_news_categories
#' @description retrieve information about all news categories
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return a tibble
#' @name cc_news_categories
#' @rdname cc_news_categories
#' @seealso
#' \code{\link{cc_get_news}}
#' \code{\link{cc_get}}
#' @examples
#' \donttest{cc_news_categories()}
#' @export

cc_news_categories <- function(quiet = FALSE){

  search.api <- NULL
  search.out <- NULL

  search.api <- Api("cryptocompare",paths = c("data", "news", "categories"),quiet = quiet, query = NULL )
  search.out <- map_tidy(search.api, binded = TRUE)

  if(is.null(search.out)){
    return(NULL)
  } else {
    return(search.out)
  }
}


#' @title cc_news_feeds
#' @description retrieve information about all news feeds
#' @param quiet logical. When TRUE function evalueates without displaying customary messages
#' @return a tibble
#' @name cc_news_feeds
#' @rdname cc_news_feeds
#' @seealso
#' \code{\link{cc_get_news}}
#' \code{\link{cc_get}}
#' @examples
#' \donttest{cc_news_feeds()}
#' @export

cc_news_feeds <- function(quiet = FALSE){

  search.api <- NA
  search.out <- NA

  search.api <- Api(api = "cryptocompare", paths = c("data", "news", "feeds"), quiet = quiet, query = NULL )
  search.out <- map_tidy(search.api, binded = TRUE)

  if(is.null(search.out)){
    return(NULL)
  } else {
    return(search.out)
  }

}







