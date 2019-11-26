cryptocompare_env <- new.env(parent = .GlobalEnv )

#' @title new_cryptocompare_env
#' @description : create a new search envirnoment if it doesn't exist
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#'
#' @export


new_cryptocompare_env <- function(quiet = FALSE ){


  if( !exists("ID", envir = cryptocompare_env) ) {

    if(!quiet) message("ID added to cryptocompare_env!")

    ID <-  cryptocompare_general("coin", quiet = quiet)
    assign("ID", ID, envir = cryptocompare_env)

  }

  if( !exists("categories", envir = cryptocompare_env) ){

    if(!quiet) message("news categories added to cryptocompare_env!")
    categories <-  cryptocompare_news_categories(quiet = quiet)
    assign("categories", categories, envir = cryptocompare_env)
  }

  if( !exists("feeds", envir = cryptocompare_env) ){

    if(!quiet) message("news feed added to cryptocompare_env!")
    feeds <-  cryptocompare_news_feeds(quiet = quiet)
    assign("feeds",feeds, envir = cryptocompare_env)
  }

  if( !exists("exchanges", envir = cryptocompare_env) ){

    if(!quiet) message("exchange added to cryptocompare_env!")
    exchanges <-  cryptocompare_general("exchange", quiet = quiet )
    assign("exchanges", exchanges, envir = cryptocompare_env)

  }

  if(!quiet) message("cryptocompare_env")

}


#' @title clean_cryptocompare_env
#' @description :
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#'
#' @export

clean_cryptocompare_env <- function(quiet = FALSE ){


  rm(list = c("ID", "categories", "feeds", "exchanges"), envir = cryptocompare_env)
  suppressWarnings(rm(list = c("ID", "categories", "feeds", "exchanges"), envir = .GlobalEnv))


}




# ----------   searching methods   ----------------------------------------------------------------------
search_feed <- function(search = NULL ) {

  matched.search <- NULL
  if ( is.null(search) || is.na(search) ) return(NULL)

   match.arg_sf <- purrr::safely(match.arg)
   new_cryptocompare_env(quiet = T)

  matched.search <- NA
  matched.search <- purrr::map(search, ~match.arg_sf(tolower(.x), cryptocompare_env$feeds$key)$result )
  matched.search <- unlist(matched.search)

  if(purrr::is_empty(matched.search)){
    return(NULL)
  } else {

    return(matched.search)
  }


}


search_categories <- function(search = NULL ) {

  matched.search <- NULL

  if ( is.null(search) || is.na(search) ) return(NULL)

  match.arg_sf <- purrr::safely(match.arg)
  new_cryptocompare_env(quiet = T)

  first.letter.upper <- toupper(stringr::str_extract(search,"^[a-z]|^[A-Z]"))
  search <- stringr::str_replace(search,"^[a-z]|^[A-Z]", first.letter.upper )

  matched.search <- purrr::map(search, ~match.arg_sf(.x, cryptocompare_env$categories$categoryName)$result )
  matched.search <- unlist(matched.search)


  if(purrr::is_empty(matched.search)){
    return(NA)
  } else {
    return(matched.search)
  }

}


search_exchange <- function(search = NULL ) {

  matched.search <- NULL

  if ( is.null(search) || is.na(search) ) return(NULL)

  match.arg_sf <- purrr::safely(match.arg)
  new_cryptocompare_env(quiet = T)

  first.letter.upper <- toupper(stringr::str_extract(search,"^[a-z]|^[A-Z]"))
  search <- stringr::str_replace(search,"^[a-z]|^[A-Z]", first.letter.upper )

  matched.search <- purrr::map(search, ~match.arg_sf(.x, cryptocompare_env$exchanges$InternalName)$result )
  matched.search <- unlist(matched.search)

  if(purrr::is_empty(matched.search)){
    return(NULL)
  } else {
    return(matched.search)
  }


}

search_symbol <- function(search = NULL ) {

  matched.search <- NULL
  if ( is.null(search) || is.na(search) ) return(NULL)

  match.arg_sf <- purrr::safely(match.arg)
  new_cryptocompare_env(quiet = T)

  matched.search <- purrr::map(search, ~match.arg_sf(toupper(.x), cryptocompare_env$ID$Symbol)$result )
  matched.search <- unlist(matched.search)

  if(purrr::is_empty(matched.search)){
    return(NULL)
  } else {
    return(matched.search)
  }

}

search_id <- function(search = NULL ) {

  matched.search <- NULL
  if ( is.null(search) || is.na(search) ) return(NULL)

  match.arg_sf <- purrr::safely(match.arg)
  new_cryptocompare_env(quiet = T)

  matched.search <- purrr::map(search, ~match.arg_sf(toupper(.x), cryptocompare_env$ID$Symbol)$result )
  matched.search <- unlist(matched.search )
  matched.search <- cryptocompare_env$ID[ cryptocompare_env$ID$Symbol %in% matched.search, ]$Id

  if(purrr::is_empty(matched.search)){
    return(NULL)
  } else {
    return(matched.search)
  }

}


# --------------  all methods  ----------------------------------------------------------------------------------------

#' @title all_feeds
#' @description get all news feeds available.
#' @examples
#' \dontrun{all_feeds()}
#' @export

all_feeds <- function(){

  new_cryptocompare_env(quiet = T)

  cryptocompare_env$feeds$key

}


#' @title all_categories
#' @description get all news categories available.
#' @examples
#' \dontrun{all_categories()}
#' @export

all_categories <- function(){

  new_cryptocompare_env(quiet = T)

  cryptocompare_env$categories$categoryName

}



#' @title all_symbols
#' @description get all symbols available.
#' @examples
#' \dontrun{all_symbols()}
#' @export

all_symbols <- function(){

  new_cryptocompare_env(quiet = T)

  cryptocompare_env$ID$Symbol

}



#' @title all_exchanges
#' @description get all exchanges available.
#' @examples
#' \dontrun{all_exchanges()}
#' @export

all_exchanges <- function(){

  new_cryptocompare_env(quiet = T)

  cryptocompare_env$exchanges$InternalName


}
