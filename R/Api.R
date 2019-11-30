#' @title Api
#' @description the base function for importing data from a web Api, it is scalable, if you want to add more
#' apis you have just to add the new base link inside the funcion in api.supported variable.
#' @param api character, api to use for importing data (cryptocompare)
#' @param paths vector, paths for the contruction of the link (see api documentation )
#' @param query list, query for the contruction of the link (see api documentation )
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @param config configuration param \code{\link[httr]{config}},
#'               see also \code{\link[httr]{authenticate}}, from httr package.
#' @param json logical. When TRUE the response will be converted from json, else is converted in the base form
#'              with the function \code{\link[httr]{content}}

Api  <- function(api = "blockchain", paths = NULL, query = NULL, config = list(), quiet = TRUE, json = FALSE  ) {

  api.match <- NULL
  base.url  <- NULL
  response.get <- NULL
  response.content <- NULL

  # supported base url
  api.supported <- list(

    blockchain    = "https://blockchain.info",
    etherscan     = "https://api.etherscan.io",
    bitnodes      = "https://bitnodes.earn.com",
    blockchair    = "https://api.blockchair.com",
    cryptocompare = "https://min-api.cryptocompare.com",
    coingenko     = "https://api.coingecko.com"
  )

  api.match <- match.arg(api , names(api.supported) )
  api.base  <- api.supported[[api.match]]

  # cleaning list query and vector paths from empty elements or NA element
  api.query <- query[!is.na(map_na(query))]
  api.paths <- paths[!is.na(map_na(paths))]

  api.url <- httr::modify_url(api.base, path = api.paths, query = api.query)


  if ( !quiet ) {
    api.get <- httr::GET(api.url, httr::progress(), config = config )
  } else {
    api.get <- httr::GET(api.url, config = config )
  }


  # importation status control
  if ( httr::status_code(api.get) != 200 || purrr::is_empty(api.get) ) {

    if ( !quiet ) message("error during the importation: status code not equal to 200")

    return(NULL)

  }


  # conversion from json
  if ( json ){

    api.content <- httr::content( api.get, "text" )
    api.content <- jsonlite::fromJSON(api.content)

  } else {

    api.content <- httr::content( api.get )

  }

  attr(api.content, "api") <- api.match

  return( api.content )

}
