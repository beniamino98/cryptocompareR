#' @title cryptocompare_api
#'
#' @description Function to execute a call to the cryptocompare Api. It will automatically detect if some errors has occurred, in that case the function will return NULL.
#' It is build using the `httr` functions.
#'
#' @param path vector, path for the construction of the link (see the official api documentation)
#' @param query list, query for the construction of the link (see official api documentation), the names of each component of the list is the name of the Api argument while the element is the argument itself.
#' @param config configuration parameter `config` from the function \code{\link[httr]{GET}}.
#' @param type character, see the documentation of \code{\link[httr]{content}}.
#' @param encoding character, see the documentation of \code{\link[httr]{content}}.
#' @param tojson logical, if TRUE the response will be converted from json.
#' \itemize{
#'  \item `type = "raw"` will return a raw vector.
#'  \item `type = "text"`, will return a character vector of length 1. The character vector is always re-encoded to UTF-8.
#'  If this encoding fails (usually because the page declares an incorrect encoding), `content()` will return NA.
#'  \item `type = "auto"`, will return a parsed R object.
#' }
#'
#'
#' @examples
#' api_paths = c("data", "exchange", "histoday")
#' api_query = list(tsym = "USD",
#'                   e = "Binance",
#'                   toTs = as_unix("2023-01-01"),
#'                   limit = 2000, api_key = NULL)
#'
#' cryptocompare_api(path = api_paths,
#'                   query = api_query,
#'                   config = list(),
#'                   type = "text",
#'                   encoding = "UTF-8",
#'                   tojson = TRUE)
#'
#' @importFrom httr GET modify_url status_code content
#' @importFrom jsonlite fromJSON
#' @export

cryptocompare_api <- function(path = NULL, query = NULL, config = list(), type = "text", encoding = "UTF-8", tojson = TRUE){

  # Base Url Api
  api_base <- "https://min-api.cryptocompare.com"

  # Path and Query parameters
  api_paths <- path[!is.null(path) && !is.na(path)]
  api_query <- query[!is.null(query) && !is.na(query)]

  # creation api url for the call
  api_url <- httr::modify_url(api_base, path = api_paths, query = api_query)

  # GET call
  api_get <- httr::GET(api_url, config = config )

  # status control
  api_status <- httr::status_code(api_get) == 200
  api_empty  <- purrr::is_empty(api_get)

  # error status
  if(!api_status){warning("GET Request Error: status code is not equal to 200.")}

  # error empty
  if(api_empty){warning("GET Request Error: get request is empty!")}

  # transformation with encoding or JSON
  if(api_status && !api_empty){
    api_content <- httr::content(api_get, type = type, encoding = encoding)
    if(tojson){
      api_content <- jsonlite::fromJSON(api_content)
    }
  } else {
    api_content <- NULL
  }

  # adding Api attribute
  attr(api_content, "Api") <- "cryptocompare"

  return(api_content)

}

