#' GET call to cryptocompare API
#' 
#' Execute a GET call to cryptocompare API. 
#'
#' @param path Character vector. API path, `NULL` or `NA` elements will be excluded. 
#'
#' @param query Named list. Query parameters for the API call, `NULL` or `NA` elements will be excluded. 
#'
#' @param config Configuration parameter `config`. See the function \code{\link[httr]{GET}}.
#'
#' @examples
#' api_path = c("data", "exchange", "histoday")
#' api_query = list(tsym = "USD", e = "Binance",
#'                  toTs = as_unix("2023-01-01"),
#'                  limit = 2000, api_key = NULL)
#' cryptocompare_api(path = api_path, query = api_query)
#'
#' @importFrom httr GET modify_url status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr is_empty map_lgl
#' 
#' @export
#' 
#' @name cryptocompare_api
#' @rdname cryptocompare_api

cryptocompare_api <- function(path = NULL, query = NULL, config = list()){

  # Api path
  if (purrr::is_empty(path)){
    api_path <- NULL
  } else {
    # Remove NA elements 
    api_path <- path[!is.na(path)]
  }
  # Api query
  if (purrr::is_empty(query)){
    api_query <- list()
  } else {
    # Remove NULL elements 
    non_null <- !purrr::map_lgl(query, is.null)
    api_query <- query[non_null]
    # Remove NA elements 
    non_na <- !purrr::map_lgl(api_query, is.na)
    api_query <- api_query[non_na]
  }
  # Api url
  api_url <- httr::modify_url("https://min-api.cryptocompare.com", path = api_path, query = api_query)
  # GET call
  response <- httr::GET(api_url, config = config)
  
  # Check http status code 
  api_status <- httr::status_code(response)
  if (api_status != 200) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request ERROR: status code is not equal to 200.")
    }
  }
  
  # Check if response is empty  
  api_empty <- purrr::is_empty(response)
  if (api_empty) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request Error: response is empty!")
    }
  }

  # Output 
  if (api_status == 200 && !api_empty) {
    api_content <- httr::content(response, type = "text", encoding = "UTF-8")
    api_content <- jsonlite::fromJSON(api_content)
  } else {
    api_content <- NULL
  }

  return(api_content)
}
