#' @docType package
#' @name cryptocompareR
#' @description pacchetto
#' @import dplyr
#' @import purrr
#' @importFrom stringr str_trim

NULL

#' @title cc_set_environment
#' @description initialize the environment called "cryptocompare" in which are saved information such as the coins ID, the api key and the exchanges names.
#' @name cc_set_environment
#' @rdname cc_set_environment

cc_set_environment <- function(quiet = FALSE){

  # Create a new environment 
  new_env <- new.env(parent = .GlobalEnv)
  # Assign the new environment `cryptocompare` to .GlobalEnv
  assign("cryptocompare", value = new_env, envir = .GlobalEnv)
  # Safe function 
  safe_import <- purrr::safely(cc_general_info)
  # Get coins_id 
  coins_id <- safe_import("coins")$result
  # Get exchanges 
  exchanges <- safe_import("exchange")$result

  check_non_empty <- !purrr::is_empty(coins_id) & !purrr::is_empty(exchanges) 
  if (check_non_empty) {
    assign("coins_id", value = dplyr::select(coins_id, Id, Symbol), envir = cryptocompare)
    assign("exchanges", value = exchanges$InternalName, envir = cryptocompare)
    if (!quiet) {
      msg <- 'Environment "cryptocompare" created and added to .GlobalEnv!'
      cli::cli_alert_success(msg)
    }
  } 
}

#' @title cc_set_api_key
#' @description add a personal api key to the "cryptocompare" environment in such a way to avoid to inserting it manually in every function's call where it is needed.
#' @param api_key character
#' @examples
#' # Add the api key to the "cryptocompare" environment
#' yourapikey <- "yourapikey"
#' cc_set_api_key(api_key = yourapikey)
#'
#' @name cc_set_api_key
#' @rdname cc_set_api_key
#' @export

cc_set_api_key <- function(api_key = NULL, quiet = FALSE){
  # Check existence of "cryptocompare" environment 
  check_env <- exists("cryptocompare", envir = .GlobalEnv)
  if (check_env) {
    assign("api_key", api_key, envir = cryptocompare)
    if (!quiet){
      msg <- 'Api key added to the environment `cryptocompare`!'
      cli::cli_alert_success(msg)
    }
  } else {
    if (!quiet){
      msg <- 'Environment `cryptocompare` does not exists in `.GlobalEnv`!'
      cli::cli_alert_danger(msg)
    }
  }
}


#' @title cc_api_key
#' @description function to use the api key saved in the "cryptocompare" environment. In order to add it you can use the function \code{cc_set_api_key}
#' @param verbose logical, if TRUE the function display a warning if the Api Key was not found!
#' @name cc_set_api_key
#' @rdname cc_set_api_key
#' @return a character, if the api key was added in the "cryptocompare" environment, otherwise NULL.

cc_api_key <- function(quiet = FALSE){

  # Check existence of "cryptocompare" environment 
  check_env <- exists("cryptocompare", envir = .GlobalEnv)
  if (check_env) {
    # Search the api_key in "cryptocompare" environment 
    api_key <- cryptocompare$api_key
    check_api_key <- is.null(api_key)
    if (check_api_key) {
      if (!quiet){
        msg <- 'Provide an api key using the function `cc_set_api_key(yourapikey)`!'
        cli::cli_alert_danger(msg)
      }
    } else {
      return(api_key)
    }
  }
  return(NULL)
}

#' Cryptocompare Symbol Id 
#' 
#' Retrieve the id for a symbol.
#' 
#' @param symbol Character. 
#' 
#' @usage 
#' cc_symbol_id(symbol = NULL)
#' 
#' @return The symbol id if was found, otherwise `NULL`.
#' 
#' @examples
#' cc_symbol_id(symbol = "BTC")
#' cc_symbol_id(symbol = "ETH")
#' 
#' @export
#' 
#' @name cc_symbol_id
#' @rdname cc_symbol_id

cc_symbol_id <- function(symbol = NULL){
  # Check existence of "cryptocompare" environment 
  check_env <- exists("cryptocompare", envir = .GlobalEnv)
  if (check_env) {
    # Search the symbol in "cryptocompare" environment 
    idx_symbol_id <- which(cryptocompare$coins_id$Symbol == symbol)
    if (!purrr::is_empty(idx_symbol_id)) {
      return(cryptocompare$coins_id$Id[idx_symbol_id])
    } 
  }
  return(NULL)
}

#' Available Exchanges
#' 
#' Retrieve the names of all the exchanges available in cryptocompare Api.
#' 
#' @usage 
#' cc_all_exchanges()
#' 
#' @return `tibble`
#' 
#' @examples
#' cc_all_exchanges()
#' 
#' @export
#' 
#' @name cc_all_exchanges
#' @rdname cc_all_exchanges

cc_all_exchanges <- function(){
  # Check existence of "cryptocompare" environment 
  check_env <- exists("cryptocompare", envir = .GlobalEnv)
  if (check_env) {
    return(cryptocompare$exchanges)
  } 
  return(NULL)
}


