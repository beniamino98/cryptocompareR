#' @docType package
#' @name cryptocompareR
#' @description ciao sono un paccketto
#' @import dplyr
#' @import purrr
#' @importFrom stringr str_trim

NULL

#' @title cc_set_environment
#' @description initialize the environment called "cryptocompare" in which are saved information such as the coins ID, the api key and the exchanges names.
#' @name cc_set_environment
#' @rdname cc_set_environment

cc_set_environment <- function(){

  new_env = new.env(parent = .GlobalEnv)

  assign("cryptocompare", value = new_env, envir = .GlobalEnv)

  safe_import <- purrr::safely(cc_maket_info)

  coins_Id <- safe_import("coins")$result

  exchange <- safe_import("exchange")$result

  if(!is.null(coins_Id) & !is.null(exchange)){

    assign("coins_Id", value = dplyr::select(coins_Id, Id, Symbol), envir = cryptocompare)
    assign("exchanges", value = exchange$InternalName, envir = cryptocompare)
    message('New environment ', '"cryptocompare"', ' created and added to .GlobalEnv!')
  } else {
    load("data/cryptocompare.RData", envir = .GlobalEnv)
    warning('Old environment ', '"cryptocompare"', ' added to .GlobalEnv!')
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

cc_set_api_key <- function(api_key = NULL){

  if(exists("cyptocompare", envir = .GlobalEnv)){
    assign("api_key", api_key, envir = get(cryptocompare))
    message('Api key added to the environment ', '"cryptocompare"', ' and saved!')
  } else {
    warning('"cryptocompare"', " environment does not exists in the .GlobalEnv")
  }
}



#' @title cc_api_key
#' @description function to use the api key saved in the "cryptocompare" environment. In order to add it you can use the function \code{cc_set_api_key}
#' @param verbose logical, if TRUE the function display a warning if the Api Key was not found!
#' @name cc_set_api_key
#' @rdname cc_set_api_key
#' @return a character, if the api key was added in the "cryptocompare" environment, otherwise NULL.

cc_api_key <- function(verbose = FALSE){

  if(exists("cyptocompare", envir = .GlobalEnv)){

    my_api_key <- cryptocompare$api_key

    if(is.null(my_api_key)){
      if(verbose) warning('Provide an api key using the function "cc_set_api_key" or insert it manually!')
      return(NULL)
    } else {
      return(my_api_key)
    }
  }
  return(NULL)
}



#' @title cc_symbol_id
#' @description retrieve the Id using the symbol, useful for uniforming the social function with the others.
#' @param symbol character, the symbol for which we would like to know the Id.
#' @name cc_symbol_id
#' @rdname cc_symbol_id
#' @examples
#' cc_symbol_id(symbol = "BTC")
#' cc_symbol_id(symbol = "ETH")
#'
#' @return a character, the symbol ID, if it was found in the "cryptocompare$coins_Id" dataset, otherwise NULL.

cc_symbol_id <- function(symbol = NULL){

  if(exists("cyptocompare", envir = .GlobalEnv)){

    Id <- dplyr::filter(cryptocompare$coins_Id, Symbol == symbol)$Id

    if(!purrr::is_empty(Id)){
      return(Id)
    }
  }
  return(NULL)
}


#' @title cc_all_exchanges
#' @description retrieve the names of all the exchanges available in cryptocompare Api.
#' @name cc_all_exchanges
#' @rdname cc_all_exchanges
#' @return character vector
#' @examples
#' cc_all_exchanges()
#' @export

cc_all_exchanges <- function(){

  if(exists("cyptocompare", envir = .GlobalEnv)){
    return(cryptocompare$exchanges)
  }

  return(NULL)

}
