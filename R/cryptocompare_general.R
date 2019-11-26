#' @title cryptocompare_general
#' @description function to obtain generic data on the world of cryptocurrencies, the supported methods concern:
#' tbl of information for all cryptocurrency \code{cryptocompare_general(search = "coin")},
#' tbl of all exchanges \code{cryptocompare_general(search = "exchanges")},
#' tbl gambling (search = "gambling"), \code{cryptocompare_general(search = "exchanges")},
#' tbl cards (search = "cards"), \code{cryptocompare_general(search = "exchanges")},
#' tbl mining contracts \code{cryptocompare_general(search = "contracts")},
#' tbl mining companies \code{cryptocompare_general(search = "companies")},
#' tbl mining equipment,\code{cryptocompare_general(search = "equipment")},
#' tbl mining pools \code{cryptocompare_general(search = "pools")},
#' @param search element to search
#' @param currency conversion currency
#' @param api_key  api key to access some endpoints
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @examples
#' \dontrun{cryptocompare_general(search = "coin")}
#' \dontrun{cryptocompare_general(search = "exchanges")}
#' \dontrun{cryptocompare_general(search = "gamling", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "wallets", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "cards", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "contracts", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "companies", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "equipment", api_key = yourapikey)}
#' \dontrun{cryptocompare_general(search = "pools", api_key = yourapikey)}
#'
#'@export

cryptocompare_general <- function(search = NULL, currency = "USD", api_key = NULL, quiet = FALSE){

  search.api <- NA
  search.out <- NA
  search.match <- NA
  search.paths <- NA
  search.query <- NA

  search.supported <- c(
    `all/coinlist` = "coin",
    `exchanges/general` = "exchanges",
    `gambling/general` = "gambling",
    `wallets/general` = "wallets",
    `cards/general` = "cards",
    `mining/contracts/general` = "contracts",
    `mining/companies/general` = "companies",
    `mining/equipment/general` = "equipment",
    `mining/pools/general` = "pools"
  )

  search.match <- match.arg(search, search.supported)

  if( !(search %in% c("coin", "exchanges")) ){
    stopifnot( is.null(api_key) )}


  search.paths <- c("data", names(search.match) )
  search.query <- list(tsym = currency, api_key = api_key)

  search.api <- Api("cryptocompare",paths = search.paths, quiet = quiet, query = search.query)

  search.out <- search.api$Data

  search.out <- map_tidy(search.out)

  return(search.out)

}



#' @title cryptocompare_rate_limit
#' @description check the rate limit for the month.
#' @examples
#' \dontrun{cryptocompare_rate_limit()}
#' @export

cryptocompare_rate_limit <- function(){

  search.api <- NA
  search.out <- NA

  search.api <- Api("cryptocompare",paths = c("stats", "rate","limit"))

  search.out <- map_tidy(search.api$Data)
  search.out <- tibble::add_column(search.out, call =  c("made", "left"), .before = "second")

  return(search.out)

}

