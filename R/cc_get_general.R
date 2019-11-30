#' @title cc_get_general
#' @description function to obtain generic data on the world of cryptocurrencies, the supported methods concern:
#'
#' \itemize{
#'   \item `"all_coins"`: tbl of information for all cryptocurrency.
#'   \item `"all_exchanges"`: tbl of all exchanges.
#'   \item `"all_gamblings"`: tbl with all gamblings.
#'   \item `"all_wallets"`: tbl with all wallets.
#'   \item `"all_contracts"`: tbl with all mining contracts.
#'   \item `"all_companies"`: tbl with all mining companies.
#'   \item `"all_equipment"`: tbl with all mining equipment.
#'   \item `"all_pools"`: tbl with all mining pools.
#' }
#' @param search element to search
#' @param currency conversion currency
#' @param api_key  api key to access some endpoints
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @return tbl
#' @seealso
#' \itemize{
#'   \item \code{\link{cc_get_options}}
#'   \item \code{\link{cc_all_coins}}:  tbl of information for all cryptocurrency.
#'   \item \code{\link{cc_all_exchanges}}: tbl of all exchanges.
#'   }
#' @name cc_get_general
#' @rdname cc_get_general
#' @examples
#' \donttest{cc_get_general(search = "all_coins")}
#' \donttest{cc_get_general(search = "all_exchanges")}
#' \donttest{cc_get_general(search = "all_gamlings", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_wallets", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_cards", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_contracts", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_companies", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_equipment", api_key = yourapikey)}
#' \donttest{cc_get_general(search = "all_pools", api_key = yourapikey)}
#'


cc_get_general <- function(search = NULL, currency = "USD", quiet = FALSE,  api_key = NULL ){

  search.api <- NA
  search.out <- NA
  search.match <- NA
  search.paths <- NA
  search.query <- NA

  api_key <- cc_use_key(x = api_key)

  search.supported <- c(
    `all/coinlist` = "all_coins",
    `exchanges/general` = "all_exchanges",
    `gambling/general` = "all_gamblings",
    `wallets/general` = "all_wallets",
    `cards/general` = "all_cards",
    `mining/contracts/general` = "all_contracts",
    `mining/companies/general` = "all_companies",
    `mining/equipment/general` = "all_equipment",
    `mining/pools/general` = "all_pools"
  )

  search.match <- match.arg(search, search.supported)

  if( !(search %in% c("all_coins", "all_exchanges")) ) stopifnot( !is.null(api_key) )


  search.paths <- c("data", names(search.match) )
  search.query <- list(tsym = currency, api_key = api_key)

  search.api <- Api("cryptocompare",paths = search.paths, quiet = quiet, query = search.query)

  search.out <- search.api$Data

  search.out <- map_tidy(search.out, binded = TRUE)

  return(search.out)

}





