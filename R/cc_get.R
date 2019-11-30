#' @title cc_get
#' @description cc_get is the basic function in cryptocompareR for importing data from cryptocompare'Api.
#' the param "get" allows you to specify the method that you want use, see \code{\link{cc_get_options}} for more details.
#' The Data were provided from  \href{https://www.cryptocompare.com}{Cryptocompare}.
#'
#' @param search the element to
#' @param get Default "stock", others available:
#'         \itemize{
#'         \item `"stock"`: Get the open, high, low, volume and adjusted stock prices for a cyptocurrency symbol.
#'         \item `"exchange"`: Get the exchange values for a cyptocurrency symbol and for exchanges.
#'         \item `"social"` : Get the social data for a cyptocurrency symbol.
#'         \item `"news"`: Get the news for feed and categories.
#'         \item `"all_methods"`: includes{ "all_coins", "all_exchanges", see the function  \code{\link{cc_get_options}} }
#'          }
#' @param from start date for import.
#' @param to end date for import.
#' @param ... other  arguments depending on the type of method used
#' \itemize{
#'    \item `"interval"`:  teemporal interval between the data can be:
#'              \itemize{
#'               \item `"day"`: daily data  (with "from", default if missing 2008-01-01)
#'               \item `"hour"`: ("stock","exchanges", "social") horuly data  (with "from", default if missing 2014-01-01)
#'               \item `"minute"`: (just "stock") minute data for the last 7 days.
#'             }
#'
#'    \item `"exchange"`: just for "stock" and "exchange" method.
#'    \item `"category"`: just for "news" method. The feed is specifyied in the search param.
#'    \item `"currency"`: Defaul USD.
#'
#' }
#'@name cc_get
#'@rdname cc_get
#'@return Returns data in the form of a `tibble` object.
#'@seealso
#'\itemize{
#'  \item   \code{\link{cc_all_coins}}
#'  \item   \code{\link{cc_all_exchanges}}
#'  \item   \code{\link{cc_get_options}}
#'  \item   \code{\link{cc_exchange_options}}, to get a full list of exchanges available.
#'  \item   \code{\link{cc_feed_options}} , to see all news feeds availables.
#'  \item   \code{\link{cc_category_options}} to see all news categories availables.
#'  \item   \code{\link{cc_use_key}} to set the api key for collecting some general data and social data.
#'  \item   \code{\link{cc_rate_limit}}
#'}
#'
#' @param api_key   cryptocompare api key. You can set it with the function \code{\link{cc_use_key}} or
#' if you insert one time the api key in the function, you do not need to insert it again until you restart R or
#' clean the `"cc_env"` with the function \code{\link{cc_clean_env}}.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#'
#' @examples
#'
#'  cc_get("BTC",get = "stock", from = "2019-01-01", to = "2019-11-01")
#'
#'  cc_get(get ="all_coins")
#'
#' # multiple stock method by exhcange
#' \donttest{ cc_get(search = c("BTC","ETH"), get = "stock", exchange = "Coinbase") }
#' \donttest{ cc_get(search = c("ETH","BTC"), get = "stock ", exchange = c("Coinbase","LocalBitcoins")) }
#'
#' # exchange method for exchanges
#' \donttest{ cc_get(get = "exchange", exchange = c("Coinbase","LocalBitcoins")) }
#'
#'  # exchange method for symbols and exchanges
#' \donttest{ cc_get(c("ETH","XRP"),get = "exchange", exchange = c("Coinbase","LocalBitcoins")) }
#'
#'  # exchange method for all the market
#' \donttest{ cc_get(get = "exchange") }
#'
#' # social method
#' \donttest{ cc_get(c("BTC","ETH"),get = "social", from = "2019-01-01", to = "2019-11-01" ) }
#'
#' # general method
#' \donttest{ cc_get(get ="all_cards") }
#' \donttest{ cc_get(get ="all_contracts") }
#' \donttest{ cc_get(get ="all_wallets") }
#'
#' # news method
#' \donttest{ cc_get(get = "news", from = "2019-01-01", to = "2019-01-05") }
#' \donttest{ cc_get(search = "yahoo", get = "news", category = "Trading") }
#'
#' @export



cc_get <- function( search = NULL, get = "stock",from = NULL, to = NULL, api_key = NULL, quiet = FALSE, ... ) {

  cc_set_env(api_key = api_key, quiet = T)

  supported.methods <- match.arg(get, cc_get_options() )

  if(supported.methods %in% c("stock","exchange","social","general","news")){

     fun.call <- paste("cc_get", supported.methods, sep = "_")
     fun.out <- do.call(fun.call, args = list(search = search, from = from, to = to, api_key = api_key, quiet = quiet,...))

  } else {
    fun.call <- cc_get_general
    fun.out <- do.call(fun.call, args = list(search = get,...))
  }


  return(fun.out)
}

