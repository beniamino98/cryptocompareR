#' @title cryptocompare
#' @description cryptocompare generic function, see the vignette C01 - Importing Data with cryptocompareR for more details.
#' @param search reference element to search.
#' @param method the method that you want to use available: "stock","exchange","social","general","news","rate_limit",
#'
#' @param interval temporal interval between the data  ("day", "hour", "minute"). DEFAULT "day"
#' @param exchange reference exchange that you want to use to acquire data, see the \code{all_exchanges} function.
#'                If NULL, the data will be imported for all exchanges.
#'
#' @param currency reference currency that you want to use. DEFAUL = "USD"
#' @param from start date for import.
#' @param to end date for import.
#' @param categories reference categories for news method,  see the \code{all_categories} function.
#'                   If NULL, the news will be imported for all categories.
#' @param lang reference languages for news method, DEFAULT = "EN".
#' @param api_key   cryptocompare api key.
#' @param quiet logical. When TRUE function evalueates without displaying customary messages.
#' @examples
#'
#'
#' \dontrun{cryptocompare( search = c("BTC","ETH", "XRP"),method = "stock",
#'          from = "2019-01-01", to = "2019-06-01", quiet = TRUE)}
#'
#' \dontrun{cryptocompare(search = c("BTC","ETH", "XRP"), method = "exchange",
#'           exchange = c("Coinbase","Yobit"), quiet = TRUE)}
#'
#' \dontrun{ cryptocompare(search = c("BTC", "ETH"), method = "social", api_key = yourapikey,
#'            from = "2019-06-01", to = "2019-11-10", quiet = T)}
#'
#'
#' \dontrun{cryptocompare(method = "news", from = "2019-11-01", to = "2019-11-10", quiet = T)}
#'
#' @export

cryptocompare <- function(search = NULL, method = "stock", interval = "day",
                          exchange = NULL, currency = "USD", from = NULL, to = NULL,
                          categories = NULL, lang = "EN", api_key = NULL, quiet = FALSE ) {

  args.list <- NA
  fun.call  <- NA
  fun.out   <- NA

  supported.methods <- match.arg(method, c("stock","exchange","social","general","news","rate_limit") )

  fun.call <- paste("cryptocompare", supported.methods, sep = "_")


  if(supported.methods %in% c("stock", "exchange")){
    args.list <- list(
      search = search,  interval = interval, exchange = exchange,
      currency = currency, from = from, to = to, api_key = api_key, quiet = quiet
    )

  } else if(supported.methods == "social" ){

    args.list <- list( search = search,  interval = interval, from = from, to = to, api_key = api_key, quiet = quiet  )

  } else if(supported.methods == "general" ){

    args.list <- list(search = search, currency = currency, api_key = api_key, quiet = quiet  )

  } else if(supported.methods == "news" ){

    args.list <- list( search = search, categories = categories, from = from,
                       to = to, lang = lang, api_key = api_key, quiet = quiet  )

  } else if(supported.methods == "rate_limit" ){

    args.list <- list()

  }

  fun.out <- do.call(fun.call, args = args.list)

  return(fun.out)
}



