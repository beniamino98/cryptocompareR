#'@title cc_get_options
#'@description visualize all get options availables.
#'
#'@return a character vector with all option for the function \code{\link{cc_get}}
#'@name cc_get_options
#'@rdname cc_get_options
#'@export


cc_get_options <- function(){

  c("stock","exchange","social","news", "all_coins","all_exchanges", "all_gamblings","all_wallets","all_cards",
    "all_contracts","all_companies","all_equipment","all_pools")
}


#'@title cc_exchage_options
#'@description  visualize all exchanges availables.
#'
#'@return a character vector with all exchanges
#'@name cc_exchange_options
#'@rdname cc_exchange_options
#'@export

cc_exchange_options <- function(){
  cc_set_env(quiet = FALSE)
  return(cc_env$exchanges$InternalName)
}


#'@title cc_feed_options
#'@description  visualize all news feeds availables.
#'
#'@return a character vector with all feeds
#'@name cc_feed_options
#'@rdname cc_feed_options
#'@export

cc_feed_options <- function(){
  cc_set_env(quiet = FALSE)
  return(cc_env$news_feeds$key)
}

#'@title cc_category_options
#'@description  visualize all news categories availables.
#'
#'@return a character vector with all categories
#'@name cc_category_options
#'@rdname cc_category_options
#'@export

cc_category_options <- function(){
  cc_set_env(quiet = FALSE)
  return(cc_env$news_categories$categoryName)
}

#'@title cc_symbol_options
#'@description  visualize all symbols availables.
#'
#'@return a character vector with all symbols
#'@name cc_symbol_options
#'@rdname cc_symbol_options
#'@export

cc_symbol_options <- function(){
  cc_set_env(quiet = FALSE)
  return(cc_env$coins_id$Symbol)
}

#'@title cc_all_coins
#'@description  visualize all coins information availables.
#'
#'@return a tbl with info for all coins available
#'@name cc_all_coins
#'@rdname cc_all_coins
#'@export

cc_all_coins <- function(){
  cc_set_env(quiet = FALSE)
  return(cc_env$coins_id)
}

#'@title cc_all_exchanges
#'@description  visualize all exchanges information availables.
#'
#'@return a tbl with info for all exchanges available
#'@name cc_all_exchanges
#'@rdname cc_all_exchanges
#'@export

cc_all_exchanges <-  function(){

  cc_set_env(quiet = FALSE)
  return(cc_env$exchanges)

}

