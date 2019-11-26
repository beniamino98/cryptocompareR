## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 8, 
                      fig.height = 4.5,
                      fig.align = 'center',
                      out.width='95%', 
                      dpi = 200)


## ------------------------------------------------------------------------
 library(cryptocompareR)

# all exchanges available 
 all_exchanges()

# all symbols  avaliable 
 all_symbols()


## ---- message = FALSE, echo=FALSE----------------------------------------

# A)  using cryptocompare generic function

# stock for multiple symbols in a specific interval of date 
cryptocompare( search = c("BTC","ETH", "XRP"),method = "stock", from = "2019-01-01",
               to = "2019-06-01", quiet = TRUE)

# filtering for exchange 
cryptocompare( search = c("BTC","ETH", "XRP"),method = "stock", from = "2019-01-01",
               to = "2019-06-01", exchange = "Coinbase", quiet = TRUE)


# B)  you can use the cryptocompare_stock method 
 
# stock for multiple symbols in a specific interval of date 
cryptocompare_stock( search = c("BTC","ETH", "XRP"), from = "2019-01-01",
               to = "2019-06-01", quiet = TRUE)

# filtering for exchange 
cryptocompare_stock( search = c("XRP","ETH", "BTC"),from = "2019-01-01",
               to = "2019-06-01", exchange = "Coinbase", quiet = TRUE)




## ------------------------------------------------------------------------

# using cryptocompare generic fun 

# market data 
cryptocompare(method = "exchange", quiet = TRUE)

# searching exchange data for some symbols 
cryptocompare(c("BTC","ETH", "XRP"),method = "exchange", quiet = TRUE)

# searching exchange data for a specific exchange 
cryptocompare(method = "exchange", exchange = c("Coinbase","Yobit"), quiet = TRUE)

# searching exchange data for a specific symbol and exchange 
cryptocompare(search = c("BTC","ETH", "XRP"),
              method = "exchange", exchange = c("Coinbase","Yobit"), quiet = TRUE)





## ------------------------------------------------------------------------

# social data for a symbol in an interval of dates 
# 
# cryptocompare(search = "ETH", method = "social", api_key = yourapikey, 
# from = "2019-06-01", to = "2019-11-10", quiet = T)

# social data for a group of symbols in an interval of dates 
# cryptocompare(search = c("BTC", "ETH"), method = "social", api_key = yourapikey, 
# from = "2019-06-01", to = "2019-11-10", quiet = T)
# 
# all social data availables for a group of symbols 
# cryptocompare(search = c("BTC", "ETH"), method = "social", api_key = yourapikey, quiet = T)

## ------------------------------------------------------------------------

# all news in an interval of dates 
cryptocompare(method = "news", from = "2019-11-01", to = "2019-11-10", quiet = T)


## ------------------------------------------------------------------------

# all categories available 
 all_categories()

# all feeds avaliable 
 all_feeds()


## ------------------------------------------------------------------------

# all news in an interval of dates searching by a specific feed   
cryptocompare(search = "yahoo", method = "news", from = "2019-06-01", to = "2019-11-10", quiet = T)


## ------------------------------------------------------------------------
# all news in an interval of dates searching by a specific category 
cryptocompare(method = "news", categories = "BTC", from = "2019-11-01", to = "2019-11-10", quiet = T)


## ------------------------------------------------------------------------
# all news in an interval of dates searching by a specific feed  and categories 
cryptocompare(search = "yahoo", method = "news", categories = "BTC", from = "2019-06-01", to = "2019-11-10", quiet = T)

## ------------------------------------------------------------------------
# coin list 

cryptocompare(search = "coin", method = "general", quiet = T)


#  all exchanges info 

cryptocompare(search = "exchanges", method = "general", quiet = T)



## ------------------------------------------------------------------------

# all gambling info 

# cryptocompare(search = "gambling", method = "general", quiet = T, api_key = yourapikey)


# all wallets info 

# cryptocompare(search = "wallets", method = "general", quiet = T, api_key = yourapikey)


# all cards info 

# cryptocompare(search = "cards", method = "general", quiet = T, api_key = yourapikey)


# all mining contracts 

# cryptocompare(search = "contracts", method = "general", quiet = T, api_key = yourapikey)


# all mining companies 

# cryptocompare(search = "companies", method = "general", quiet = T,api_key = yourapikey)


# all mining equipment 

# cryptocompare(search = "equipment", method = "general", quiet = T, api_key = yourapikey)


# all mining pools

# cryptocompare(search = "pools", method = "general", quiet = T, api_key = yourapikey)




