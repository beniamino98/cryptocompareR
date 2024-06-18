library(tidyverse)

source("R/cryptocompare.R")
source("R/cc_market_info.R")
source("R/cryptocompare_api.R")

# rm(list = ls())

# Create `cryptocompare` environment 
cc_set_environment()
cryptocompare$coins_id
cryptocompare$exchanges

# Check output when api_key = NULL
cc_set_api_key(api_key = NULL)
cc_api_key()
# Set an `api_key`
cc_set_api_key(api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813")
cc_api_key()
# Remove the `api_key`
cc_set_api_key(api_key = NULL)
cc_api_key()

# Get symbol_id 
cc_symbol_id("BTC")
cc_symbol_id("LTC")
cc_symbol_id("WLD")

# Get all exchanges
cc_all_exchanges()



# -------------------------------- cc_orderbook --------------------------------
api_key <- "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813"
cc_orderbook_exchange(api_key)
cc_L1_order_book("BTC", "USDT", exchange = "Binance", api_key)
cc_order_book_L2(api_key)

cc_orderbook_exchange(api_key = api_key)
cc_L1_order_book("BTC", currency = "USDT",exchange = "Binance", api_key = api_key)
cc_order_book_L2(api_key = api_key)

cc_eth_staking(start = "2020-01-01", end = Sys.Date(), api_key = api_key)




cc_blockchain_historical(symbol = "BTC", start = "2017-01-01", end = "2023-01-01", api_key = api_key)
cc_blockchain_historical(symbol = "ETH", start = "2017-01-01", end = "2023-01-01", api_key = api_key)
cc_blockchain_historical(symbol = "DOGE", start = "2017-01-01", end = "2023-01-01", api_key = api_key)

