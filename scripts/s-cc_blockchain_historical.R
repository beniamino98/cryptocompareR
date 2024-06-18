
rm(list = ls())

symbol = "BTC"
end = "2023-01-01"
api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813"

source("R/cc_blockchain_historical.R")
cc_blockchain_historical(symbol = "BTC", start = "2017-01-01", end = "2023-01-01", api_key = api_key)
cc_blockchain_historical(symbol = "ETH", start = "2017-01-01", end = "2023-01-01", api_key = api_key)
cc_blockchain_historical(symbol = "DOGE", start = "2017-01-01", end = "2023-01-01", api_key = api_key)

source("R/as_unix.R")
source("R/cryptocompare_api.R")


# Default Parameters
end_date <- ifelse(is.null(end), as.character(Sys.Date()), as.character(end))
end_date <- as.Date(end_date)
end_date <- as_unix(end_date)

# Path, Query & Response
api_path <- c("data", "blockchain", "histo", "day")
api_query <- list(fsym = symbol, toTs = end_date, limit = 2000, api_key = api_key)

api_response <- cryptocompare_api(path = api_path, query = api_query)

if(is.null(api_response)){
  warning("Errors in cyptocompare_api function")
  return(NULL)
}

# cleaning
api_response <- api_response$Data$Data
api_response <- api_response[!duplicated(api_response),] # remove duplicated for safety
api_response <- dplyr::as_tibble(api_response)           # conversion to tibble

# create new variables and convert time index
api_response <- dplyr::mutate(api_response,
                              Symbol = symbol,
                              Id = id,
                              Date = as.POSIXct(time, origin = "1970-01-01"))

# reorder and rename the variables
api_response <- dplyr::select(api_response, Date, Symbol, dplyr::everything())
api_response <- dplyr::select(api_response, -time, -id, -symbol)
api_response <- dplyr::mutate(api_response, Date = as.Date(Date))
api_response <- dplyr::arrange(api_response, Date)
api_response

api_response$Date[nrow(api_response)]

api_response$Date[1]
