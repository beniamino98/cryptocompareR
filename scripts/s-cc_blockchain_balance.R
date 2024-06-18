api_key = "153bc970c37d2a4d8e6735f89d5602b7787f9765e0add94ad960ff91b0326813"
to = "2022-12-24"

source("R/as_unix.R")
source("R/cryptocompare_api.R")


# ---------------------------------------- cc_api_blockchain_balance ---------------------------------------- 
# actually this endpoint is available only for BTC
symbol <- "BTC"
api_path <- c("data", "blockchain", "balancedistribution")

# API path and query
if (is.null(to)) {
  api_path <- c(api_path, "latest")
  api_query <- list(fsym = symbol, api_key = api_key)
  to_date <- NULL
} else {
  to_date <- as_unix(as.Date(to))
  api_path <- c(api_path, "histo", "day")
  api_query <- list(fsym = symbol, toTs = to_date, limit = 2000, api_key = api_key)
}

api_response <- cryptocompare_api(path = api_path, query = api_query)

# warning message
check_empty <- purrr::is_empty(api_response)
if (check_empty) {
  cli::cli_alert_danger("Error in GET call, response is empty")
  return(NULL)
}

# Create date variable 
if (is.null(to_date)) {
  api_response <- api_response$Data$balance_distribution
  api_response$date <- Sys.Date()
} else {
  api_response <- api_response$Data$Data
  api_response <- purrr::map2_df(api_response$balance_distribution,
                                 api_response$time, ~dplyr::bind_cols(.x, date = as.POSIXct(.y, origin = "1970-01-01")))
}

# Convert to tibble 
api_response <- dplyr::as_tibble(api_response)
api_response$symbol <- symbol
# Reorder and rename columns
colnames(api_response) <- c("to", "from", "amount", "wallets", "date", "symbol")
idx_columns <- c("date", "symbol", "from", "to", "amount", "wallets")
api_response <- api_response[,idx_columns]
# Arrange by date (ascending)
idx_order <- order(api_response$date, decreasing = FALSE)
api_response <- api_response[idx_order,]

api_response
# --------------------------------------------------------------------------------------------------------------

# -------------------------------------------------- Test ------------------------------------------------------
source("R/cc_blockchain_balance.R")
cc_api_blockchain_balance("2022-12-24", api_key = api_key)
cc_blockchain_balance("2022-12-24", "2022-12-29", api_key = api_key)
