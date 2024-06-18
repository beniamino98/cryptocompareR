#' @title cc_price_historical
#' @description Get the Open, High, Low, Close, Volume, Open-Close change (OC) and High-Low change (HL) for the specified symbol.
#' @param symbol A string or character vector containing the symbols of interest. (e.g. "BTC", "ETH", "BNB",...)
#' @param start A Date or character vector containing  the start date for importation.
#' @param end  A Date or character vector containing the end date for importation.
#' @param exchange A string or character vector containing the exchange to use, if NULL it will be returned a mean between all the exchanges and denoted with "global".
#' @param currency A string or character vector containing the currency in which convert the output (e.g. "USD", "USDT", "BNB", "EUR",...).
#' @param interval  A string or character vector containing the frequency of the data, it can be "daily", "hourly" or "minutely". The access to minutely data is restricted to the last 7 days, even with the free api key.
#' @param api_key character containing your api key.
#' @param verbose logical
#' @examples
#' # Daily Data from General Exchange for the Pair BTC-USD
#' cc_price_historical(symbol = "BTC", start = "2022-01-01", end = "2023-01-01",
#'                     exchange = NULL, currency = "USD", interval = "daily")
#'
#' # Hourly Data from General Exchange for the Pair BTC-USD
#' cc_price_historical(symbol = "BTC", start = "2022-01-01 00:00:00", end = "2023-01-01 00:00:00",
#'                     exchange = NULL, currency = "USD", interval = "hourly")
#'
#' # Daily Data from Binance Exchange for the Pair BTC-BUSD
#' cc_price_historical(symbol = "BTC", start = "2022-01-01", end = "2023-01-01",
#'                     exchange = "Binance", currency = "BUSD", interval = "daily")
#'
#' # Daily Data from Kucoin Exchange for the Pair BTC-USDT
#' cc_price_historical(symbol = "BTC", start = "2022-01-01", end = "2023-01-01",
#'                     exchange = "Kucoin", currency = "USDT", interval = "daily")
#' @name cc_price_historical
#' @rdname cc_price_historical
#' @return tibble, with 12 columns (Date, Symbol, Currency, Exchange, High, Low, Open, Volume, Close, Adj, OC, HL)
#' @export

cc_price_historical <- function(symbol, start = NULL, end = NULL, exchange = NULL, currency = "USD",
                                interval = c("daily", "hourly", "minutely"), api_key = NULL, verbose = TRUE ) {

  # Match the Interval
  interval <- match.arg(interval, choices = c("daily", "hourly", "minutely"))

  # Default End Date
  if(is.null(end)){
    end_date <- Sys.time()
  } else {
    end_date <- end
  }

  # Default Start Date
  if (is.null(start)) {
    start_date <- as.POSIXct("2010-01-01 12:00:00 CEST")
  } else {
    start_date <- start
  }

  # Conversion and Default Values depending on the "interval"
  if (interval == "daily") {
    end_date <- as.Date(end_date)
    start_date <- as.Date(start)
  } else if (interval == "hourly") {
    end_date <- as.POSIXct(end_date, start = "1970-01-01")
    start_date <- as.POSIXct(start_date, start = "1970-01-01")
  } else if (interval == "minutely" & is.null(api_key)) {
    end_date <- Sys.time()
    start_date <- end_date - 7*23*60*60
  } else {
    end_date <- as.POSIXct(end_date, start = "1970-01-01")
    start_date <- as.POSIXct(start_date, start = "1970-01-01")
  }

  # Loop to get all the data
  i <- 1
  last_date <- end_date
  historical_data <- list()
  while(as.Date(start_date) < as.Date(last_date)){

    # save function to avoid interruptions if an error occur
    safe_import <- purrr::safely(cc_api_price_historical)
    historical_data[[i]] <- safe_import(symbol = symbol, end = last_date, exchange = exchange, 
                                        currency = currency, interval = interval, api_key = api_key)$result
    
    # Control for Minutely Data:
    if(purrr::is_empty(historical_data)){

      if(interval == "minutely"){
        warn_msg <- "Your Api Key allow just for the last 7 days for minutely data!\nConsider to upgrade your plan visiting: https://min-api.cryptocompare.com/pricing"

        if (verbose) {
          warning(warn_msg)
        }

        end_date <- Sys.time()
        start_date <- end_date - 7*23*60*60
        last_date <- end_date
        next
      }
    }

    # break control
    if(is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0){break}

    # update the last date
    last_date = historical_data[[i]]$Date[1]
    i <- i + 1
  }

  # Create the dataset
  historical_data <- dplyr::bind_rows(historical_data)
  historical_data <- unique(historical_data)
  historical_data <- dplyr::arrange(historical_data, Date)

  # remove potentially repeated observations
  historical_data <- dplyr::filter(historical_data, Date >= start_date & Date <= end_date )

  # convert in Date if daily
  if(interval == "daily"){
    historical_data$Date <- as.Date(historical_data$Date)
  }

  return(historical_data)

}


# Single Function for comunicating with the API: it will import the 2000 observatios before the "end" parameter specified.
# For example setting: end = "2019-01-01" and interval daily will give as output the observations till "2013-07-11"
cc_api_price_historical <- function(symbol = NULL, end = NULL, exchange = NULL, currency = "USD",
                                    interval = c("daily", "hourly", "minutely"), api_key = NULL){

  # Matching arguments for the API call
  api_interval <- dplyr::case_when(interval == "daily" ~ "histoday",
                                   interval == "hourly" ~ "histohour",
                                   interval == "minutely" ~ "histominute")

  # default starts and end [daily]
  if(interval == "daily"){

    if(is.null(end)){
      end_date <- as_unix(Sys.Date())
    } else {
      end_date <- as_unix(as.Date(end))
    }

  } else if(interval %in% c("hourly", "minutely")){

    if(is.null(end)){
      end_date <- as_unix(Sys.time())
    } else {
      end_date <- as_unix(as.POSIXct(end))
    }
  }

  # Path & Query
  api_paths <- c("data", "v2", api_interval)
  api_query <- list(fsym = symbol, tsym = currency, e = exchange, toTs = end_date, limit = 2000, api_key = api_key)

  # GET Call
  api_response <- cryptocompare_api(path = api_paths, query = api_query)

  # Create the Dataset and cleaning
  api_response <- api_response$Data$Data                   # selecting the list contining the data of interest
  api_response <- api_response[!duplicated(api_response),] # remove duplicated for safety
  api_response <- dplyr::as_tibble(api_response)           # conversion to tibble

  # create new variables and convert time index
  api_response <- dplyr::mutate(api_response,
                                Symbol   = symbol,
                                Date     = as.POSIXct(time, origin = "1970-01-01" ),
                                Currency = currency,
                                Exchange = ifelse(is.null(exchange), "global", exchange),

                                Adj = (high + close + open +  low)/2,
                                HL  = (high - low)/low,
                                OC  = (close - open)/open
  )

  # reorder and rename the variables
  api_response <- dplyr::select(api_response,
                                Date, Symbol, Currency, Exchange,
                                high = "high", Low = "low", Open = "open", Volume = "volumefrom", Close = "close",
                                Adj, OC, HL)

  # conversion in daily dates if daily interval
  if(interval == "daily"){
    api_response <- dplyr::mutate(api_response, Date = as.Date(Date))
  }

  api_response <- dplyr::arrange(api_response, Date)

  return(api_response)

}

