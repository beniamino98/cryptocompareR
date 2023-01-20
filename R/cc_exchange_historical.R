#' @title cc_exchange_historical
#' @description
#' @param symbol character, the symbol of interest.
#' @param start character or Date, the start date for importation.
#' @param end character or Date, the end date for importation.
#' @param exchange character, the exchange in which retrieve the informations, if NULL it will be returned a mean between all the exchanges and denoted with "global".
#' @param currency character, the currency in which convert the output.
#' @param interval character, the frequency of the data, it can be "daily", "hourly".
#' @param api_key character
#' @return a tibble, with 7 columns (Date, Symbol, Currency, Exchange, VolumeTo, VolumeFrom, Volume)
#' @name cc_exchange_historical
#' @rdname cc_exchange_historical
#' @export

cc_exchange_historical <- function(symbol = NULL, start = NULL, end = NULL, exchange = NULL, currency = "USD", interval = c("daily", "hourly"), api_key = NULL ){

  # Match the Interval
  interval <- match.arg(interval, choices = c("daily", "hourly"))

  # Default End Date
  if(is.null(end)){
    end_date <- Sys.time()
  } else {
    end_date <- end
  }

  # Default Start Date
  if(is.null(start)){
    start_date <- as.POSIXct("2010-01-01 12:00:00 CEST")
  } else {
    start_date <- start
  }

  # Conversion depending on the interval
  if(interval == "daily"){

    end_date   <- as.Date(end_date)
    start_date <- as.Date(start_date)

  } else {

    end_date   <- as.POSIXct(end_date, start = "1970-01-01")
    start_date <- as.POSIXct(start_date, start = "1970-01-01")
  }

  # Loop to get all the data
  i = 1
  last_date = end_date
  historical_data <- list()

  while(as.Date(start_date) < as.Date(last_date)){

    # save version to avoid errors
    safe_import <- purrr::safely(cc_api_exchange_historical)

    historical_data[[i]] <- safe_import(symbol = symbol, end = last_date, exchange = exchange, currency = currency, interval = interval, api_key = api_key)$result

    # break conditions
    check_conditions <- is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0 || sum(historical_data[[i]][1,] == 0) > 7

    if(check_conditions){
      break
    }

    # Use last Date to continue the Calls till start date is reached
    last_date <- historical_data[[i]]$Date[1]

    if(last_date < start_date ){
      break
    } else {
      i <- i + 1
    }
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

  # Different columns names:
  if(!is.null(symbol) & is.null(exchange)){

    colnames(historical_data) <- c("Date", "Symbol", "Currency",
                                   "Exchange", "Tier_Vol_Quote", "Tier_Vol_Base",
                                   "Tier_Vol_Total", "Agg_Vol_Quote", "Agg_Vol_Base", "Agg_Vol_Total",
                                   "Tot_Vol_Quote", "Tot_Vol_Base", "Volume")

  } else if(is.null(symbol) & !is.null(exchange)){

    colnames(historical_data) <- c("Date", "Symbol", "Currency",
                                   "Exchange", "Volume")

  } else if(!is.null(symbol) & !is.null(exchange)){

    colnames(historical_data) <- c("Date", "Symbol", "Currency",
                                   "Exchange", "VolumeTo", "VolumeFrom", "Volume")

  }
  return(historical_data)
}



# Single Function for comunicating with the API: it will import the 2000 observatios before the "end" parameter specified.
# For example setting: end = "2019-01-01" and interval daily will give as output the observations till "2013-07-11"
cc_api_exchange_historical <- function(symbol = NULL, end = NULL, exchange = NULL, currency = "USD", interval = c("daily", "hourly"), api_key = NULL){

  # Matching arguments for the API call
  interval <- match.arg(interval, choices = c("daily", "hourly"))
  api_interval <-  dplyr::case_when(interval == "daily" ~ "histoday",
                                    interval == "hourly" ~ "histohour")

  # default End Date:
  if(interval == "daily"){

    if(is.null(end)){
      end_date <- as_unix(Sys.Date())
    } else {
      end_date <- as_unix(as.Date(end))
    }

  } else {

    if(is.null(end)){
      end_date <- as_unix(Sys.time())
    } else {
      end_date <- as_unix(as.POSIXct(end))
    }

  }

  # Different Path:
  if(is.null(symbol)){

    # Only Exchange volume for aggregated Symbols
    api_paths <- c("data", "exchange", api_interval)
    api_query <- list(tsym = currency, e = exchange, toTs = end_date, limit = 2000, api_key = api_key)

  } else if(is.null(exchange)){

    # Only Symbol volume for aggregated Exchanges
    api_paths <- c("data", "symbol", api_interval)
    api_query <- list(fsym = symbol, tsym = currency, toTs = end_date, limit = 2000, api_key = api_key)

  } else{

    # Symbol volume for a specific Exchange
    api_paths <- c("data", "exchange", "symbol", api_interval)
    api_query <- list(fsym = symbol, tsym = currency, e = exchange, toTs = end_date, limit = 2000, api_key = api_key)
  }

  # GET call
  api_response <- cryptocompare_api(path = api_paths, query = api_query)

  # Create the Dataset and cleaning
  api_response <- api_response$Data                        # selecting the list contining the data of interest
  api_response <- api_response[!duplicated(api_response),] # remove duplicated for safety
  api_response <- dplyr::as_tibble(api_response)           # conversion to tibble


  # standardize variables: add Symbol, Currency, Exchange and convert the Date
  api_response <- dplyr::mutate(api_response,
                                Symbol   = ifelse(is.null(symbol), "global", symbol),
                                Date     = as.POSIXct(time, origin = "1970-01-01" ),
                                Currency = currency,
                                Exchange = ifelse(is.null(exchange), "global", exchange)

  )

  # Reorder the variables
  api_response <- dplyr::select(api_response, Date, Symbol, Currency, Exchange, dplyr::everything())
  api_response <- dplyr::select(api_response, -time)

  # conversion in daily dates if daily interval
  if(interval == "daily"){
    api_response <- dplyr::mutate(api_response, Date = as.Date(Date))
  }

  # Arrange with respect to Date (descending)
  api_response <- dplyr::arrange(api_response, Date)

  return(api_response)

}

