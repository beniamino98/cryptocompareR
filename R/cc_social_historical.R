#' @title cc_social_historical
#' @description
#' @param symbol character, the symbol of interest.
#' @param start character or Date, the start date for importation.
#' @param end character or Date, the end date for importation.
#' @param interval character, the frequency of the data, it can be "daily" or "hourly".
#' @param api_key character
#' @return a tibble
#' @name cc_social_historical
#' @rdname cc_social_historical
#' @export

cc_social_historical <- function(symbol = NULL, start = NULL, end = NULL,  interval = c("daily", "hourly"), api_key = NULL ){

  coinId <- cc_symbol_id(symbol)

  interval <- match.arg(interval, choices = c("daily", "hourly"))

  # time parameters
  if(interval == "daily"){

    end_date <- ifelse(is.null(end), Sys.Date(), end)
    end_date <- as.Date(end_date)
    start_date <- as.Date(start)

  } else {

    end_date <- ifelse(is.null(end), Sys.time(), end)
    end_date <- as.POSIXct(end_date, start = "1970-01-01")
    start_date <- as.POSIXct(start, start = "1970-01-01")
  }

  # while loop to get all the data
  i <- 1
  last_date <- end_date
  historical_data <- list()

  while(start_date < as.Date(last_date)){

    # save version to avoid errors
    safe_import <- purrr::safely(cc_api_social_historical)

    historical_data[[i]] <- safe_import(coinId = coinId, end = last_date, interval = interval, api_key = api_key)$result

    # break control
    if(is.null(historical_data[[i]]) || nrow(historical_data[[i]]) == 0){
      break
    }

    last_date <- historical_data[[i]]$Date[1]
    i <- i + 1
  }

  historical_data <- dplyr::bind_rows(historical_data)
  historical_data <- unique(historical_data)
  historical_data <- dplyr::arrange(historical_data, Date)

  # if selected a range filter since we import always 2000
  historical_data <- dplyr::filter(historical_data, Date >= start_date & Date <= end_date )
  historical_data$Id <- symbol

  historical_data <- dplyr::rename(historical_data, Symbol = "Id")

  if(interval == "daily"){

    historical_data$Date <- as.Date(historical_data$Date)

  }

  return(historical_data)
}


cc_api_social_historical <- function(coinId = NULL, end = NULL,  interval = c("daily", "hourly"), api_key = NULL){

  if(is.null(api_key)){
    warning("Submit a valid Api Key in order to access to this endpoint!")
    return(NULL)
  }

  # matching arguments for api calls  to avoin ifelse
  api.interval <-  dplyr::case_when(interval == "daily" ~ "day",
                                    interval == "hourly" ~ "hour")

  # creating end date for daily and hourly data
  if(interval == "hourly"){
    end_date <- ifelse(is.null(end), as.character(Sys.Date()), as.character(end))
    end_date <- as.POSIXct(end_date)
  } else {
    end_date <- ifelse(is.null(end), as.character(Sys.time()), as.character(end))
    end_date <- as.Date(end_date)
  }

  end_date <- as_unix(end_date)


  # Path, Query & Response
  api.paths <- c("data", "social", "coin", "histo", api.interval)

  api.query <- list(coinId = coinId, toTs = end_date, limit = 2000, api_key = api_key)

  api.response <- cryptocompare_api(path = api.paths, query = api.query)

  if(is.null(api.response)){
    warning("Errors in cyptocompare_api function")
    return(NULL)
  }

  # cleaning
  api.response <- api.response$Data
  api.response <- api.response[!duplicated(api.response),] # remove duplicated for safety
  api.response <- dplyr::as_tibble(api.response)           # conversion to tibble

  # create new variables and convert time index
  api.response <- dplyr::mutate(api.response,
                                Id = coinId,
                                Date = as.POSIXct(time, origin = "1970-01-01" ))

  # reorder and rename the variables
  api.response <- dplyr::select(api.response, Date, Id, dplyr::everything())
  api.response <- dplyr::select(api.response, -time)


  # conversion in daily dates if daily interval
  if(interval == "daily"){

    api.response <- dplyr::mutate(api.response, Date = as.Date(Date))

  }

  api.response <- dplyr::arrange(api.response, Date)

  return(api.response)

}
