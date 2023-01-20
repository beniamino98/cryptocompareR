#' @title cryptocompare_api
#' @description function to retrieve the "general informations", using the cryptocompare Api.
#'
#' @param info  character, the info to be obtained. Without a valid api_key you can access only to the "coins" and "exchanges" endpoints.
#' Using a free api key, obtained from the website, you can access also to the endpoints:
#' \itemize{
#'   \item "blockchain", "cards", "companies", "contracts", "equipment", "gambling", "wallets", and "pools".
#' }
#'
#' @param currency character, the currency in which convert the monetary values, if present.
#' @param api_key character, a valid api key from cyptocompare.com.
#' @param verbose logical, if TRUE it will display warning messages if you do not insert a valid api key for the api-key-only endpoints.
#'
#' @details
#'
#'
#' @return a tibble, the dimensions of the output depend on the "info" parameters.
#'
#'
#' @examples
#' # Endpoints reachable without an Api Key
#' cc_maket_info("coins")
#' cc_maket_info("exchanges")
#'
#' # Endpoints reachable only with a valid Api Key
#' api_key <- "yourapikey"
#'
#' cc_maket_info("gambling", api_key = api_key)
#' cc_maket_info("wallets", api_key = api_key)
#' cc_maket_info("cards", api_key = api_key)
#' cc_maket_info("contracts", api_key = api_key)
#' cc_maket_info("companies", api_key = api_key)
#' cc_maket_info("equipment", api_key = api_key)
#' cc_maket_info("pools", api_key = api_key)
#' cc_maket_info("blockchain", api_key = api_key)
#'
#' @name cc_maket_info
#' @rdname cc_maket_info
#' @export

cc_maket_info <- function(info = "coins", currency = "USD", api_key = NULL, verbose = FALSE){

  # path for general info
  general_info <- list(
    blockchain = "blockchain/list",
    cards = "cards/general",
    coins = "all/coinlist",
    companies = "mining/companies/general",
    contracts = "mining/contracts/general",
    equipment = "mining/equipment/general",
    exchanges = "exchanges/general",
    gambling = "gambling/general",
    wallets = "wallets/general",
    pools = "mining/pools/general"
  )

  search_match <- match.arg(info, choices = names(general_info))

  # check for API key if endpoints are different from "coins" or "exchanges"
  if(is.null(api_key) && !search_match %in% c("coins", "exchanges") ){

    if(verbose) warning("The Endpoint", '"', search_match,'"',  " need a valid Api Key!")

    return(NULL)
  }

  # Path
  search_paths <- c("data", general_info[[search_match]] )

  # Query
  search_query <- list(tsym = currency, api_key = api_key)

  # GET call
  search_api <- cryptocompare_api(path = search_paths, query = search_query)$Data

  # Replace null with NA's in order to build the dataframe
  safe_tibble <- function(x){

    x.cond <- purrr::map_lgl(x, is.list)
    x <- x[!x.cond]
    x <- purrr::map(x, ~ifelse(is.null(.x), NA, .x))

    dplyr::as_tibble(x)
  }

  # Output Data
  search_api <- purrr::map_df(search_api, safe_tibble)

  # Cleaning depending on the ourput
  if(info == "coins"){

    search_api <- dplyr::mutate(search_api,
                         Algorithm = toupper(Algorithm),
                         Algorithm = stringr::str_trim(Algorithm),
                         ProofType = toupper(ProofType),
                         ProofType = stringr::str_trim(ProofType)
    )
    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", ""), NA_character_, .x))
    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))
    search_api <- dplyr::arrange(search_api, Rank)
    search_api <- dplyr::select(search_api, -Url, -ImageUrl, -SortOrder)
    search_api <- dplyr::filter(search_api, IsTrading)


  } else if (info == "blockchain"){

    search_api <- dplyr::mutate(search_api,
                                From = as.POSIXct(data_available_from, origin = "1970-01-01"),
                                From = as.Date(From)
    )

    search_api <- dplyr::select(search_api, -data_available_from)
    search_api <- dplyr::arrange(search_api, From)

  } else if (info == "exchanges"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api,
                                -Url, -LogoUrl, -SortOrder, -ItemType,
                                -Description, -Fees, -DepositMethods, -WithdrawalMethods,
                                -Sponsored, -Recommended )

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  }  else if (info == "gambling"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, -BettingLimits)

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  } else if (info == "wallets"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, -IsUsingOurApi, -HasVouchersAndOffers, -WalletFeatures)

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  } else if (info == "cards"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended)

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  } else if (info == "contracts"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, -CurrenciesAvailableLogo )

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  } else if (info == "companies"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  } else if (info == "pools"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)


  } else if (info == "equipment"){

    search_api <- dplyr::mutate(search_api, Rank = as.integer(SortOrder))

    search_api <- dplyr::select(search_api, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )

    search_api <- dplyr::mutate_if(search_api, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))

    search_api <- dplyr::arrange(search_api, Rank)

  }

  return(search_api)

}






