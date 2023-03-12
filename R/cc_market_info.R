#' @title cryptocompare_api
#' @name cc_maket_info
#' @rdname cc_maket_info
#' 
#' @description Retrieve the general informations, using the cryptocompare Api.
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
#' @details to be written
#'
#' @return a tibble, the dimensions of the output depend on the "info" parameters.
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
#' @export

cc_maket_info <- function(info = "coins", currency = "USD", api_key = NULL, verbose = FALSE){

  # available paths for general info
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

  # match the path 
  search_match <- match.arg(info, choices = names(general_info))

  # check if endpoints are different from "coins" or "exchanges" and api_key is NULL
  if (is.null(api_key) && !search_match %in% c("coins", "exchanges")) {

    if (verbose) {
      warning("The Endpoint", '"', search_match,'"',  " need a valid Api Key!")
    }
    
    return(NULL)
  }

  # api path 
  api_path <- c("data", general_info[[search_match]])

  # api query
  api_query <- list(tsym = currency, api_key = api_key)

  # GET call
  response <- cryptocompare_api(path = api_path, query = api_query)$Data

  # function for replacing NULL values with NA's in order to build a tibble
  safe_tibble <- function(x){

    x.cond <- purrr::map_lgl(x, is.list)
    x <- x[!x.cond]
    x <- purrr::map(x, ~ifelse(is.null(.x), NA, .x))

    dplyr::as_tibble(x)
  }

  # Output Data
  response <- purrr::map_df(response, safe_tibble)

  # different cleaning on the enpoint 
  if (info == "coins") {
    
    response <- dplyr::mutate(response,
                         Algorithm = toupper(Algorithm),
                         Algorithm = stringr::str_trim(Algorithm),
                         ProofType = toupper(ProofType),
                         ProofType = stringr::str_trim(ProofType)
    )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", ""), NA_character_, .x))
    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::arrange(response, Rank)
    response <- dplyr::select(response, -Url, -ImageUrl, -SortOrder)
    response <- dplyr::filter(response, IsTrading)
    
  } else if (info == "blockchain") {

    response <- dplyr::mutate(response,
                                From = as.POSIXct(data_available_from, origin = "1970-01-01"),
                                From = as.Date(From))
    response <- dplyr::select(response, -data_available_from)
    response <- dplyr::arrange(response, From)

  } else if (info == "exchanges") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response,
                                -Url, -LogoUrl, -SortOrder, -ItemType,
                                -Description, -Fees, -DepositMethods, -WithdrawalMethods,
                                -Sponsored, -Recommended )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  }  else if (info == "gambling") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, -BettingLimits)
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "wallets") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, 
                              -IsUsingOurApi, -HasVouchersAndOffers, -WalletFeatures)
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "cards") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended)
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "contracts") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended, -CurrenciesAvailableLogo )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "companies") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "pools") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  } else if (info == "equipment") {

    response <- dplyr::mutate(response, Rank = as.integer(SortOrder))
    response <- dplyr::select(response, -Url, -LogoUrl, -SortOrder, -Sponsored, -Recommended )
    response <- dplyr::mutate_if(response, is.character, ~ifelse(.x %in% c("N/A", "", "-"), NA_character_, stringr::str_trim(.x)))
    response <- dplyr::arrange(response, Rank)

  }

  return(response)

}






