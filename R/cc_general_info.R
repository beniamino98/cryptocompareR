#' Cryptocompare General Info
#' 
#' Retrieve the general information, using the cryptocompare Api.
#' 
#' @param info Character. Without an `api_key` it is possible to access only to `"coins"` and `"exchanges"` information.
#' Using a free api key, it is possible to access to all the endpoints: 
#' - `"blockchain"`: returns a list of [all coins](https://min-api.cryptocompare.com/documentation?key=Blockchain&cat=blockchainListOfCoins) for which we currently get blockchain data from IntoTheBlock.
#' - `"cards"`: returns general info about all the integrated [crypto cards](https://min-api.cryptocompare.com/documentation?key=Other&cat=allCardsStaticInfoEndpoint).
#' - `"coins"`: returns general info about all the integrated [coins](https://min-api.cryptocompare.com/documentation?key=Other&cat=allCoinsWithContentEndpoint).
#' - `"companies"`: returns general info about all the integrated [mining companies](https://min-api.cryptocompare.com/documentation?key=Other&cat=allMiningCompaniesStaticInfoEndpoint).
#' - `"contracts"`: returns general info about all the integrated [mining contracts](https://min-api.cryptocompare.com/documentation?key=Other&cat=allMiningContractsStaticInfoEndpoint).
#' - `"equipment"`: returns general info about all the integrated [mining equipment](https://min-api.cryptocompare.com/documentation?key=Other&cat=allMiningEquipmentStaticInfoEndpoint). 
#' - `"exchanges"`: returns general info and 24h volume for all integrated [exchanges](https://min-api.cryptocompare.com/documentation?key=Other&cat=allExchangesStaticInfoEndpoint).
#' - `"gambling"`: returns general info about all the integrated [exchanges](https://min-api.cryptocompare.com/documentation?key=Other&cat=allGamblingStaticInfoEndpoint).
#' - `"wallets"`: returns general info about all the integrated [wallets](https://min-api.cryptocompare.com/documentation?key=Other&cat=allWalletsStaticInfoEndpoint).
#' - `"pools"`: returns general info about all the integrated [mining pools](https://min-api.cryptocompare.com/documentation?key=Other&cat=allMiningPoolsStaticInfoEndpoint). 
#'
#' @param currency Character. Currency in which convert the monetary values.
#' @param api_key Character, optional. Api key from cyptocompare.
#' @param verbose Logical. If `TRUE` it will display warning messages if you do not insert a valid api key for the api-key-only endpoints.
#'
#' @return a tibble
#'
#' @examples
#' # Endpoints reachable without an Api Key
#' cc_general_info("coins")
#' cc_general_info("exchanges")
#'
#' # Endpoints reachable only with a valid Api Key
#' \dontrun{
#' api_key <- "yourapikey"
#' cc_general_info("gambling", api_key = api_key)
#' cc_general_info("wallets", api_key = api_key)
#' cc_general_info("cards", api_key = api_key)
#' cc_general_info("contracts", api_key = api_key)
#' cc_general_info("companies", api_key = api_key)
#' cc_general_info("equipment", api_key = api_key)
#' cc_general_info("pools", api_key = api_key)
#' cc_general_info("blockchain", api_key = api_key)
#' }
#' @export
#' 
#' 
#' @name cc_general_info
#' @rdname cc_general_info

cc_general_info <- function(info = "coins", currency = "USD", api_key = NULL, verbose = FALSE){

  # Available paths
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
  path <- match.arg(info, choices = names(general_info))

  # Api key is required for endpoints different from `coins`/`exchanges`
  if (!path %in% c("coins", "exchanges")) {
    # Check "api_key" argument 
    if (missing(api_key) || is.null(api_key)) {
      if (!quiet) {
        msg <- paste0('An "api_key" is required to reach this endpoint!')
        cli::cli_alert_danger(msg)
        return(NULL)
      }
    } 
  }

  # Function to replace `NULL` with `NA` and build a `tibble`
  safe_tibble <- function(x){
    x.cond <- purrr::map_lgl(x, is.list)
    x <- x[!x.cond]
    x <- purrr::map(x, ~ifelse(is.null(.x), NA, .x))
    dplyr::as_tibble(x)
  }
  
  # GET call
  api_path <- c("data", general_info[[path]])
  api_query <- list(tsym = currency, api_key = api_key)
  response <- cryptocompare_api(path = api_path, query = api_query)$Data
  response <- purrr::map_df(response, safe_tibble)
  
  # Output 
  if (info == "coins") {
    response <- dplyr::mutate(response,
                         Algorithm = toupper(Algorithm),
                         Algorithm = stringr::str_trim(Algorithm),
                         ProofType = toupper(ProofType),
                         ProofType = stringr::str_trim(ProofType))
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
                              -Sponsored, -Recommended)
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

