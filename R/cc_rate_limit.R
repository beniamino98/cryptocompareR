
cc_rate_limit <- function() {

  api_paths <- c("stats", "rate", "limit")

  df_limit <- cryptocompare_api(path = api_paths, query = NULL)$Data

  dplyr::bind_rows(
    dplyr::bind_cols(Calls = "Made", dplyr::bind_rows(df_limit$calls_made) ),
    dplyr::bind_cols(Calls = "Left", dplyr::bind_rows(df_limit$calls_made) )
  )
}



