context("Testing cc_historical_price")

start_date <- "2020-01-01"
end_date <- "2023-01-01"
expected_observations <- as.numeric(as.Date(end_date)-as.Date(start_date))


df <- cc_price_historical(symbol = "BTC", start = start_date, end = end_date, currency = "USD", exchange = NULL, interval = "daily", api_key = NULL)


test_that("Test 1: Minimum Date is coherent with start date.", {

  summarise(df, Min_Date = min(Date))$Min_Date %>%
    expect_equal(as.Date(start_date))
})

test_that("Test 2: Maximum Date is coherent with end date.", {

  summarise(df, Max_Date = max(Date))$Max_Date %>%
    expect_equal(as.Date(end_date)-1)
})

test_that("Test 3: the number of observations is coherent with start and end date.", {

  nrow(df) %>%
    expect_equal(expected_observations)
})
