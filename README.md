# A Wrapper for Cryptocompare Api in R [<img src="https://www.cryptocompare.com/media/20562/favicon.png?v=10" width=30 height=30 style="display:inline;">](https://beniamino98.github.io/cryptocompareR-docs/)

The package is part of a set of developing packages that allow a *tidy* approach, for downloading, grouping and analyzing data for cryptocurrencies. 

The Data are provided by [cryptocompare Api](https://www.cryptocompare.com). 

- Here the [official Api Documentation](https://min-api.cryptocompare.com/documentation)

# Installation 

```{r Installation}
# lightweight
remotes::install_github("beniamino98/cryptocompareR")
# or
devtools::install_github("beniamino98/cryptocompareR")

library(cryptocompareR)
```

## 1) OHLCV Historical Data
The package allow to retrieve different informations from a given cryptocurrency. The most searched information are the Open, High, Low, Close prices and the Volumes. The cryptocompare Api allows to ask this informations on different timeframes for free. For example, considering the well-known pair BTC-USD it is possible to search it in differents ways.

### Daily OHLCV Historical Data

```{r}

BTC <- cc_price_historical(symbol = "BTC", currency = "USD", 
                           start = "2017-01-01", end = "2023-01-19", 
                           exchange = NULL, interval = "daily", api_key = NULL)
```
Printing the output in console gives the following output: 

```{r}
BTC

> # A tibble: 2,209 × 12
   Date       Symbol Currency Exchange  high   Low  Open  Volume Close   Adj       OC     HL
   <date>     <chr>  <chr>    <chr>    <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>  <dbl>
 1 2017-01-01 BTC    USD      global   1002.  956.  963.  41149.  995. 1958.  0.0333  0.0476
 2 2017-01-02 BTC    USD      global   1032.  990.  995.  64952. 1017. 2017.  0.0217  0.0419
 3 2017-01-03 BTC    USD      global   1035. 1007. 1017.  54788. 1033. 2046.  0.0160  0.0288
 4 2017-01-04 BTC    USD      global   1149. 1022. 1033. 156272. 1135. 2170.  0.0988  0.123 
 5 2017-01-05 BTC    USD      global   1151.  875. 1135. 240008.  989. 2075. -0.129   0.316 
 6 2017-01-06 BTC    USD      global   1027.  853.  989. 194291.  886. 1878. -0.104   0.205 
 7 2017-01-07 BTC    USD      global    901.  807.  886. 130663.  889. 1741.  0.00306 0.117 
 8 2017-01-08 BTC    USD      global    936.  876.  889.  76906.  901. 1801.  0.0135  0.0688
 9 2017-01-09 BTC    USD      global    910.  871.  901.  62580.  900. 1791. -0.00120 0.0454
10 2017-01-10 BTC    USD      global    911.  890.  900.  53709.  904. 1803.  0.00510 0.0238
# … with 2,199 more rows

```

### Hourly OHLCV Historical Data

```{r}

BTC <- cc_price_historical(symbol = "BTC", currency = "USD", 
                           start = "2017-01-01", end = "2023-01-19", 
                           exchange = NULL, interval = "hourly", api_key = NULL)
```
Printing the output in console gives the following output: 

```{r}
BTC

> # A tibble: 53,017 × 12
   Date                Symbol Currency Exchange  high   Low  Open Volume Close   Adj        OC      HL
   <dttm>              <chr>  <chr>    <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>     <dbl>   <dbl>
 1 2017-01-01 00:00:00 BTC    USD      global    966.  963.  966.   934.  965. 1930. -0.000435 0.00301
 2 2017-01-01 01:00:00 BTC    USD      global    971.  966.  965.   773.  968. 1935.  0.00250  0.00458
 3 2017-01-01 02:00:00 BTC    USD      global    967.  964.  968.   785.  964. 1932. -0.00342  0.00358
 4 2017-01-01 03:00:00 BTC    USD      global    963.  959.  964.   450.  962. 1924. -0.00297  0.00373
 5 2017-01-01 04:00:00 BTC    USD      global    966.  961.  962.   700.  963. 1926.  0.00132  0.00438
 6 2017-01-01 05:00:00 BTC    USD      global    966.  962.  963.   546.  963. 1927.  0.000395 0.00357
 7 2017-01-01 06:00:00 BTC    USD      global    966   963.  963.   417.  964. 1928.  0.00114  0.00333
 8 2017-01-01 07:00:00 BTC    USD      global    959.  956.  964.   408.  957. 1918. -0.00721  0.00311
 9 2017-01-01 08:00:00 BTC    USD      global    961.  956.  957.   565.  959. 1917.  0.00201  0.00543
10 2017-01-01 09:00:00 BTC    USD      global    962.  960.  959.   390.  962. 1922.  0.00259  0.00207
# … with 53,007 more rows

```

### Minutely OHLCV Historical Data
Unfortunatly the data available with the minutely timeframe, even if a free api key is provided, are limited to the last 7 days. In this case the start and end date does not matter, in this case the data obtained are from today "2023-01-20" up to 7 days ago. 

```{r}

BTC <- cc_price_historical(symbol = "BTC", currency = "USD", 
                           start = "2017-01-01", end = "2023-01-20", 
                           exchange = NULL, interval = "minutely", api_key = NULL)
```
Printing the output in console gives the following output: 

```{r}
BTC

> # A tibble: 9,660 × 12
   Date                Symbol Currency Exchange   high    Low   Open Volume  Close    Adj         OC       HL
   <dttm>              <chr>  <chr>    <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>    <dbl>
 1 2023-01-14 06:46:00 BTC    USD      global   20885. 20872. 20878.   42.6 20880. 41757.  0.0000948 0.000609
 2 2023-01-14 06:47:00 BTC    USD      global   20884. 20874. 20880.   12.9 20877. 41757. -0.000177  0.000502
 3 2023-01-14 06:48:00 BTC    USD      global   20879. 20846. 20877.   59.6 20849. 41726. -0.00130   0.00159 
 4 2023-01-14 06:49:00 BTC    USD      global   20849. 20830. 20849.   57.9 20838. 41683. -0.000551  0.000904
 5 2023-01-14 06:50:00 BTC    USD      global   20844. 20838. 20838.   16.2 20844. 41682.  0.000289  0.000298
 6 2023-01-14 06:51:00 BTC    USD      global   20847. 20842. 20844.   31.0 20845. 41689.  0.0000782 0.000251
 7 2023-01-14 06:52:00 BTC    USD      global   20852. 20843. 20845.   35.0 20852. 41696.  0.000305  0.000421
 8 2023-01-14 06:53:00 BTC    USD      global   20857. 20850. 20852.   16.4 20857. 41708.  0.000234  0.000306
 9 2023-01-14 06:54:00 BTC    USD      global   20859. 20845. 20857.   32.5 20849. 41705. -0.000381  0.000638
10 2023-01-14 06:55:00 BTC    USD      global   20849. 20835. 20849.   23.3 20841. 41687. -0.000390  0.000655
# … with 9,650 more rows

# Check for the Minimum and Maximum Date
c(min(BTC$Date), max(BTC$Date))

> "2023-01-14 06:46:00 CET" "2023-01-20 23:45:00 CET"

```

### Live Data 

```{r}

live_data <- cc_symbol_price(symbol = c("BTC", "ETH"), currency = c("USDT", "BUSD"), exchange = c("Binance", "Kraken", "Coinbase"), api_key = NULL)
live_data

# A tibble: 6 × 5
  Date                Exchange Symbol   USDT   BUSD
  <dttm>              <chr>    <chr>   <dbl>  <dbl>
1 2023-01-20 23:50:05 Binance  BTC    22621. 22630.
2 2023-01-20 23:50:05 Binance  ETH     1653.  1653.
3 2023-01-20 23:50:05 Kraken   BTC    22648.    NA 
4 2023-01-20 23:50:05 Kraken   ETH     1654.    NA 
5 2023-01-20 23:50:05 Coinbase BTC    22659     NA 
6 2023-01-20 23:50:05 Coinbase ETH     1654.    NA 
```






