![cryptocompareApi](images/0.png)

# cryptocompareR: A Wrapper for Cryptocompare Api in R

The package will be part of a set of developing packages that allow a "tidy" approach, for downloading, grouping and analyzing data deriving from blockchain technology. The goal of cryptocompareR is to integrate with the frameworks already introduced by other packages: first of all "tidyverse" and then "quantmod", "tidyquant", etc. and expand the range of downloadable and analysable data.

The Data were provided from the [cryptocompare Api](https://www.cryptocompare.com). 

[Click Here to see te Api Documentation](https://min-api.cryptocompare.com/documentation)


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

# A tibble: 2,209 × 12
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

### Minutely OHLCV Historical Data

### Live Data 

### Different Exchanges 





