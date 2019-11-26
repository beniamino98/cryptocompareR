# cryptocompareR: a framework for cryptocurrency Data 

The package will be part of a set of developing packages that allow a "tidy" approach, for downloading, grouping and analyzing data deriving from blockchain technology.
The goal of cryptocompareR is to integrate with the frameworks already introduced by other packages: first of all "tidyverse" and then "quantmod", "tidyquant", "xts", etc. and expand the range of downloadable and analysable data.


### the "cryptocompare" function:

For simplicity and to speed up the learning the various methods available have been grouped into a more flexible function, but for which a little explanation is needed.


### 1) Method: "stock"

##### usage the method: "search"
The search method allows you to get an idea about the parameters that can be entered. 
The parameters that can vary using the "stock" method and the "exchange" method are the symbol of the cryprocurrency and the reference exchange.

```{r}
# getting information about all exchanges available 
cryptocompare(method = "search", search = "exchange", all = TRUE )

> cryptocompare("search", search = "exchange", all = TRUE)
  [1] "Bitstamp"           "BitTrex"            "OKCoin"             "BTER"               "Poloniex"          
  [6] "Kraken"             "Bitfinex"           "Cexio"              "Yacuna"             "Coinbase"    

# getting information about all symbols available 
cryptocompare(method = "search", search = "symbol", all = TRUE)
> cryptocompare("search", search = "symbol", all = TRUE)
   [1] "42"        "300"       "365"       "404"       "433"       "611"       "808"       "888"       "1337"     
  [10] "2015"      "LTC"       "XMR"       "NXT"       "ETC"       "DOGE"      "ZEC"       "BTS"       "DGB"      

```

##### Getting Stock Data 
The "stock" method allows you to download the price data of one of the available symbols. The substantial difference from yahoo's financial data is the ability to download hourly data and data minute by minute (for the last 7 days). The hourly data prove to be indispensable to carry out any analysis that involves the blockchain data combined with the financial data.



```{r}

# download all data from 2008-01-01 to now
cryptocompare(method = "stock", search = "BTC")

Downloading: 8.7 kB 

                        BTC_High    BTC_Low    BTC_Open BTC_Quantity      BTC_Volume            BTC_Adjusted
2008-01-01 01:00:00        0.00000     0.00000     0.00000      0.00           0.000000e+00     0.000000
...
2019-11-24 01:00:00     7364.59     7004.10     7339.24     18909.77   135867519             7184.345



# hourly data in an interval of date [default end = now ]
> cryptocompare(method = "stock", search = "BTC", interval = "hour", from = "2019-01-01")
Downloading: 62 kB                         
                    BTC_High  BTC_Low  BTC_Open   BTC_Quantity   BTC_Volume   BTC_Adjusted
2019-01-01 00:00:00  3754.34  3739.76  3743.61    1775.7400      6608416.60   3747.050
...
2019-11-24 01:00:00  7364.59  7264.49  7339.24    1157.34        8452895      7314.540




# hourly data in an interval of date 
> cryptocompare(method = "stock", search = "BTC", interval = "hour", from = "2019-01-01", to = "2019-02-01")

Downloading: 64 kB                         
                     BTC_High BTC_Low  BTC_Open   BTC_Quantity   BTC_Volume   BTC_Adjusted
2019-01-01 00:00:00  3754.34  3739.76  3743.61         1775.74      6608417       3747.050
2019-01-01 01:00:00  3759.98  3741.44  3747.39         1451.38      5416482       3750.710
...
2019-02-01 00:00:00  3447.25  3430.15  3447.17         2405.99      8240201       3438.700
2019-02-01 01:00:00  3444.76  3430.64  3434.13         2969.32     10257093       3437.700

```



```{r}
# visualizing data with xts 
plot.xts(btc_xts["2018::2019"], col = "black", main = "BTC/USD" )


chartSeries(btc_xts, 
            name = "BTC/USD 2019",
            subset = "2019-09::2019::11",
            theme =chartTheme(theme = "white"))
addSMA(n = 15, col = "blue")
addSMA(n = 60)

```



##### 2) Method: "news"

##### usage the method: "search"


```{r}
# getting information about all feeds available 
cryptocompare(method = "search", search = "feed", all = TRUE)

> cryptocompare("search", search = "feed", all = TRUE)
 [1] "cryptocompare"       "cryptoglobe"         "cryptonewsreview"    "coindesk"           
 [5] "bitcoinmagazine"     "yahoofinance"        "ccn"                 "financemagnates"    
 [9] "espaciobit"          "99bitcoins"          "bitcoinist"          "bitcoin.com"        
[13] "ethnews.com"         "cointelegraph"       "cointelegraph_es"    "cryptovest"         
[17] "cryptoinsider"       "livebitcoinnews"     "newsbtc"             "coinspeaker"        
[21] "coinjoker"           "trustnodes"          "themerkle"           "cryptocoremedia"    
[25] "cryptopotato"        "coinnounce"          "blokt"               "cointelligence"     
[29] "ethereumworldnews"   "ambcrypto"           "ambcrypto_es"        "cryptobriefing"     
[33] "cryptoslate"         "coingape"            "dailyhodl"           "bitcoinerx"         
[37] "chaindd"             "cryptonewsz"         "criptonoticias"      "diariobitcoin"      
[41] "cryptopolitan"       "theblock"            "decrypt"             "chaintimes"         
[45] "timesnext"           "crypto_watch_com_br"
 

# getting information about all categories available 

> cryptocompare("search", search = "categories", all = TRUE)
 [1] "BTC"        "BCH"        "ETH"        "LTC"        "XMR"        "ZEC"        "ETC"       
 [8] "XRP"        "TRX"        "ADA"        "DASH"       "XTZ"        "USDT"       "Mining"    
[15] "Exchange"   "Market"     "Asia"       "ICO"        "Regulation" "Blockchain" "Trading"   
[22] "Technology" "Wallet"     "Altcoin"    "Fiat"       "Business"   "Commodity"  "Sponsored"

```





```{r}

# news for all feed and all categories (last 90 day )
all_news <- cryptocompare("news")
> all_news
# A tibble: 4,500 x 12
   time                source id    guid  imageurl title url   body  tags  categories upvotes
   <dttm>              <chr>  <chr> <chr> <chr>    <chr> <chr> <chr> <chr> <chr>      <chr>  
 1 2019-11-24 00:21:44 bitco… 4707… http… https:/… A Fe… http… If y… Feat… BTC|Fiat|… 0      
 2 2019-11-24 00:06:58 crypt… 4706… http… https:/… Coin… http… Cryp… Revi… Regulation 0      
 3 2019-11-24 00:00:06 theme… 4710… http… https:/… Worl… http… The … Cryp… ICO        0     
 # … with 4,490 more rows, and 1 more variable: downvotes <chr>
 
 

# news for one feed , for all categories for specifying a time interval 

> cryptocompare(method = "news", feed = "cryptocompare", from = "2019-10-01", to = "2019-11-01" )

Downloading: 6.7 kB     # A tibble: 49 x 13
   time                source id    guid  imageurl title url   body  tags  categories upvotes
   <dttm>              <chr>  <chr> <chr> <chr>    <chr> <chr> <chr> <chr> <chr>      <chr>  
 1 2019-10-31 10:14:26 crypt… 3990… http… https:/… Mark… http… CME … Bitc… BTC|Marke… 0      
 2 2019-10-30 10:14:15 crypt… 3990… http… https:/… Mark… http… Bitm… Bitc… Market|Fi… 0      
 3 2019-10-30 10:14:15 crypt… 3990… http… https:/… Mark… http… Bitm… Bitc… Market|Fi… 0      
 # … with 39 more rows, and 2 more variables: downvotes <chr>, lang <chr> 


# news for one feed , for a specific category  for specifying a time interval for a specific languege 

> cryptocompare(method = "news", feed = "cryptocompare", categories = "mining", from = "2019-10-01", to = "2019-11-01", lang = "PT")
Downloading: 5.6 kB     # A tibble: 3 x 13
  time                source id    guid  imageurl title url   body  tags  categories upvotes downvotes
  <dttm>              <chr>  <chr> <chr> <chr>    <chr> <chr> <chr> <chr> <chr>      <chr>   <chr>    
1 2019-10-29 10:12:52 crypt… 3989… http… https:/… Mark… http… Full… Bitc… BTC|Marke… 1       0        
2 2019-10-22 10:13:02 crypt… 3982… http… https:/… Mark… http… CFTC… Bitc… BTC|Marke… 0       0        
3 2019-10-22 10:13:02 crypt… 3982… http… https:/… Mark… http… CFTC… Bitc… BTC|Marke… 0       0        
# … with 1 more variable: lang <chr>


```




##### 3) Method: "social": ( with api key )

##### usage the method: "search"

```{r}

```


##### 4) Method: "general"



```{r}

```
