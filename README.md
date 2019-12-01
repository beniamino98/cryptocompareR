![cryptocompareApi](images/0.png)

# cryptocompareR: a framework for cryptocurrency Data 

The package will be part of a set of developing packages that allow a "tidy" approach, for downloading, grouping and analyzing data deriving from blockchain technology.
The goal of cryptocompareR is to integrate with the frameworks already introduced by other packages: first of all "tidyverse" and then "quantmod", "tidyquant", etc. and expand the range of downloadable and analysable data.

The Data were provided from [cryptocompareApi](https://www.cryptocompare.com).
[Click Here to see te Api Documentation](https://min-api.cryptocompare.com/documentation)


### the "cc_get" function:

For simplicity and to speed up the learning the various methods available have been grouped into a more flexible function, but for which a little explanation is needed.

##### setting the api key  and the environment 

```{r}
# setting just the api key 
cc_use_key(x= yourapikey)

# or setting the entire environment 
cc_set_env(api_key = yourapikey)

# for cleaning the envirnoment 
cc_clean_env()
```
You can just call the first time the function `cc_get` with the param `api_key` specifyied just the first time. Then it will be
automatically added to `cc_env` in order to reuse it until you restart R or clean the environment.



### 1) Method: "stock"

##### list of all symbols and exchanges availables 

```{r}

cc_symbol_options()
cc_exchange_options()
```

##### getting all get options 

```{r}
cc_get_options()

```



##### Getting Stock Data 
The "stock" method allows you to download the price data of one of the available symbols. The substantial difference from yahoo's financial data is the ability to download hourly data and data minute by minute (for the last 7 days). The hourly data prove to be indispensable to carry out any analysis that involves the blockchain data combined with the financial data.

```{r}

cc_get("BTC", get = "stock")
cc_get(c("BTC","ETH"), get = "stock")

cc_get("BTC", get = "stock",exchange = "Coinbase")
cc_get(c("BTC","ETH"), get = "stock", exchange = c("Coinbase","LocalBitcoins"))
```



#### 2) Method: "news"

##### list of all feeds and categories availables 

```{r}
cc_feed_options()
cc_category_option()
```

##### getting news data 

```{r}
cc_get(get = "news")
cc_get(get = "news", from = "2019-01-01", to = "2019-02-01")
cc_get(search = "cryptocompare", get = "news", category = "Mining")
cc_get(search = "yahoo", get = "news", from = "2019-01-01", to = "2019-02-01")

```


#### 3) Method: "social": ( with api key )


##### usage of the method: "social"


```{r}

cc_get("BTC", get = "social")
cc_get(c("BTC","ETH"), get = "social")

```


##### 4) Method: "all..."

##### searching generic elements 


```{r}

# works also without an api key 
cc_get(get = "all_coins")
cc_get(get = "all_exchanges")

# need a valid api key or to have set one 
cc_get(get = "all_gamlings", api_key = yourapikey)
cc_get(get = "all_wallets", api_key = yourapikey)
cc_get(get = "all_cards", api_key = yourapikey)
cc_get(get = "all_contracts", api_key = yourapikey)
cc_get(get = "all_companies", api_key = yourapikey)
cc_get(get = "all_pools", api_key = yourapikey)
```

