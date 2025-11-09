#' @title SMA
#' @description compute the Simple Moving Average of a vector.
#' @param x numeric vector.
#' @param n integer, the number of length of the moving average.
#' @param .fun function, the function to use, for example `mean`, `var`, `sd`, ...
#' @param na.rm logical,
#' @return a vector of the same length of `x`.
#' @name SMA
#' @rdname SMA
#' @export
SMA <- function(x = NULL, n = 10, .fun = mean, na.rm = TRUE){

  N = length(x)

  y = rep(NA_integer_, N)

  for(i in n:N){
    y[i] = .fun(x[(i-n+1):i], na.rm = TRUE)
  }

  return(y)

}

#' @title SMA
#' @description compute the Exponential Moving Average of a vector.
#' @param x numeric vector.
#' @param n integer, the number of length of the moving average.
#' @param smooth numeric, a parameter to use for smoothing the moving average.
#' @param na.rm logical,
#' @return a vector of the same length of `x`.
#' @name EMA
#' @rdname EMA
#' @export

EMA <- function(x = NULL, n = 10, smooth = 2, na.rm = TRUE){

  N = length(x)
  y = rep(NA_integer_, N)

  k = smooth/(1+n)
  y[n-1] =  mean(x[1:(n-1)], na.rm = TRUE)

  for(i in n:N){

    y[i] = y[i-1]*(1-k) + k*x[i]

  }

  y[n-1] = NA_integer_

  return(y)

}


#' @title add_EMA
#' @description generic method for compute the Exponential Moving Average of an object.
#' @param ... other parameters.
#' @name add_EMA
#' @rdname add_EMA
#' @export

add_EMA <- function(object, ...){
  UseMethod("add_EMA")
}


#' @param object other parameters.
#' @param variable other parameters.
#' @param n
#' @param ... ther parameters.
#' @name add_SMA
#' @rdname add_SMA
#' @export

add_SMA <- function(object, ...){
  UseMethod("add_EMA")
}

#' @title add_EMA.data.frame
#' @description method for the class of objects `data.frame` for computing the Exponential Moving Average and adding to the
#' dataset in a tidy way.
#' @param variable the name of the variable of which we would compute the moving average.
#' @param n integer, the number of length of the moving average.
#' @param smooth numeric, a parameter to use for smoothing the moving average.
#' @param na.rm logical,
#' @return a tibble with one more column named as `EMA_` + `variable`, `_n`.
#' @examples
#' df <- data.frame(x = runif(1000, 0, 10))
#' df <- add_EMA(df, variable = "x", n = 10)
#'
#' @name add_EMA.data.frame
#' @rdname add_EMA.data.frame
#' @export

add_EMA.data.frame <- function(object, variable = "Close", n = 10, smooth = 2, na.rm = TRUE){

  col_price_index <- tolower(colnames(object)) %in% c(tolower(variable))

  X <- object[[which(col_price_index)]]

  Y <- EMA(X, n = n, smooth = smooth, na.rm = na.rm)

  new_col_name <- paste0("EMA_", variable, "_", n )

  object[[new_col_name]] <- Y

  return(object)

}

#' @title add_SMA.data.frame
#' @description method for the class of objects `data.frame` for computing the Simple Moving Average and adding to the
#' dataset in a tidy way.
#' @param variable the name of the variable of which we would compute the moving average.
#' @param n integer, the number of length of the moving average.
#' @param smooth numeric, a parameter to use for smoothing the moving average.
#' @param na.rm logical,
#' @return a tibble with one more column named as `SMA_` + `variable`, `_n`.
#' @examples
#'
#' df <- data.frame(x = runif(1000, 0, 10))
#'
#' # the new column will be named as "SMA_x_10"
#' df <- add_EMA(df, variable = "x", n = 10)
#'
#' @name add_EMA.data.frame
#' @rdname add_EMA.data.frame
#' @export

add_SMA.data.frame <- function(object, variable = "Close", n = 10, na.rm = TRUE){

  col_price_index <- tolower(colnames(object)) %in% c(tolower(variable))

  X <- object[[which(col_price_index)]]

  Y <- SMA(X, n = n, .fun = mean, na.rm = na.rm)

  new_col_name <- paste0("SMA_", variable, "_", n )

  object[[new_col_name]] <- Y

  return(object)

}

