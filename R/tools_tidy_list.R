#' @title mapNA
#' @description : function that iterate over a list the condition na_if ,
#' specified in the pat argument the function will also perform a substitution of empty elements,
#'  and NULL elements with NA
#' @param x : a list of elements
#' @param pattern_na : a pattern that will subsistute with NAs
#' @return clean list
#' @export

mapNA <- function ( x, pattern_na = "" ) {

  attr_list <-  attributes( x )

  mapna <- function( x, fun ){ unlist( purrr::map( x , fun ) ) }

  x <-  dplyr::na_if( x, pattern_na )

  x[ mapna( x, purrr::is_empty ) ]  = NA
  x[ mapna( x, purrr::is_null  ) ]  = NA

  attributes(x) <- attr_list

  return( x )

}


#' @title tidy_list
#' @description : function for binding a list to dataframe safely the function will also perform a substitution
#' of empty elements, and NULL elements with NA.
#' @param .l : a list of elements
#' @param quiet : indicate if display messages or not
#' @return a tibble
#' @examples
#' \dontrun{tidy_list(.l = a list)}
#' @export


tidy_list <- function( .l = NULL, quiet = FALSE ){

  list.index <- NULL
  list.tidy <- NULL

  na.names <- which(is.na(mapNA(names(.l))))
  names(.l)[na.names] <- paste0("V", 1:length(na.names))

  list.index <-  unlist( purrr::map( .l, is.list ) )


  list.tidy <- mapNA( .l[ !list.index ] )
  list.tidy <- dplyr::as_tibble(dplyr::bind_rows( list.tidy  ) )

  if(!quiet) message( sum(list.index), " elements dropped")


  return(list.tidy)

}


#' @title map_tidy
#' @description : mapped version of tidy_list
#' @param .l : a list of list
#' @return a tibble
#' @examples
#'  \dontrun{map_tidy(.l = a list of lists)}
#' @export

map_tidy <- function( .l = NULL ) {

  if( is.null(.l) ){ return(NA) }

  list.nested <- purrr::map(.l, ~tidy_list(.x, quiet = T))
  list.binded <- dplyr::bind_rows(list.nested)

  return(list.binded)
}






#' @title equalize
#' @description: equalize two vector of different lengths, it repeat the y-element n-times, with n
#' equal to the lenght of the x-vector
#'
#' @param x : a vector
#' @param y : a vector
#' @param names : if you want to set new names c( "colA", "colB" )
#' @return  a tibble with x and y
#' @examples
#' \dontrun{equalize(x = c("a", "b"), y = c("d", "e"))}
#'
#' @export


equalize <- function(x = NULL, y = NULL, names = NULL ){

  if( (is.null(y) || is.na(y)) & ( is.null(x) ||  is.na(x)) ) return(list(x = NULL, y = NULL))
  if( (is.null(x) || is.na(x)) & (!is.null(y) || !is.na(y)) ) return(list(x = NULL, y = y))
  if( (is.null(y) || is.na(y)) & (!is.null(x) || !is.na(x)) ) return(list(x = x, y = NULL))

  equalized <- dplyr::tibble()
  for(i in 1:length(x)){

    if(i == 1 ){
      equalized <- cbind(x = x[i],y =  y)
    } else{
      equalized <- rbind(equalized, cbind(x = x[i],y =  y))
    }

  }

  list(
    x = equalized[,1],
    y = equalized[,2]
  )

}

