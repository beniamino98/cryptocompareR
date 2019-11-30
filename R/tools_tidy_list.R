#' @title map_na
#' @description : function that iterate over a list the condition na_if ,
#' specified in the pat argument the function will also perform a substitution of empty elements,
#'  and NULL elements with NA
#' @param .l : a list of elements
#' @param nested Default FALSE, indicate if you want to map the function in a nested list(list of lists) or in a simple list.
#' @return a list with
#' @name map_na
#' @rdname map_na
#' @export

map_na  <- function ( .l,  nested = FALSE){

  map_na_sf <-  purrr::safely(function ( .l, .repair = TRUE) {

    pattern_na1 <- ""
    pattern_na2 <- 'N/A'
    pattern_na3 <- '.'

    map_c <- function(.l, fun) unlist(purrr::map(.l, fun))

    .l <-  dplyr::na_if( .l, pattern_na1 )
    .l <-  dplyr::na_if( .l, pattern_na2 )
    .l <-  dplyr::na_if( .l, pattern_na3 )

    .l[ map_c( .l, purrr::is_empty ) ]  <- NA
    .l[ map_c( .l, purrr::is_null  ) ]  <- NA

    if(.repair){ .l <- tibble::repair_names(.l)}

    return( .l )

  })

  if(nested){
    result <- purrr::map(map_na_sf(.l, .repair = F)$result, ~map_na_sf(.l, .repair = F)$result)
  } else{
    result <- map_na_sf(.l, .repair = F)$result
  }

  if(is.null(result)){
    warning("sorry something went wrong with map_na")
    return(NULL)
  } else {
    return(result)
  }

}


#' @title tidy_list
#' @description : function for binding a list to dataframe safely the function will also perform a substitution
#' of empty elements, and NULL elements with NA.
#' @param .l : a list of elements
#' @param quiet : indicate if display messages or not
#' @return a tibble
#' @name tidy_list
#' @rdname tidy_list
#' @examples
#' \dontrun{tidy_list(.l = a list)}
#' @export


tidy_list <- function( .l = NULL, quiet = FALSE ){

  list.index  <- NULL
  list.tidy   <- NULL
  bind_safe   <-  purrr::safely(dplyr::bind_rows)
  list.index  <-  unlist( purrr::map( .l, is.list ) )
  list.tidy   <-  map_na(.l[ !list.index ])
  list.tidy_b <-  bind_safe(list.tidy)$result

  if(is.null(list.tidy_b)){

    if(!quiet) message("list can't be binded ")
    list.tidy <- map_na(.l)

  } else {

    list.tidy <- dplyr::tbl_df( list.tidy_b )

  }

  return(list.tidy)

}


#' @title map_tidy
#' @description : mapped version of tidy_list
#' @param .l : a list of list
#' @param binded logical, FALSE, indicate if apply bind_rows to reduce the list in a tibble or not.
#' @return a tibble
#' @name map_tidy
#' @rdname map_tidy
#' @examples
#'  \dontrun{map_tidy(.l = a list of lists)}
#' @export

map_tidy <- function( .l = NULL, binded = FALSE ) {

  if( is.null(.l) ){ return(NA) }

  list.nested <- purrr::map(.l, ~tidy_list(.x, quiet = T))
  if( binded ) {

    list.nested <- map_na(list.nested)
    list.nested <- list.nested[!is.na(list.nested)]
    list.binded <- dplyr::bind_rows(list.nested)

  } else {
    list.binded <- list.nested
  }
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
#' @name equalize
#' @rdname equalize
#' @examples
#' \dontrun{equalize(x = c("a", "b"), y = c("d", "e"))}
#'



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



if_null_s <- purrr::safely(function(x, y = NULL, z = NULL )ifelse( is.null(x), ifelse(is.null(y),ifelse(is.null(z), NA, z), y), x ) )
if_null <- function(x, y = NULL, z = NULL ) {ifelse(is.null(if_null_s(x, y, z )$result), NA, if_null_s(x, y, z )$result)}





