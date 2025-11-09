repair_NA <- function(object){

  UseMethod("repair_NA")

}

base_repair <- function(x){

  default <- c("NA", "NULL", "N/A", "NA", " ", "", "-", "N.D")

  x <- ifelse(x %in% default, NA, x)

  return(x)

}

repair_NA.character <- function(object){

  y <- purrr::map_chr(object, base_repair)

  return(y)
}


repair_NA.list <- function(object){

  y <- purrr::map(object, base_repair)

  return(y)

}

repair_NA.data.frame <- function(object){

  y <- purrr::map_df(object, base_repair)

  return(y)

}