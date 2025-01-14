#' Check if dates are in ISO format.
#' Returns TRUE if a vector of character string dates is in a
#' recognizable YYYY-MM-DD format. This works with - or / separators.
#' It Also works with single digit month or day. 
#' @param d a vector of dates in character format
#'
#' @return a vector of TRUE and FALSE indicating whether each date is in ISO format. NAs are returns as NA.
#' @export
#'
#' @examples
#' myd = c("2024-01-01","2022-01-01","6/23/22","24-01-01", NA,"jbs")
#' is.ISOdate(myd)
is.ISOdate <- function(d){
  dvec <- sapply(d, function(x){tryCatch(as.character(as.Date(x)), error = function(e)NA)})
  outdvec <- ifelse(!is.na(d),!is.na(dvec) & substr(dvec,1,2)!="00",NA)
  return(outdvec)
}
