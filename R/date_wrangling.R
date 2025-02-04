#' Check if dates are in ISO format.
#' 
#' Returns TRUE if a vector of character string dates is in a
#'YYYY-MM-DD format recognized by as.Date.
#' as.Date recognizes - or / separators.
#' It Also works with single digit month or day. 
#' @param d 
#' a vector of dates in character format
#' @param sepstr 
#' Specified separator character (usually -). Useful for applications that don't recognize / separator.
#' @return a vector of TRUE and FALSE indicating whether each date is in ISO format. NAs are returns as NA.
#' @export
#'
#' @examples
#' myd = c("2024-01-01","2022/04/01","2023-15-01","6/23/22","24-01-01", NA,"jbs")
#' cbind(myd, is.ISOdate(myd), is.ISOdate(myd, sep="-"))
#' is.ISOdate(myd)
is.ISOdate <- function(d,sepstr = NULL){
  ## checks if string is YYYY-MM-DD format 
  dvec <- sapply(d, function(x){tryCatch(as.character(as.Date(x)), error = function(e)NA)})
  ISOdatevec <- !is.na(dvec) & substr(dvec,1,2)!="00" # prevents 2-digit years from being interpreted as early history
  if(is.null(sepstr)){
    # allows for other separator strings recognized by as.Date
    outdvec <- ifelse(!is.na(d),ISOdatevec,NA) 
  }else{
    if(length(sepstr) > 1){
      stop("only one sepstr at a time")
    }
    myregex <- paste0("^\\d{4}",sepstr,"\\d{1,2}",sepstr,"\\d{1,2}$")
    rvec <- grepl(myregex,d)
    sepdatevec <- rvec & ISOdatevec
    outdvec <- ifelse(!is.na(d),sepdatevec,NA) 
  }
  return(outdvec)
}

