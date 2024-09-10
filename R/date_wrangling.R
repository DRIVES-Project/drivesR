
date_formats <- list(
  mdy = c(
    "%m-%d-%Y",	# 08-01-2024
    "%m/%d/%Y",	# 08/01/2024
    "%m-%d-%y",	# 08-01-24
    "%m/%d/%y"	# 08/01/24
  ),
  yyyymd = c(
    "%Y-%m-%d",	# 2024-08-01
    "%Y/%m/%d"	# 2024/08/01
    
  ),
  bmo = c(
    "%b %d, %Y",   # Aug 01, 2024
    "%B %d, %Y",   # August 01, 2024
    "%d %b %Y",	# 01 Aug 2024
    "%d %B %Y",	# 01 August 2024
    "%d-%b-%Y",	# 01-Aug-2024
    "%d %b %y",	# 01 Aug 24
    "%d %B %y",	# 01 August 24
    "%d-%b-%y",	# 01-Aug-24
    "%d %b %Y",	# 01 Aug 2024
    "%b %d, %Y",	# Aug 01, 2024
    "%b %d, %y",   # Aug 01, 24
    "%B %d, %y"   # August 01, 24
  ),
  dmy = c(
    "%d-%m-%Y",	# 01-08-2024
    "%d/%m/%Y",	# 01/08/2024
    "%d-%m-%y",	# 01-08-24
    "%d/%m/%y"	# 01/08/24
  ),
  unlikely = c(
    "%y-%m-%d",	# 24-08-01
    "%Y-%m",   	# 2024-08
    "%m/%Y",   	# 08/2024
    "%y/%m/%d"	# 24/08/01
  )
)
#save(date_formats,file= "data/date_formats.rda")




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
