#' Possible date formate
#' 
#' A list of date formats that may occur in data.
#' 
#' @format 'date_formats'
#' A list with subvectors containing different date formats recognized by date functions in R.
#' @source Internal.
#' 
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

