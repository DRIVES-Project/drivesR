# Functions to process database tables from directus.

#' Expand data frame by start and end years
#' 
#' Converts a table with time ranges specified as start- and end-year column into 
#' a longer table with separate rows for each year.
#'
#' @param mydf 
#' A data frame containing start- and end-year columns
#' @param na_end_year 
#' A year to use when end-year is NA. 
#' Start years must not have any NAs.
#' @param sycol
#' Name of the start-year column. Default is start_year. 
#' @param eycol 
#' Name of the end-year column. Default is end_year.
#' @param ycol 
#' Name of the new column for individual years. Default is year.
#' @returns
#' A data-frame with separate rows for each year between start and end year. 
#' Start and end year columns are removed. 
#' @export
#'
#' @examples
#' #Not run: exp_unit_trt <- get_db_table("experimental_units_treatments") 
#' #Not run: exp_trt_yearly <- expand_years(exp_unit_trt)
expand_years <- function(mydf = NULL,
                         na_end_year = 2021, 
                         sycol = "start_year",
                         eycol = "end_year", 
                         ycol = "year"){
  # replace NA end years with na_end_year
  mydf[which(is.na(mydf[,eycol])),eycol] <- ifelse(mydf[,sycol] > na_end_year, 
                                                   mydf[,sycol], na_end_year)
  
  if(any(is.na(mydf[,sycol]))){
    stop("start_year column must not contain NAs")
  }
  
  yearvec <- apply(mydf[,c(sycol,eycol)],1, function(x){return(x[1]:x[2])})
  repvec <- sapply(yearvec, length)
  rowreps <- sapply(1:nrow(mydf), function(i){rep(i,repvec[i])})
  expanded_df <- mydf[unlist(rowreps),]
  expanded_df[,ycol] <- unlist(yearvec)
  # remove start and end year columns
  outdf <- expanded_df[,!names(expanded_df) %in% c(sycol,eycol)]
  return(outdf)
}