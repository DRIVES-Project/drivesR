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
  narows <- which(is.na(mydf[,eycol]))
  mydf[narows,eycol] <- ifelse(mydf[narows,sycol] > na_end_year, 
                               mydf[narows,sycol], na_end_year)
  
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


#' Import dictionary tables
#'
#' @param public 
#' True or false indicating whether tables are in the set approved for public access.
#' @param ... 
#' Arguments passed to get_db_table(). Specifically, myurl and mytoken. 
#' @returns
#' A list of three dictionary tables. One describing database tables, one 
#' describing columns within tables, and one describing categories within columns. 
#' @export
#' @import purrr
#' @examples
#' # not run: dict <- import_dictionary_tables()
import_dictionary_tables <- function(public = FALSE,...){
  ## I'm a bit unclear on how I'm going to separate public and private tables. I 
  # may need a separate set of dictionaries. For now, just pretend there's only 
  # one set of dictionaries.
  dictvec <- paste0(c("table","column","category"),"_dictionary")
  dictnames <- dictvec # keep names if public dictionaries are being used. 
  if(public == TRUE){
    dictvec <- paste0("public_",dictvec)
  }
  outlist <- purrr::map(dictvec, ~get_db_table(table_name = .x,...))
  names(outlist) <- dictnames
  return(outlist)
}


#' Bulk download database tables
#'
#' @param tablevec
#' A vector of table names to download. If NULL, the function downloads all DRIVES tables
#' available to the user.  
#' @param save_locally 
#' TRUE or FALSE indicating whether the downloaded tables should be 
#' saved to an R data object.
#' @param savedir 
#' Directory path for saving locally. Defaults to the working directory.
#' @param savename 
#' File name for saving locally, excluding .Rda. Defaults to "drives_dblist"
#' @param ...
#' Arguments to be passed to get_db_table and get_db_info.
#' @returns
#' A list of DRIVES database tables. 
#' @export
#' @import jsonlite
#' @import purrr
#' @examples
#' # not run: db <- import_db_tables()
import_db_tables <- function(tablevec = NULL, 
                             save_locally = FALSE,
                             savedir = ".", 
                             savename = "drives_dblist",
                             ...){
  if(is.null(tablevec)){ 
    ## query the database for all table names available to the user.
    ## Note: I tried excluding internal directus collections as a query, but it 
    # didn't work.
    collection_info <- get_db_info(mytarget = "collections",output_format = "data.frame",... )
    ## remove internal directus tables. 
    tablevec <- collection_info$collection[which(!grepl("^directus_", collection_info$collection))]
    } # ends if(is.null(tablevec)
  db <- purrr::map(tablevec, ~get_db_table(table_name = .x,...))
  names(db) <- tablevec
  if(save_locally == TRUE){
    save(db, file = file.path(savedir, paste0(savename,".Rda")))
  }
  return(db)
}# ends function