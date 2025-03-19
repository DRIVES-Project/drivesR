# script for quality control fucntions that come up often. 


#' Subset a dataframe for rows that are duplicated across a set of columns. 
#'
#' @param mydf A dataframe.
#' @param checkcols A vector of column names that you want to check for duplicates. Will also work with column indices. Alternative to dupvec.
#' @param dupvec A vector to use to check for duplicate values. Length(dupvec) = nrow(mydf). Alternative to column names. 
#' @param dupTFmatchesInput If TRUE, the TRUE/FALSE vector indicating duplicated values corresponds 
#' to the input dataframe. If FALSE, it corresponds to the subsetted dataframe containing duplicate values. 
#' values 
#' @return Returns a list containing the number of unique column combinations that have duplicate values (ndups),
#' a subsetted dataframe containing all duplicated values,
#' and a TRUE FALSE vector to facilitate removal of duplicate rows (created by duplicated). 
#' This vector can correspond to the input dataframe (mydf) or the output dataframe (dupdf),
#' depending on the choice for dupTFmatchesInput.
#' @export
#'
#' @examples testdf <- data.frame("col1" = c("spam","eggs","spam","spam"),"col2" = c(1,2,1,1),"col3" = c("cat","fish","dog","cat"),"col4" = c(1,2,3,4))
#' check_dups(testdf, checkcols = c("col1","col2"))
#' check_dups(testdf, checkcols = c(1,2,3))
check_dups <- function(mydf, checkcols = NULL, 
                       dupvec = NULL,
                       dupTFmatchesInput = TRUE){
  # test cases.
  if(is.null(checkcols) & is.null(dupvec))stop("You must include checkcols or dupvec.")
  if(!is.null(checkcols) & !is.null(dupvec))stop("Either checkcols or dupvec must be NULL.")
  if(is.null(dupvec) & is.vector(checkcols)){
    dupvec <- apply(mydf[,checkcols], 1, function(x){paste(x, collapse=";")})
  }
  mydups <- unique(dupvec[which(duplicated(dupvec))])
  numdupvals <- length(mydups)
  subrows <-which(dupvec %in% mydups)
  outdf <- mydf[subrows,]
  if(dupTFmatchesInput == TRUE){
    outdupTF <- duplicated(dupvec)
  }
  if(dupTFmatchesInput==FALSE){
    outdupTF <- duplicated(dupvec)[subrows]
  }
  
  return(list(ndups = numdupvals, dupdf = outdf, dupTF = outdupTF))
}


