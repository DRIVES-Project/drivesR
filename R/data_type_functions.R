
#' Check if a number is an integer
#'
#' @param x 
#' A vector of one or more values to test.
#' 
#' @param non_numbers_as_na 
#' The vector is coerced to numeric before checking it is an integer. 
#' If this option is set to TRUE (default), non-numeric values in the vector are returned as NA. 
#' If it is set to FALSE, non-numeric values are returned as FALSE
#'
#' @returns
#' A logical vector the same length as X with TRUE for integers, 
#' FALSE for non-numeric integers, and either FALSE or NA for non-numeric values.
#' 
#' @export
#'
#' @examples
#' x1 <- c(1,2.1,"a")
#' is_integer(x1, non_numbers_as_na = TRUE)
#' is_integer(x1, non_numbers_as_na = FALSE)
#' 
is_integer <- function(x, non_numbers_as_na = TRUE){
  xnum <- as.numeric(x)
  TFoutput <- xnum == round(xnum,digits=0)
  
  if(non_numbers_as_na == FALSE){
    TFoutput[is.na(TFoutput)] <- FALSE
  }
  return(TFoutput)
}
