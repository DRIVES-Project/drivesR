## this script has miscellaneous functions that are useful for manipulating strings or vectors of strings

#' Print a character vector as a list of quoted strings separated by commas.
#'
#' @param mystringvec A character vector.
#'
#' @return The character vector printed in the console in a way that can be copied into a vector in the code.
#' @export
#' Usually this is something I would use to trim excess names off a data frame.
#' @examples
#' df1 <- as.data.frame(matrix(1:26, nrow = 1, ncol = 26, dimnames = list(NULL,LETTERS)));
#' print_stringvec(LETTERS);
#' # Copy the output into the script, remove a handful of values I don't want
#' # for whatever arbitrary, difficult-to-code reason.
#' colsIWant <- c("A" ,"B" ,"C" ,"D" ,"M"  ,"S" ,"T" ,"U" ,"V" ,"W" ,"X" ,"Y" ,"Z")
#' df2 <- df1[,colsIWant]
#' 
print_stringvec_as_commasep <- function(mystringvec){
  output <- cat(paste(shQuote(mystringvec,type="cmd"), collapse=" ,"))
  print(output)
}
