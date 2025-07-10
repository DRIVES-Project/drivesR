
#' Make a template data frame from a vector of column names
#'
#' @param cnames A vector of column names
#' @param numrows Number of rows the template should have.
#'
#' @returns An empty data frame with specified column names
#' and number of rows.
#' @export
#'
#' @examples
#' cnames <- c("a","b","c")
#' templatedf <- make_template_df(cnames, 5)
#' print(templatedf) 
make_template_df <- function(cnames = NULL, numrows = 1){
  outdf <- as.data.frame(matrix(NA, nrow = numrows, ncol = length(cnames), dimnames = list(NULL, cnames)))
  return(outdf)
}
