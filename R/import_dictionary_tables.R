#' Import dictionary tables
#' These do not require an API key.
#' @param myurl 
#' URL for the Directus database. Set as a default option (data.drives-network.org)
#' @returns
#' A list of three dictionary tables. One describing database tables, one 
#' describing columns within tables, and one describing categories within columns. 
#' @export
#' @import purrr
#' @import httr
#' @examples
#' # not run: dict <- import_dictionary_tables()
import_dictionary_tables <- function(){
  ## dictionaries do not require an API key to access.
  dictvec <- paste0(c("table","column","category"),"_dictionary")
  outlist <- purrr::map(dictvec, ~ suppressMessages(get_db_table(table_name = .x)))
  names(outlist) <- dictvec
  return(outlist)
}
