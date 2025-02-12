

#' Delete rows in a table based on their primary key value
#'
#' @param table_name 
#' The table identifier in directus
#' @param pkvec 
#' A vector of primary keys for rows you want to delete in the table.
#' @param check_only
#' TRUE or FALSE indicating whether you want to inspect the rows instead of deleting them.
#' @returns
#' If check_only = TRUE, returns a data frame of the table subset corresponding to the primary 
#' keys in pkvec. if check_only = FALSE, the function performs delete operations on primary keys
#' within the specified table.
#' @export
#' @import httr
#' @importFrom glue glue
#' @examples
#' # not run: fakerowjson <- make_insert_json(data.frame("site_id" = c("fake_id1","fake_id2")))
#' # not run: api_request("POST","items/site_info", fakerowjson)
#' # not run: testrun <- delete_rows("site_info",c("fake_id1","fake_id2"), check_only = TRUE)
#' # not run: delete_rows("site_info",c("fake_id1","fake_id2"), check_only = FALSE)

delete_rows <- function(table_name = NULL, 
                        pkvec = NULL,
                        check_only = FALSE,
                        mytoken = NULL,
                        myurl = "https://data.drives-network.org"){
  if(check_only == TRUE){
    checkdf <- c()
    for(pk in pkvec){
      testrow <- get_db_info(glue::glue("items/{table_name}/{pk}"),
                             output_format = "data.frame",
                             mytoken = mytoken,
                             myurl = myurl)
      checkdf <- rbind(checkdf, testrow)
    }# closes for loop
    return(checkdf)
  }# closes if
  
  if(check_only == FALSE){
    for(pk in pkvec){
      api_request("DELETE",glue::glue("items/{table_name}/{pk}"))
    }# closes for loop
  }# closes if 
} #closes function
