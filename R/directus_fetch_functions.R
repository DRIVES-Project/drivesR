
#' Fetch an entire table from Directus
#'
#' @param table_name 
#' The table identifier within Directus
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets
#' @returns
#' A data frame containing all rows and columns of the specified table
#' @export
#' @import httr
#' @import glue
#' @import jsonlite
#' @examples
#' testdf <- get_db_table("test_cat_info")
#' 
get_db_table <- function(table_name = "test_cat_info", 
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}"){
  table_req <- GET(
    glue::glue("{myurl}/items/{table_name}"),
    add_headers(
      "Authorization" = mytoken
    )
  )
  table_resp <- content(table_req, as="text")
  table_df <- jsonlite::fromJSON(table_resp)[["data"]]
  return(table_df)
}
