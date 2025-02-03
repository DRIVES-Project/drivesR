
#' Fetch an entire table from Directus
#'
#' @param table_name 
#' The table identifier within Directus
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets
#' @param mylimit
#' Can be used to set a limit to the number of rows. By default, limit = "-1", which imports all rows.
#' It may be desirable to import rows in batches for very large tables. 
#' @returns
#' A data frame containing all rows and columns of the specified table
#' @export
#' @import httr
#' @import glue
#' @import jsonlite
#' @examples
#' testdf <- get_db_table("test_cat_info")
#' 
get_db_table <- function(table_name = "site_info", 
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}",
                         mylimit = "-1"){
  table_req <- GET(
    glue::glue("{myurl}/items/{table_name}?limit={mylimit}"),
    add_headers(
      "Authorization" = mytoken
    )
  )
  table_resp <- content(table_req, as="text")
  table_df <- jsonlite::fromJSON(table_resp)[["data"]]
  return(table_df)
}

#' Fetch schema information from directus
#'
#' @param mytarget 
#' The part of the URL indicating the part of the database you want schema information about. 
#' "collections" returns information about all tables. "collections/{table_name}" returns metadata on a 
#' specific collection. 
#' 
#' For information about columns within a table, use "fields/{table_name}". 
#' For information about relations in a table, use "relations/{table_name}/{column_name}
#'    
#' @param output_format 
#' By default, the output is formatted as a data frame. There is also the option to format as a prettified json string.
#' @param myurl 
#' Base URL for the drives database.
#' @param mytoken 
#' User-specific Directus API token, in the format "Bearer {myAPItoken}" without curly brackets.
#' @returns
#' A dataframe (default) or json string with schema and other metadata information pulled from directus
#' @export
#' 
#' @import httr
#' @import jsonlite
#' @import glue
#' 
#' @examples
#' collection_info <- get_db_info("collections")
#' site_field_info <- get_db_info("fields/site_info")
#' foreign_key_info <- get_db_info("relations")
#' 
get_db_info <- function(mytarget = "collections",
                        output_format = c("data.frame","json")[1],
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}"){
  if(!output_format %in% c("json","data.frame")){
    stop("output must be 'json' or 'data.frame'")
  }
  table_req <- httr::GET(
    glue::glue("{myurl}/{mytarget}"),
    httr::add_headers(
      "Authorization" = mytoken
    )
  )
  table_resp <- httr::content(table_req, as="text")
  if(output_format == "json"){
    output <- jsonlite::prettify(table_resp)
  }
  if(output_format == "data.frame"){
    output <- jsonlite::fromJSON(table_resp)[["data"]]  
  }
  return(output)
}


#' Download content from an API fetch request into a data frame
#'
#' This is useful for api_request() that involves a json_body object, such as filtering for conditions in a table.
#' @param apirequest 
#' An API request made with httr or api_request() with GET or SEARCH. 
#' @returns
#' A data-frame with contents from the API request.
#' @export
#' @import jsonlite
#' @import httr
#' @examples
get_table_from_req <- function(apirequest = NULL){
  resp <- httr::content(apirequest, as= "text")
  output <- jsonlite::fromJSON(resp)[["data"]]
  return(output)
}
