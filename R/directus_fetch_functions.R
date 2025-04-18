
#' Fetch an entire table from Directus
#'
#' @param table_name 
#' The table identifier within Directus
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets
#'  
#' @returns
#' A data frame containing all rows and columns of the specified table.
#' 
#' @export
#' @import httr
#' @import glue
#' @import jsonlite
#' @examples
#' #not run: testdf <- get_db_table("site_info") # at once
#' #not run: testdf2 <- get_db_table("site_info", in_batches = TRUE, batchsize = 4)
#' 
get_db_table <- function(table_name = "site_info", 
                         myurl = getOption("drivesR.default.url"),
                         mytoken = getOption("drivesR.default.directustoken"),
                         ){
  # TODO: integrate public and canadian options------
  ## Check arguments-----
  ## conditions to stop execution with incompatible arguments
  validToken <- test_api_token(mytoken = mytoken,
                               myurl = myurl)
  if(!validToken){
    stop("Invalid Directus API token.")
  }

  ## Bulk import------- 
  if(in_batches==FALSE){
    table_req <- GET(
      glue::glue("{myurl}/items/{table_name}?limit=-1"),
      add_headers(
        "Authorization" = mytoken
      )
    )
    if(table_req$status_code != 200){
      ## error message
      emessage <- paste0("Could not fetch data, status code ", table_req$status_code)
      stop(emessage)
    }
    
    table_resp <- content(table_req, as="text")
    table_df <- jsonlite::fromJSON(table_resp)[["data"]]
    return(table_df)
  }
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
#' @param flatten
#' If FALSE (default), the data dataframe will be nested. If TRUE, it will be flattened. Irrelevant if output is json.
#' @returns
#' A dataframe (default) or json string with schema and other metadata information pulled from directus
#' @export
#' 
#' @import httr
#' @import jsonlite
#' @import glue
#' 
#' @examples
#' # collection_info <- get_db_info("collections")
#' # site_field_info <- get_db_info("fields/site_info")
#' # foreign_key_info <- get_db_info("relations")
#' 
get_db_info <- function(mytarget = "collections",
                        output_format = c("data.frame","json")[1],
                         myurl = getOption("drivesR.default.url"),
                         mytoken = getOption("drivesR.default.directustoken"),
                         flatten = FALSE){
  # check for valid API token.
  validToken <- test_api_token(mytoken = mytoken,
                               myurl = myurl)
  if(!validToken){
    stop("Invalid Directus API token.")
  }
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
    if(flatten==TRUE){
      output <- jsonlite::flatten(output)
    }
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

#' Fetch table contents by primary keys
#' Function to query contents of a database table with a vector of primary key values.
#' Useful for double-checking before running 'modify_rows' or 'delete_rows'.
#' It could be useful for querying information across tables. 
#'
#' @param table_name
#' The table identifier in the DRIVES database. If public = TRUE, the table name is automatically 
#' modified to query the public version of the database table. 
#' @param pkvec 
#' A vector of primary key values.
#' @param pkfield
#' The name of the column name that holds the table's primary key. 
#' For most tables, this is 'uid'. 
#' @param public
#' If TRUE, the function queries publicly available data tables. 
#' Since this function is mostly for internal use, the default is FALSE. 
#' @param mytoken 
#' Directus token, formatted as "Bearer APITOKEN". 
#' Can be set with set_default_token. 
#' @param myurl 
#' Directus URL. Set at package loading ("https://data.drives-network.org").
#' @returns
#' @export
#'
#' @examples
#' #not run: qtable <- query_table_by_pk("harvest_dates",pkvec = 1:100, pkfield = "uid)
query_table_by_pk <- function(
    table_name = NULL,
    pkvec = NULL,
    pkfield = "uid",
    public = getOption("drivesR.default.public"),
    mytoken = getOption("drivesR.default.directustoken")){

    # set table name as public or internal:
  if(public == TRUE & !grepl("dictionary", table_name)){
    # TODO: set up Canadian system------
    tname <- paste0("public_",table_name)
  }else{
    tname <- table_name
  }
  # set up request
  
  reqlist <- list("query"= list("filter" = 
                    list("fieldname" = 
                           list("_in" = pkvec)),
                  "limit" = -1))
  names(reqlist[[1]][[1]]) <- pkfield
  reqjson <- jsonlite::toJSON(reqlist,auto_unbox = TRUE)
  testreq <- api_request("SEARCH", glue::glue("items/{tname}"),jsonbody = reqjson)
  outdf <- get_table_from_req(testreq)
  return(outdf)
}
