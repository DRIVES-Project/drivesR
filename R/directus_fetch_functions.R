
#' Get column dictionary for a specified table
#' As an alternative to subsetting.
#' @param table_name 
#' @param myurl Base url for the Directus database.
#'
#' @returns
#' A dataframe consisting of the subsetted column dictionary for the corresponding table.
#' 
#' @export
#' @import httr
#' @import jsonlite
#' @examples
#' #not run: cdict <- get_column_dict_for_table("site_info")
#' 
get_column_dict_for_table <- function(table_name = "site_info",
                                myurl = getOption("drivesR.default.url") ){
  reqlist <- list("query"= list("filter" = 
                                  list("table_name" =
                                         list("_eq" = table_name)),
                                "limit" = -1))
  reqjson <- jsonlite::toJSON(reqlist,auto_unbox = TRUE)
  dictreq <- VERB("SEARCH",
                  url = glue::glue("{myurl}/items/column_dictionary"),
                  body = reqjson,
                  content_type_json())
  outdf <- get_table_from_req(dictreq)
  outdf <- outdf[order(outdf$column_order),]
  rownames(outdf) <- outdf$column_order
  #rownames(outdf) <- NULL
  return(outdf)
}


#' Fetch an entire table from Directus
#'
#' @param table_name 
#' The table identifier within Directus
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets.
#' This can be set with set_default_token.
#' @returns
#' A data frame containing all rows and columns of the specified table. 
#' If public is set to TRUE, the data will exclude sites and years not approved 
#' for public access. Also, data from Canadian sites will be fetched from a 
#' Canadian-hosted repository (Borealis).
#' 
#' @export
#' @import httr
#' @import glue
#' @import jsonlite
#' @import dplyr
#' @examples
#' #not run: testdf <- get_db_table("site_info") 
#' 
get_db_table <- function(table_name = "site_info",
                         myurl = getOption("drivesR.default.url"),
                         mytoken = getOption("drivesR.default.directustoken")
                         ){
  ## Check arguments-----
  ## conditions to stop execution with incompatible arguments
  validToken <- test_api_token(mytoken = mytoken,
                               myurl = myurl)
  if(!validToken & !is.null(mytoken)){
    stop("Invalid Directus API token.")
  }
  if(is.null(mytoken)){
    message("Directus API token set to NULL. Will only work for certain tables.")
  }
  
  ## get column order and data type from column dictionary.
  # this function sorts the output by column order. 
  column_order <- get_column_dict_for_table(table_name)[,c("column_name","postgres_data_type","column_order")]
  
  ## Bulk import------- 
  ## batch import didn't work for more than 10K rows.
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
    
    # convert geometry columns back to geojson.
    # could also go by class data frame in the column.
    geometrycols <- column_order$column_name[which(column_order$postgres_data_type=="geometry")]
    
    if(length(geometrycols) > 0){
      for(mycol in geometrycols){
        ## should ignore any geometry columns that are totally empty 
        if(class(table_df[,mycol])=="data.frame"){ # geojson will be read as data frame.
          mygeodf <- table_df[,mycol]
          outgeom <- apply(mygeodf, 1, function(x){ifelse(is.na(x["type"]),NA,jsonlite::toJSON(x, auto_unbox = TRUE))})
          table_df[,mycol] <- NA
          table_df[,mycol] <- outgeom
        }
      }
    }
    
    # put columns in the correct order
    table_df <- table_df[,column_order$column_name]
    # last step
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
#' # not run:
#' # this example demonstrates how a query filter might be set up 
#' # in an api request.
#' # querylist <- 
#' #   list("query"= list("filter" = 
#' #   list("table_name" =
#' #   list("_eq" = "site_info")),
#' #   "limit" = -1))
#' # queryjson <- toJSON(querylist, auto_unbox = TRUE)
#' # myreq <- api_request("SEARCH","items/column_dictionary", queryjson)
#' # outdf <- get_table_from_req(myreq)
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
#' @param public_tables
#' Vector of tables that receive the public_ prefix if public==TRUE.
#' Set as a global default. 
#' @returns
#' A dataframe of the specified tables subsetted for rows matching the 
#' primary key vector.
#' @export
#'
#' @examples
#' #not run: qtable <- query_table_by_pk("harvest_dates",pkvec = 1:100, pkfield = "uid)
#' 
query_table_by_pk <- function(
    table_name = NULL,
    pkvec = NULL,
    pkfield = "uid",
    public = getOption("drivesR.default.public"),
    public_tables = getOption("drivesR.default.tablevec")){

    # set table name as public or internal:
  if(public == TRUE & table_name %in% public_tables){
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
