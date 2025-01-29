
#' Map data types from Postgresql to Directus schema
#'
#' @param pgtype 
#' A postgres data type as specified in the column dictionary
#' @returns 
#' A character string with the corresponding datatype recognized by the Directus API.
#' @export
#'
#' @examples
#' pg_to_directus_type("array")
pg_to_directus_type <- function(pgtype){
  mapping_df <- data.frame(pg_type = c("integer",
                                       "text",
                                       "varchar(255)",
                                       "boolean",
                                       "numeric",
                                       "array",
                                       "date"
                                      ),
                           dir_type = c("integer",
                                        "text",
                                        "string",
                                        "boolean",
                                        "float",
                                        "text",
                                        "date"))
  if(length(pgtype) > 1){
    stop("pgtype must be of length 1")
  }
  if(pgtype %in% mapping_df$pg_type){
    return(as.character(mapping_df$dir_type[which(mapping_df$pg_type == pgtype)]))
  }else{
    stop("pgtype not in list")
  }
}

#' Make a collection schema json object from data dictionary tables
#'
#' @param columndf 
#' A dataframe of schema information about each column (field) from the column dictionary.
#' @param tablerow 
#' A one-row dataframe of schema information about the table (collection) from the collection dictionary
#'
#' @returns
#' A character string for the corresponding data type recognized by the Directus API.
#' @export
#' @import jsonlite
#'
#' @examples
#' my_collection_json <- make_collection_json(columndf = test_column_dict[which(test_column_dict$table_name=="test_cat_info"),],
#'                                             test_table_dict[1,])
#' 
#' 
make_collection_json <- function(columndf = test_column_dict[which(test_column_dict$table_name=="test_cat_info"),],
                                 tablerow = test_table_dict[1,]){
  field_json_list <- lapply(1:nrow(columndf), function(i){
    list(collection = columndf$table_name[i], 
         field = columndf$column_name[i],
         type = pg_to_directus_type(columndf$postgres_data_type[i]) ,
         meta = list( note = columndf$description[i],
                      sort = columndf$column_order[i]),
         schema = list(is_primary_key = columndf$primary_key[i],
                       is_nullable = columndf$nullable[i],
                       is_unique = columndf$unique_value[i],
                       has_auto_increment = columndf$auto_increment[i]
         ))  
  })
  collection_list <- list("collection" = tablerow$table_name,
                          "meta" = list(note = paste(tablerow$description, tablerow$organization, sep="\n ")),
                          "schema" = list("name" = tablerow$table_name),
                          "fields" = field_json_list)
  collection_json <-  jsonlite::toJSON(collection_list, pretty = TRUE, auto_unbox = TRUE) 
  return(collection_json)
}

#' Send a request to the Directus API
#' https://docs.directus.io/
#' https://directus.io/docs/api
#' 
#' @param myverb 
#' Verb in the http::VERB function. Typically POST, PATCH, SEARCH, GET, or DELETE
#' @param jsonbody 
#' A JSON object containing the content of the request, if applicable. For POST and PATCH, this is content you want to modify.
#' For SEARCH, the content will be some kind of filter query. This argument will typically be NULL for DELTE and GET requests.
#' @param mytarget
#' The part of the URL string pointing to the content you want to read or modify. 
#' @param myurl 
#' Root URL for the DRIVES database. 
#' @param mytoken 
#' API token for the user. This is formatted as "Bearer {APItoken}" (without the curly brackets).
#' It is recommended that the user make a script that loads the API token and database URL.
#' 
#' @returns
#' POST, PATCH, and DELETE requests will modify content on Directus and return a status message (https://directus.io/docs/guides/connect/errors)
#' Usually, 200 means the request was successful and anything else means the request failed. These kinds of requests can be saved to an R object or run as interactive code.
#' Both methods 
#' 
#' Requests that retrieve content from the database (GET, SEARCH) must be passed on to httr::content and downstream functions used to organize content. 
#' 
#' @export
#' @import httr
#' @import glue
#' @examples
#' # Accessing database content (here, it is information about collections)
#' collectionsreq <- api_request("GET", mytarget = "collections")
#' collectionjson <- jsonlite::toJSON(httr::content(collectionsreq), pretty=TRUE, auto_unbox = TRUE) 
#' 
#' # Modifying database content (here it is adding a new collection)
#' testcollection <- make_collection_json()# default with sample data dictionary 
#' new_collection_req <- api_request("POST", "collections",testcollection)
#' 
#' @seealso [${1:make_collection_json}()]
api_request <- function(myverb = "POST",
                         mytarget = "collections",
                         jsonbody = NULL ,
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}"){
  #require(httr)  
  if(!is.null(jsonbody)){
    VERB(myverb,
         glue::glue("{myurl}/{mytarget}"),
         add_headers(
           "Authorization" = mytoken
         ),
         body = jsonbody,
         content_type_json()
    )
  }else{
    VERB(myverb,
         glue::glue("{myurl}/{mytarget}"),
         add_headers(
           "Authorization" = mytoken
         )
    )
  }
}

