
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
#' Root URL for the DRIVES database. Set as a default when the package loads ("https://data.drives-network.org/)
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
#' # not run: collectionsreq <- api_request("GET", mytarget = "collections")
#' # not run: collectionjson <- jsonlite::toJSON(httr::content(collectionsreq), pretty=TRUE, auto_unbox = TRUE) 
#' 
#' # Modifying database content (here it is adding a new collection)
#' #not run: testcollection <- make_collection_json()# default with sample data dictionary 
#' #not run: new_collection_req <- api_request("POST", "collections",testcollection)
#' 
#' @seealso [${1:make_collection_json}()]
api_request <- function(myverb = "POST",
                        mytarget = "collections",
                        jsonbody = NULL ,
                        myurl = getOption("drivesR.default.url"),
                        mytoken = getOption("drivesR.default.directustoken")){
  
  validToken <- test_api_token(mytoken = mytoken,
                               myurl = myurl)
  if(!validToken){
    stop("Invalid Directus API token.")
  }
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

#' Make a collection schema json object from data dictionary tables
#'
#' @param columndf 
#' A dataframe of schema information about each column (field) from the column dictionary.
#' @param tablerow 
#' A one-row dataframe of schema information about the table (collection) from the table dictionary.
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

#' Make relations json schema objects from data dictionary tables
#'
#' @param columndf 
#' A dataframe of schema information about each column (field) from the column dictionary.
#' @param tablerow 
#' A one-row dataframe of schema information about the table (collection) from the table dictionary.
#' @param update_action
#' Action if the foreign key value is modified in it's original table. The default is CASCADE, which
#' means that the update will cascade to related records. NO ACTION prevents records from being modified if they have
#' dependent records 
#' 
#' @param delete_action 
#' Action if the foreign key is deleted in its original table. The default is NO ACTION, which prevents records
#' from being modified if they have dependent records. Another useful option might be SET DEFAULT or SET NULL.
#'
#' @returns
#' A list of json objects. This list must be subsetted to use in api calls
#' @export
#' 
#' @examples
#' 
#' #not run: testrel <- make_relations_json(columndf = test_column_dict[which(test_column_dict$table_name=="test_favorite_toy"),],
#' #                                tablerow = test_table_dict[2,])
#
#' # If the table has multiple foreign keys, you can use this in a loop (lapply doesn't work)
#' #not run: rel_req <- api_request("POST",mytarget = "relations",jsonbody = testrel[[1]])
#' 
#' 
#' @import jsonlite
make_relations_json <- function(columndf = test_column_dict[which(test_column_dict$table_name=="test_favorite_toy"),],
                                tablerow = test_table_dict[2,],
                                update_action = "CASCADE",
                                delete_action = "NO ACTION"){
  fkdf <- columndf[which(!is.na(columndf$foreign_key_table)),]
  if(nrow(fkdf) > 0){
    rel_json <- lapply(1:nrow(fkdf), function(i){
      rel_list <- list(collection = tablerow$table_name,
                       field = fkdf$column_name[i],
                       related_collection = fkdf$foreign_key_table[i],
                       schema = list(constraint_name = paste0("fkey_",fkdf$column_name),
                                     table = tablerow$table_name,
                                     column = fkdf$column_name[i],
                                     foreign_key_table = fkdf$foreign_key_table[i],
                                     foreign_key_column = fkdf$foreign_key_column[i],
                                     on_update = update_action, 
                                     on_delete = delete_action
                       ),
                       meta = NA
      )
      return(jsonlite::toJSON(rel_list, pretty=TRUE, auto_unbox = TRUE))
    })
    return(rel_json)
  }
}

#' Convert a dataframe or tibble to a json object suitable to insert rows (items)
#'  
#' Basically just the jsonlite::toJSON function with options set to non-defaults 
#' for conventience
#' @param mydf 
#' A dataframe or tibble with quality control steps complete
#' @returns
#' A prettified json object 
#' @export
#' @import jsonlite
#'
#' @examples
#' testdf <- data.frame(x = 1:5, y = LETTERS[1:5])
#' insert_json <- make_row_insert_json(testdf)
#' # example with API request:
#' test_insert <- make_row_insert_json(test_cat_info)
#' # insert_req <- api_request("POST","items/test_cat_info",test_insert)
make_row_insert_json <- function(mydf){
  jsonlite::toJSON(mydf, pretty = TRUE, auto_unbox=TRUE)
}


#' Convert column dictionary content into json-formatted field schema.
#' This is for adding fields to existing collections. Once 
#' a collection exists, only one field can be added at a time with 
#' an API request.
#'
#' @param column_dictionary_row 
#' A row from the column dictionary, filled out with information for 
#' the new field.
#' @returns
#' A json-formatted object containing schema information for a new field in an 
#' existing collection.
#' 
#' @export
#'
#' @examples
#' column_dictionary <- test_column_dict
#' ## model the new field on an existing field (for demo purposes)
#' newrow <- test_column_dict[which(test_column_dict$column_name == "cat_name"),]
#' newrow$column_id <- "test_cat_info:favorite_food"
#' newrow$column_name <- "favorite_food"
#' newrowjson <- make_field_json(newrow)
#' # not run: newfieldreq <- api_request("POST",mytarget ="fields/test_cat_info",jsonbody= newrowjson)
make_field_json <- function(column_dictionary_row = NULL){
  if(nrow(as.data.frame(column_dictionary_row)) != 1){
    stop("Input must be a single-row data frame.")
  }
  fieldlist <- list(collection = column_dictionary_row$table_name, 
                    field = column_dictionary_row$column_name,
                    type = pg_to_directus_type(column_dictionary_row$postgres_data_type) ,
                    meta = list( note = column_dictionary_row$description,
                                 sort = column_dictionary_row$column_order),
                    schema = list(is_primary_key = column_dictionary_row$primary_key,
                                  is_nullable = column_dictionary_row$nullable,
                                  is_unique = column_dictionary_row$unique_value,
                                  has_auto_increment = column_dictionary_row$auto_increment
                    ))  
  
  fieldjson <- jsonlite::toJSON(fieldlist, pretty = TRUE, auto_unbox = TRUE) 
  return(fieldjson)  
}


#' Add rows to a database table
#' 
#' This adds all rows in an input dataframe in a single API request. To add
#' more than 100 rows, use post_rows_in_batches. For use in an interactive R session.
#'    
#' @param table_name 
#' The name of the database table.  
#' @param inputdf
#' The dataframe containing rows to be added. Contents should have been checked.
#'  
#' @param mytoken 
#' Directus token, formatted as "Bearer mytoken."
#' @returns
#' Converts the input df to a json and runs an API post request on the specified table. 
#' The status code of that request is returned as a message.
#' @export
#'
#' @examples
post_rows <- function(table_name = NULL,
                      inputdf = NULL,
                      mytoken = getOption("drivesR.default.directustoken")){
  insert_json <- make_row_insert_json(inputdf)
  myreq <- api_request("POST",glue::glue("items/{table_name}"),insert_json,mytoken = mytoken)
  message(paste("POST request complete with status code",myreq$status_code))
}

#' Post rows in batches
#' 
#' To get around issues of posting limits.
#'
#' @param table_name
#' Table identifier in the database.
#' 
#' @param batchsize 
#' Number of rows to be added at a time. Suggested amount is 1000 rows.
#' Progress is printed to the console.
#' 
#' @param inputdf 
#' Data frame of rows to be added. This should have passed quality control checks.
#' 
#' @param mytoken 
#' Directus token. Can be set with set_default_token()
#' @returns
#' Silent.
#' @export
#'
#' @examples
post_rows_in_batches <- function(table_name = "crop_yields", inputdf = NULL,batchsize = 1000,mytoken = getOption("drivesR.default.directustoken")){
  nitems = nrow(inputdf)
  nbatches = ceiling(nitems/batchsize)
  start_i = 1
  end_i = 0
  batch_i = 1
  
  while(end_i < nitems){
    end_i <- min(start_i + batchsize -1 , nitems)
    subsetdf <- inputdf[start_i:end_i,]
    insert_json <- make_row_insert_json(subsetdf)
    myreq <- api_request("POST",glue::glue("items/{table_name}"),insert_json,mytoken = mytoken)
    message(paste("Batch",batch_i,"of",nbatches,"status",myreq$status_code))
    #cat(paste0("\nstart_i = ",start_i,", end_i = ",end_i,", batch_i = ",batch_i))
    start_i <- end_i + 1
    batch_i <- batch_i + 1
  }
}