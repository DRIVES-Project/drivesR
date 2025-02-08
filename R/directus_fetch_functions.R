
#' Fetch an entire table from Directus
#'
#' @param table_name 
#' The table identifier within Directus
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets
#' @param in_batches
#' When set to FALSE, all rows are imported with a single API call. When set to TRUE,
#' rows are imported in batches specified by batchsize. Batched import may be useful 
#' for large tables. 
#' 
#' @param batchsize
#' Sets the number of rows per batch-import when in_batches = TRUE.
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
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}",
                         in_batches = FALSE,
                         batchsize = NULL
                         ){
  ## Check arguments-----
  ## conditions to stop execution with incompatible arguments
  if(!in_batches %in% c(TRUE,FALSE)){
    stop("in_batches must be TRUE or FALSE")
  }
  limitTest <- ifelse(is.null(batchsize), TRUE, 
                      ifelse(is.na(as.numeric(batchsize)), TRUE,
                             !is_integer(batchsize) | as.numeric(batchsize) < 0))
  if(in_batches == TRUE & limitTest) {
    stop("When in_batches = TRUE, batchsize must be a positive integer")
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
 
  ## Batch import------ 
  if(in_batches == TRUE){
    outdf <- c() # empty data frame
    offset <- 0 # start with an offset of 0 
    breakcondition <- FALSE
    while(breakcondition==FALSE){
      
      batch_req <- GET(
        glue::glue("{myurl}/items/{table_name}?limit={batchsize}&offset={offset}"),
        add_headers(
          "Authorization" = mytoken
        )
      )
      ## check status of batch req. If successful, continue. 
      if(batch_req$status_code != 200){
        #update so it shows the row range that had problems.
        startrow = offset + 1
        endrow = startrow + batchsize
        emessage <- paste0("Could not fetch data for rows ",startrow," to ",endrow, ": status code ", table_req$status_code)
        stop(emessage)
      }# ends if for status code error. 
      
      batch_resp <- content(batch_req, as="text")
      batch_df <- jsonlite::fromJSON(batch_resp)[["data"]]
      ## convert request to data frame and append to outdf
      
      outdf <- rbind(outdf, batch_df)
      
      ### for each iteration, update the offset with offset + batchsize.
      offset <- offset + batchsize
      #include a condition to end once the fetched data has fewer rows than the batchsize
      breakcondition <- ifelse(is.null(nrow(batch_df)),TRUE,
                               nrow(batch_df) < batchsize)
    }# ends while
  return(outdf)  
  }# ends if batch import
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
#' collection_info <- get_db_info("collections")
#' site_field_info <- get_db_info("fields/site_info")
#' foreign_key_info <- get_db_info("relations")
#' 
get_db_info <- function(mytarget = "collections",
                        output_format = c("data.frame","json")[1],
                         myurl = "https://data.drives-network.org",
                         mytoken = "Bearer {myAPItoken}",
                         flatten = FALSE){
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

