
#' Get column dictionary for a specified table
#' As an alternative to subsetting.
#' @param table_name 
#' @param myurl 
#'
#' @returns
#' @export
#' @import httr
#' @import jsonlite
#' @examples
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
#' @param public
#' TRUE or FALSE indicating whether the table should be read from the 
#' public or internal portions of the DRIVES database. This can be set as a global option.
#' @param myurl 
#' The database url. By default, "https://data.drives-network.org"
#' @param mytoken 
#' The user-specific API token, in the format "Bearer {insertAPItoken}", without curly brackets.
#' This can be set with set_default_token.
#' @param public_tables
#' Vector of table names that receive the prefix 'public_' when public = TRUE.
#' @param borealis_repo_info 
#' A dataframe containing Borealis file identifiers for each table_name. 
#' If NULL, this information is imported from Directus. 
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
#' #not run: testdf <- get_db_table("site_info") # at once
#' #not run: testdf2 <- get_db_table("site_info", in_batches = TRUE, batchsize = 4)
#' 
get_db_table <- function(table_name = "site_info",
                         public = getOption("drivesR.default.public"),
                         myurl = getOption("drivesR.default.url"),
                         mytoken = getOption("drivesR.default.directustoken"),
                         public_tables = getOption("drivesR.default.tablevec"),
                         borealis_repo_info = NULL
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
  ## add public prefix if applicable 
  table_id <- ifelse(public == TRUE & table_name %in% public_tables,
                             paste0("public_", table_name), table_name)
  
  ## get column order from column dictionary.
  column_order <- get_column_dict_for_table(table_name)[,c("column_name","column_order")]
  
  ## Bulk import------- 
    table_req <- GET(
      glue::glue("{myurl}/items/{table_id}?limit=-1"),
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
    
    # import canadian table if: public is TRUE, table is in the list of public_tables,
    # table is not crop_info.
    if(public == TRUE & 
       table_name %in% public_tables &
       table_name != "crop_info"){
      candf <- get_canadian_data(table_name = table_name,
                                    borealis_repo_info = borealis_repo_info )
      table_df <- dplyr::bind_rows(table_df, candf)
      
    }
    # put columns in the correct order
    table_df <- table_df[,column_order$column_name]
    # last step
    return(table_df)
}

#' Get Canadian data from the Borealis Repository
#' This function is called by get_db_table if the 
#' public option is set to TRUE.
#'
#' @param table_name
#' Name of the drives database table, as shown in table_dictionary. 
#' @param dataverse_url 
#' Server URL for the Borealis repository, part of Dataverse.
#' By default is set to "https://borealisdata.ca"
#' @param dataverse_doi
#' Persistent identifier for the collection in Borealis. This is set 
#' to a default. 
#' @param dataverse_api 
#' API key for the Borealis repository. This option is included for troubleshooting
#' before the repository data tables are published. Once they are published, this option
#' can remain as NULL.
#' @param borealis_repo_info 
#' A dataframe containing Borealis file identifiers for each table_name. 
#' If NULL, this information is imported from Directus. Set with set_default_token().
#' @param directus_url
#' URL for the Directus database (can be set with global options).
#' @returns
#' A dataframe with rows for the two Canadian sites for the specified table_name. 
#' This can be combined with the public Direcctus table using bind_rows.
#' @export
#' @import httr
#' @import jsonlite
#'
#' @examples
get_canadian_data <- function(table_name = NULL,
                                  dataverse_url = "https://borealisdata.ca",
                                  dataverse_doi = "doi:10.5683/SP3/QGLCKO",
                                  dataverse_api = getOption("drivesR.default.dataversetoken"),
                                  borealis_repo_info = NULL,
                                  directus_url =getOption("drivesR.default.url") ){
  ## import borealis_repo_info if NULL
  if(is.null(borealis_repo_info)){
    ## doesn't require a token.
    brireq <- GET(url = glue::glue("{directus_url}/items/borealis_repo_info?limit=-1"))
    borealis_repo_info <- get_table_from_req(brireq)
      
  }
  #fetch File id from borealis_repo_info-----------
  myFileId <- borealis_repo_info$repo_file_id[which(borealis_repo_info$table_name==table_name)]
  
  #fetch table from Borealis  -----------
  ## for now, use my API key. This won't require a key
  # once it's published.
  breq <- GET(url =glue::glue("{dataverse_url}/api/access/datafile/{myFileId}"),
              add_headers(
                `X-Dataverse-key` = dataverse_api  
              )
    )
  if(breq$status_code != 200){
    stop("Request failed with status code ", breq$status_code)
  }
  bdf <- utils::read.table(text = content(breq,as = "text"),sep = "\t",header =TRUE, na.strings = "")
  return(bdf)
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
#' @param public_tables
#' Vector of tables that receive the public_ prefix if public==TRUE.
#' Set as a global default. 
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
