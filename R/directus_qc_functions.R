
#' Check dictionary metadata
#' 
#' Check for discrepancies between collection metadata stored in Directus
#'  and in data dictionaries.
#'  
#' @param table_name
#' Table (collection) name.
#'   
#' @param mytoken 
#' Directus token. Can be set in the function or via set_default_token()E
#' @returns
#' A data frame with a set of conditions, outcome (TRUE = pass or FALSE = fails), and 
#' fields that fail the condition. In case of mismatched field names, there are separate 
#' columns for fields in the column_dictionary (dcols) and Directus table schema (tcols).
#' 
#' @export
#' @importFrom glue glue
#' @import jsonlite
#' @import httr
#' @importFrom dplyr inner_join
#' @examples
#' # set mytoken in function or with set_default_token()
#' # Not run:  site_info_check <- check_dictionary(table_name = "site_info",mytoken = "Bearer {myAPItoken}") 
#' 
check_dictionary <- function(table_name = "site_info",mytoken = "Bearer {myAPItoken}"){
  ## get metadata info from Directus  
  table_info <- get_db_info(glue::glue("fields/{table_name}"),mytoken = mytoken)
    if(!is.null(table_info)){
      table_info <- jsonlite::flatten(table_info)
    }else{
      stop("Invalid token or collection url.")
    }
  # get rows from column dictionary  
  qlist <- list(
      query = list(
        filter = list(
          table_name = list(
            "_eq" = table_name
          )
        )
      )
    )
  
    qjson <- jsonlite::toJSON(qlist, pretty = TRUE, auto_unbox = TRUE)
    
    dict_req <- api_request("SEARCH","items/column_dictionary",qjson, mytoken = mytoken)
    dict_table <- get_table_from_req(apirequest = dict_req)
    if(length(dict_table) ==0){
      stop("table_name not found in column_dictionary")
    }
    dict_table <- dict_table[order(dict_table$column_order),]
    ## make a merged table to ease comparison.
    names(dict_table) <- paste0("dict.", names(dict_table))
    combined_table <- dplyr::inner_join(dict_table, table_info, by = c("dict.column_name"="schema.name")) ## omits rows whose names don't match
    
    checkdf <- data.frame(condition = NA, outcome = NA, dcols=NA, tcols = NA)
    
    # Checks:
    ## number of fields matches
    cond1name <- "Number of columns match"
    cond1 <- nrow(table_info) == nrow(dict_table)
    row1 <- c(cond1name,cond1)
    checkdf[1,1:2] <-row1
    
    ## No mismatched field names in dictionary
    condname <- "No mismatched fields in dictionary"
    mismatched_dict_fields <- setdiff(dict_table$dict.column_name, table_info$field)
    cond <- length(mismatched_dict_fields)==0
    if(!cond){
      dfields <- paste(mismatched_dict_fields, collapse=";")
    }else{dfields <- NA}
    newrow <- c(condname, cond, dfields,NA)
    checkdf <- rbind(checkdf, newrow)
    
    ## No mismatched field names in table
    condname <- "No mismatched fields in table"
    mismatched_table_fields <- setdiff(table_info$field,dict_table$dict.column_name)
    cond <- length(mismatched_table_fields)==0
    if(!cond){
      tfields <- paste(mismatched_table_fields, collapse=";")
    }else{tfields <- NA}
    newrow <- c(condname,cond, NA,tfields)
    checkdf <- rbind(checkdf, newrow)

    ## fields are in the correct order (after sorting by column order)
    condname <- "Fields are in the same order"
    cond <- identical(table_info$field, dict_table$dict.column_name)
    newrow <- c(condname, cond,NA,NA)
    checkdf <- rbind(checkdf, newrow)
    
    # Constraints------
    ## data type
    combined_table[,"dict.type"] <-sapply(combined_table$dict.postgres_data_type, 
                                   function(x){pg_to_directus_type(x)})
    
    ## Most constraints can be done in a loop.
    # foreign key involves more columns, so will be done separately
    constraint_mat <- rbind("Same data type"=
                              c("dict.type","type"),
                            "Same primary key option"= 
                              c("dict.primary_key","schema.is_primary_key"),
                            "Same unique option" =
                              c("dict.unique_value","schema.is_unique"),
                            "Same nullable option"=
                              c("dict.nullable","schema.is_nullable"),
                            "Same autoincrement option" =
                              c("dict.auto_increment","schema.has_auto_increment"))
    for(i in 1:nrow(constraint_mat)){
      condname <- rownames(constraint_mat)[i] 
      dictname <-  constraint_mat[i,1]
      tname <- constraint_mat[i,2]
      mismatch <- combined_table$dict.column_name[which(combined_table[,dictname] != 
                                                        combined_table[,tname])]
      cond <- length(mismatch) == 0 
      dfield <- ifelse(!cond, paste(mismatch, collapse=";"), NA)
      newrow <- c(condname, cond,dfield,NA)
      checkdf <- rbind(checkdf, newrow)
    }
    
    # foreign key. 
    condname <- "Same foreign key option"
    mismatch <- combined_table$dict.column_name[which(
        combined_table$dict.foreign_key_table != combined_table$schema.foreign_key_table |
          combined_table$dict.foreign_key_column != combined_table$schema.foreign_key_column
        )]
    cond <- length(mismatch) == 0 
    dfield <- ifelse(!cond, paste(mismatch, collapse=";"), NA)
    newrow <- c(condname, cond,dfield,NA)
    checkdf <- rbind(checkdf, newrow)
    
    #output
    return(checkdf)

}



#' Check category dictionary
#' 
#' Checks for discrepancies between the category dictionary and 
#' column dictionary and input data. 
#' 
#' @param table_name 
#' The table (collection) identifier
#' 
#' @param mytoken 
#' Directus token. Can be set in the function or via set_default_token()
#' 
#' @param inputdf 
#' A data frame of prospective rows to be added to the table. 
#' If NULL, the function checks the category and column dictionaries. 
#' 
#' @returns
#' If inputdf = NULL, it returns a data frame describing conditions and outcomes corresponding to 
#' particular tables and fields. 
#' If inputdf is provided, it returns a list with a dataframe of conditions (checkdf) and a list of 
#' categorical values in the input data that are missing from the category dictionary. This list can
#' be used to ammend the category dictionary. 
#' 
#' @export
#' @import jsonlite
#' @import httr
#' @examples
#' #Not run: check_categories("site_info") 
#' 
check_categories <- function(table_name = "site_info",mytoken = "Bearer {myAPItoken}", inputdf = NULL){
  ## filter query for column and category dictionary.
  qlist <- list(
    query = list(
      filter = list(
        table_name = list(
          "_eq" = table_name
        )
      )
    )
  )
  qjson <- jsonlite::toJSON(qlist, pretty = TRUE, auto_unbox = TRUE)
  col_dict_req <- api_request("SEARCH","items/column_dictionary",qjson, mytoken = mytoken) 
  col_dict_table <- get_table_from_req(apirequest = col_dict_req)
  if(length(col_dict_table) ==0){
    stop("table_name not found in column_dictionary")
  }
  cat_fields_tdict <- col_dict_table$column_name[which(col_dict_table$is_category)]
  if(length(cat_fields_tdict)==0){
    stop("no category fields in column dictionary")
  }
  cat_dict_req <- api_request("SEARCH","items/category_dictionary",qjson, mytoken = mytoken)
  cat_dict_table <- get_table_from_req(apirequest = cat_dict_req)
  if(length(cat_dict_table) ==0){
    stop("table_name not found in category dictionary")
  }
  # check that fields marked as categories in the column dictionary are included in the category dictionary
  checkdf <- data.frame(table = NA,condition = NA, outcome = NA,fields=NA)
  cat_fields_catdict <- unique(cat_dict_table$column_name)
  in_cat_not_cdict <- setdiff(cat_fields_catdict, cat_fields_tdict)
  
  condname <- "All fields are categories in column_dictionary" 
  cond <- length(in_cat_not_cdict) == 0
  dfields <- ifelse(cond, NA, paste(in_cat_not_cdict, collapse=";"))
  checkdf[1,] <- c("category_dictionary",condname, cond, dfields)
  
  # check that the categorical values in the data frame are accounted for in the category dictionary. 
  in_cdict_not_cat <- setdiff(cat_fields_tdict, cat_fields_catdict)
  condname <- "No fields are missing from category_dictionary"
  cond <- length(in_cdict_not_cat) == 0
  dfields <- ifelse(cond, NA, paste(in_cdict_not_cat, collapse=";"))
  checkdf <- rbind(checkdf,c("column_dictionary",condname, cond, dfields))
  
  # stop here if not providing an input data frame--just checking dictionaries. 
  if(is.null(inputdf)){
    return(checkdf) 
  }else{
    # check for missing category columns
    missingcat <- setdiff(cat_fields_catdict, names(inputdf))
    condname <- "No missing category columns"
    cond <- length(missingcat) == 0
    dfields <- ifelse(cond, NA, paste(missingcat, collapse=";"))
    checkdf <- rbind(checkdf,c("inputdf",condname, cond, dfields))
    
    # check for category values in input data missing from category dictionary
    ## use field names from category dictionary
    addvals <- c()
    addvals_names <- c()
    for(i in 1:length(cat_fields_catdict)){
      cf <- cat_fields_catdict[i]
      inputvals <- unique(inputdf[,cf][!is.na(inputdf[,cf])])
      catvals <- cat_dict_table$category_level[which(cat_dict_table$column_name==cf)]
      ## category values in input data missing from category_dictionary
      inputvals_not_catvals <- setdiff(inputvals, catvals)
      if(length(inputvals_not_catvals) > 0){
        addvals_names <- c(addvals_names, cf)
        addvals <- append(addvals, list(inputvals_not_catvals))
      }
    }
    names(addvals) <- addvals_names
    
    condname <- "All values present in category_dictionary"
    cond <- length(addvals) == 0
    if(!cond){
      dfields <- paste(addvals_names, collapse=";")
    }
     
    checkdf <- rbind(checkdf,c("inputdf",condname, cond, dfields))
    
    return(list(checkdf = checkdf, missing_cats = addvals))
    
  } #end else 
}#end function


#' Check column names
#' 
#' Quality control step to check column names in a data frame to be imported into Directus.
#' 
#' @param table_name 
#'  Table identifier, such as "site_info"
#' @param inputdf 
#'  Dataframe to be evaluated. 
#'  
#' @param mytoken
#' Directus token. Can be set in the function or through set_default_token()
#'
#' @returns
#' A dataframe of checks, outcomes, and relevant fields. 
#' 
#' @export
#' 
#' @import jsonlite
#' @import httr
#' @import glue
#' @examples
#' #Not run: check_column_names("site_info", mydf)
check_column_names <- function(table_name = "site_info", inputdf = NULL, mytoken = mytoken){
  ## get fields from directus
  table_info <- get_db_info(glue::glue("fields/{table_name}"),mytoken = mytoken)
  if(!is.null(table_info)){
    table_info <- jsonlite::flatten(table_info)
  }else{
    stop("Invalid token or collection url.")
  }
  
  checkdf <- data.frame("condition"=NA,"outcome"=NA,"fields"=NA)
  
  # fields in inputdf not in collection
  condname <- "Inputdf fields match collection"
  in_input_not_coll <- setdiff(names(inputdf), table_info$field)
  cond <- length(in_input_not_coll) == 0
  dfields <- ifelse(cond,NA, paste(in_input_not_coll, collapse=";"))
  checkdf[1,] <- c(condname, cond, dfields)
  
  # fields in collection missing from inputdf
  condname <- "All fields present in inputdf"
  in_coll_not_input <- setdiff(table_info$field, names(inputdf))
  cond <- length(in_coll_not_input) == 0
  dfields <- ifelse(cond,NA, paste(in_coll_not_input, collapse=";"))
  checkdf <- rbind(checkdf, c(condname, cond, dfields))
  
  return(checkdf)
}