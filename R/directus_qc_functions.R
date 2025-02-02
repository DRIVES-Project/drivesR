
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
#' # Not run: site_info_check <- check_dictionary(table_name = "site_info",mytoken = "Bearer {myAPItoken}") 
#' 
check_dictionary <- function(table_name = "site_info",mytoken = "Bearer {myAPItoken}"){
  ## get metadata info from Directus  
  table_info <- get_db_info(glue::glue("fields/{table_name}"),mytoken = mytoken)
    if(!is.null(table_info)){
      table_info <- jsonlite::flatten(table_info)
    }else{
      stop("table_name not found in Directus")
    }
    qlist <- list(
      query = list(
        filter = list(
          table_name = list(
            "_eq" = table_name
          )
        )
      )
    )
    
    # get rows from column dictionary
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
