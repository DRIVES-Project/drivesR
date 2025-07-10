
#' Check dictionary metadata for a database table.
#' 
#' Check for discrepancies between collection metadata stored in Directus
#'  and in data dictionaries for a specified table. Conditions tested are:
#'  1) The table_name is present in the column_dictionary.
#'  2) The number of columns matches between the table schema and column_dictionary.
#'  3) Column names match between the table schema and column_dictionary.
#'  4) Column names are in the same order, as specified by the column_order field in in column_dictionary. 
#'  5) Data constraints coded in the column_dictionary match what is in the table schema.
#'  These constraints are:
#'  - data type
#'  - primary key
#'  - unique
#'  - nullable
#'  - autoincremented
#'  - foreign key tables and fields. 
#'   
#'  
#' @param table_name
#' Table (collection) name.
#'   
#' @param mytoken
#' API token, formatted as "Bearer apitoken"
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
#' # Not run:  site_info_check <- check_dictionary(table_name = "site_info") 
#' 
check_dictionary <- function(table_name = "site_info",mytoken = getOption("drivesR.default.directustoken")){
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
  cond1name <- "Number of columns match."
  cond1 <- nrow(table_info) == nrow(dict_table)
  row1 <- c(cond1name,cond1)
  checkdf[1,1:2] <-row1
  
  ## No mismatched field names in dictionary
  condname <- "No mismatched column names in dictionary."
  mismatched_dict_fields <- setdiff(dict_table$dict.column_name, table_info$field)
  cond <- length(mismatched_dict_fields)==0
  if(!cond){
    dfields <- paste(mismatched_dict_fields, collapse=";")
  }else{dfields <- NA}
  newrow <- c(condname, cond, dfields,NA)
  checkdf <- rbind(checkdf, newrow)
  
  ## No mismatched field names in table
  condname <- "No mismatched column names in table schema"
  mismatched_table_fields <- setdiff(table_info$field,dict_table$dict.column_name)
  cond <- length(mismatched_table_fields)==0
  if(!cond){
    tfields <- paste(mismatched_table_fields, collapse=";")
  }else{tfields <- NA}
  newrow <- c(condname,cond, NA,tfields)
  checkdf <- rbind(checkdf, newrow)
  
  ## fields are in the correct order (after sorting by column order)
  condname <- "Column names are in the same order"
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
  condname <- "Same foreign key options"
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
#' First the function tests a set of preliminary conditions. Failure
#' in any of these throws out an error message.
#' 1) The table name is present in the column dictionary
#' 2) The column dictionary for this table contains no category fields.
#' 3) The table name is present in the category dictionary.
#' 
#' If these conditions are met, the function checks for correspondence 
#' between the category dictionary and the column dictionary. These conditions 
#' are:
#' 1) All column_name values in the category_dictionary are specified as categories in column_dictionary.
#' 2) No fields are missing from category_dictionary
#' 
#' If an input table is provided under the inputdf argument, the function checks that
#' the contents of this table match what is in the category dictionary. 
#' The two conditions are: 
#' 1) No category columns are missing from inputdf.
#' 2) All category levels in inputdf are present in category_dictionary.
#' 
#' @param table_name 
#' The table (collection) identifier
#' 
#' @param mytoken
#' API token, formatted as "Bearer apitoken". 
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
check_categories <- function(table_name = "site_info",
                             inputdf = NULL, 
                             mytoken = getOption("drivesR.default.directustoken")){
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
  
  condname <- "All column_name values in the category_dictionary are specified as categories in column_dictionary." 
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
    condname <- "No category columns are missing from inputdf."
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
    
    condname <- "All category levels in inputdf are present in category_dictionary."
    cond <- length(addvals) == 0
    if(!cond){
      dfields <- paste(addvals_names, collapse=";")
    }
    
    checkdf <- rbind(checkdf,c("inputdf",condname, cond, dfields))
    
    return(list(checkdf = checkdf, missing_cats = addvals))
    
  } #end else 
}#end function


#' Check column names for a staged table.
#' 
#' Quality control step to check that column names in a data frame to be imported into Directus
#' match what is in the table schema.
#' 
#' @param table_name 
#'  Table identifier, such as "site_info"
#' @param inputdf 
#'  Dataframe to be evaluated. 
#'  
#' @param mytoken
#' directus API token, as "Bearer apitoken"
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
check_column_names <- function(table_name = "site_info", inputdf = NULL, mytoken = getOption("drivesR.default.directustoken")){
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



#' Check table contents before import
#' 
#' This function checks contents of a table (usually imported as a CSV) for
#' possible failures in validation constraints. 
#' The constraints checked are:
#' 1) is_nullable
#' 2) max_length (if applicable)
#' 3) is_unique
#' 4) data type.
#' Column names and foreign key values are checked with different functions.
#' @param table_name 
#' Name of the table in Directus
#' 
#' @param inputdf 
#' data frame intended for the Directus table
#' 
#' @returns
#' A nested data frame (tibble) with results for each constraint. 
#' If the constraint does not pass, the problem_list column contains
#' a named list of row indices that break the constraint
#' e.g., list("column1" = c(1,2,3), "column2" = c(3,4,5))
#' 
#' The dtype_problem_list column contains a list of the data types for 
#' each columns that does not pass the constraint. 
#' e.g., list("column1" = "boolean", "column2" = "date")
#'    
#' 1) is_nullable
#' 2) max_length 
#' 3) is_unique
#' 4) data type.
#' @export
#' @import httr
#' @import glue
#' @import jsonlite
#' @import tibble
#' @examples
#' #not run: mydf <- read.csv("site_info_to_add.csv")
#' #not run: checkdf <-  check_table_contents("site_info", inputdf = mydf)
check_table_contents <- function(table_name = NULL,
                                 inputdf = NULL){
  pass_message <- "pass"
  fail_message <- "fail"
  na_message <- "constraint does not apply"
  # constraintvec <- c("non-nullable","max_length","unique","data_type")
  checkdf <- c()# empty data frame
  
  # template row for checkdf
  emptycheckrow <- tibble::tibble(constraint = NA,
                 pass = NA,
                 message = NA,
                 problem_cols = NA,
                 problem_list = NA,
                 dtype_problem_list = NA)
  # Fetch schema information for table
  table_info <- get_db_info(glue::glue("fields/{table_name}"),flatten=TRUE)
  
  #0)  Primary key------
  checkrow <- emptycheckrow
  checkrow$constraint <- "primary key"
  # the primary key must not contain any existing keys. 
  pkrow <- table_info[which(table_info$schema.is_primary_key),]
  pkfield <- pkrow$field
  auto_increment_pk <- pkrow$schema.has_auto_increment
  pk_in_inputdf <- pkfield %in% names(inputdf)
  pk_all_empty <- ifelse(pk_in_inputdf, all(is.na(inputdf[,pkfield])),FALSE)# test logic with NA
  pk_all_full <- ifelse(pk_in_inputdf, all(!is.na(inputdf[,pkfield])), FALSE)# test logic with NA
  
  ## Conditions applying only to non-autoincremented primary key.
  if(!auto_increment_pk){
    if(!pk_in_inputdf){
      checkrow$pass <- FALSE
      checkrow$message <- "Missing column for non-auto-incremented primary key."
      checkrow$problem_cols <- pkfield
    }
    if(pk_in_inputdf & any(is.na(inputdf[,pkfield]))){
      checkrow$pass <- FALSE
      checkrow$message <- "Missing values for non-auto-incremented primary key."
      checkrow$problem_cols <- pkfield
    }
  }
  ## conditions applying only to autoincremented primary key
  if(auto_increment_pk){
    ## passes if primary key column is absent or all empty
    if(!pk_in_inputdf | (pk_in_inputdf & pk_all_empty)){
      checkrow$pass <- TRUE
      checkrow$message <- pass_message
    }
    if(pk_in_inputdf & !pk_all_empty & !pk_all_full){
      checkrow$pass <- FALSE
      checkrow$message <- "Partially filled primary key column."
      checkrow$problem_cols <- pkfield
    }
  }
  ## conditions applying to both
  if(pk_in_inputdf & pk_all_full){
    ## pk options need to be checked.
    pkreq <- api_request("GET",glue::glue("items/{table_name}?fields={pkfield}&limit=-1"))
    pkreqdf <-  get_table_from_req(pkreq)
    pkoptions <- NULL
    if(length(pkreqdf) > 0){
      pkoptions <- pkreqdf[[1]]## subset so it's a vector instead of a df.  
    }
    pk_overlap <- any(inputdf[,pkfield] %in% pkoptions)
    pk_dups <- any(duplicated(inputdf[,pkfield]))
    if(pk_overlap & !pk_dups){
      checkrow$pass <- FALSE
      checkrow$message <- "Primary keys overlap with existing values."
      checkrow$problem_cols <- pkfield
      plist <- list(which(inputdf[,pkfield] %in% pkoptions))
      names(plist) <- pkfield
      checkrow$problem_list <- plist
    } 
    if(!pk_overlap & pk_dups){
      checkrow$pass <- FALSE
      checkrow$problem_cols <- pkfield
      checkrow$message <- "Primary keys are not unique."
    }
    
    if(pk_overlap & pk_dups){
      checkrow$pass <- FALSE
      checkrow$problem_cols <- pkfield
      checkrow$message <- "Primary keys are not unique and overlap with existing values."
    }
    if(!pk_overlap & !pk_dups){
      checkrow$pass <- TRUE
      checkrow$message <- pass_message
    }
  }
  
  # append to output tibble. 
  checkdf <- rbind(checkdf, checkrow)
  
  ## data validation constraints. 
  # start with easier ones. 

  # 1) is_nullable-----
  checkrow <- emptycheckrow
  checkrow$constraint <- "non_nullable"
  
  ## validation fails if any non-nullable columns contain null (NA) values
  ## this excludes fields with defaults provided (such as autoincremented)
  nonNullableCols <- table_info$field[which(!table_info$schema.is_nullable & 
                                              is.na(table_info$schema.default_value))]
  if(length(nonNullableCols) > 0){
    outlist <- c() # empty list for problem items
    cnames <- "" # empty string for column names
    ## If field is missing, it counts as null. 
    for(i in 1:length(nonNullableCols)){
      colname <- nonNullableCols[i]
      if(!colname %in% names(inputdf)){
        listitem <- list("missing non-nullable field")
        names(listitem) <- colname
        outlist <- append(outlist, listitem)
        mysep <- ifelse(cnames == "", "",";")
        cnames <- paste(cnames,colname, sep=mysep)
      }#closes if field is missing.
      else{ 
        failrows <- which(is.na(inputdf[,colname]))
        if(length(failrows) > 0){
          listitem <- list(failrows)
          names(listitem) <- colname
          outlist <- append(outlist, listitem)
          mysep <- ifelse(cnames == "", "",";")
          cnames <- paste(cnames,colname, sep=mysep)
        } #closes if
      }#closes else
    }# closes loop
    if(length(outlist) > 0){
      checkrow$pass <- FALSE
      checkrow$message <- fail_message
      checkrow$problem_list <- list(outlist)
      checkrow$problem_cols <- cnames
      
    }#closes if
    else{
      checkrow$pass <- TRUE
      checkrow$message <- pass_message
    }#closes else
    
  }else{
    checkrow$pass <- TRUE
    checkrow$message <- na_message
  }
  # append to output tibble. 
  checkdf <- rbind(checkdf, checkrow)
  
  # 2) max_length ----------
  checkrow <- emptycheckrow
  checkrow$constraint <- "max_length"
  maxLengthDf <- table_info[which(!is.na(table_info$schema.max_length)),c("field","schema.max_length")]
  ## it will always be 255, but that could change
  if(nrow(maxLengthDf) > 0){
    outlist <- c()
    cnames <- "" # empty string for column names
    for(i in 1:nrow(maxLengthDf)){
      colname <- maxLengthDf$field[i]
      # if column name isn't there, skip to the next one. 
      if(!colname %in% names(inputdf)){
        next
      }
      failrows <- nchar(inputdf[[colname]]) > maxLengthDf$schema.max_length[i]
      if(sum(failrows, na.rm=TRUE) > 0){
        listitem <- list(which(failrows==TRUE))
        names(listitem) <- colname
        outlist <- append(outlist, listitem)
        mysep <- ifelse(cnames == "", "",";")
        cnames <- paste(cnames,colname, sep=mysep)
      }
    }
    
    if(length(outlist) > 0){
      checkrow$pass <- FALSE
      checkrow$message <- fail_message
      checkrow$problem_list <- list(outlist)
      checkrow$problem_cols <- cnames
    }else{
      checkrow$pass <- TRUE
      checkrow$message <- pass_message
    }
  }else{
    checkrow$pass <- TRUE
    checkrow$message <- na_message
  }
  ## append to output
  checkdf <- rbind(checkdf, checkrow)
  
  # 3) is_unique--------
  checkrow <- emptycheckrow
  checkrow$constraint <- "unique"
  ## exclude primary key because that is checked above.
  uniqueCols <- table_info$field[which(table_info$schema.is_unique & 
                                         !table_info$schema.is_primary_key)]
  if(length(uniqueCols) > 0){
    outlist <- c()# empty list for problem items
    cnames <- "" # empty string for column names
    for(i in 1:length(uniqueCols)){
      colname <- uniqueCols[i]
      # skip if absent
      if(!colname %in% names(inputdf)){
        next
      }
      if(anyDuplicated(inputdf[[colname]])){
        dupvals <- inputdf[[colname]][which(duplicated(inputdf[[colname]]))]  
        failrows <- which(inputdf[[colname]] %in% dupvals)
        listitem <- list(failrows)
        names(listitem) <- colname
        outlist <- append(outlist, listitem)
        mysep <- ifelse(cnames == "", "",";")
        cnames <- paste(cnames,colname, sep=mysep)
        
      }
      
    } # closes loop
    if(length(outlist) > 0){
      checkrow$pass <- FALSE
      checkrow$message <- fail_message
      checkrow$problem_list <- list(outlist)
      checkrow$problem_cols <- cnames
    }else{
      checkrow$pass <- TRUE
      checkrow$message <- pass_message
    }
  }else{
    checkrow$pass <- TRUE
    checkrow$message <- na_message
  }
  # append
  checkdf <- rbind(checkdf, checkrow)
  
  # 4) data type.-----------
  checkrow <- emptycheckrow
  checkrow$constraint <- "data_type"
  outlist <- c()# empty list for problem items
  out_dlist <- c()# empty list for problem items for data type
  cnames <- "" # empty string for column names
  # This one is a little harder. It applies to all columns. 
  for(i in 1:nrow(table_info)){
    colname <- table_info$field[i]
    dtype <- table_info$type[i]
    ## skip to next if missing from inputdf
    if(!colname %in% names(inputdf) ){
      next
    }
    # unrecognized data type
    if(!dtype %in% c("text","string","integer","boolean","float","date")){
      listitem <- list("unrecognized data type")
      names(listitem) <- colname
      outlist <- append(outlist, listitem)
      listitem <- list(dtype)
      names(listitem) <- colname
      out_dlist <- append(out_dlist, listitem)
      mysep <- ifelse(cnames=="", "",";")
      cnames <- paste(cnames,colname, sep=mysep)
    }
    # test the data:
    colvec <- inputdf[[colname]]
    
    if(dtype == "integer"){
      # test for integer
      ## Note: I tested this and NAs are excluded. 
      failrows <- which(colvec != round(colvec, digits=0))
    }
    if(dtype %in% c("string","text")){
    # test for string and text
    # I don't think this needs a check. anything can be read as a string
    # anything can be string or text. string length constraint is above. 
      failrows <- NULL   
    }
    
    if(dtype == "boolean"){
      # test for boolean
      ## Note: directus recognizes A lot of alternatives as boolean. 
      # TRUE, FALSE, T, F with or without quotes. upper or lowercase. 0 or 1. 
      # Handling of NA's depend on null constraint. If nulls are allowed, NAs will 
      # be the default value. 
      # R recognizes TRUE, FALSE, T, F, and NA as logical. not 0 or 1. 
      # for our purposes, just use TRUE and FALSE
      failrows <- which(!as.character(colvec) %in% c("TRUE","FALSE", NA))
    }
    
    if(dtype == "float"){
      # test for float
      failrows <- which(!is.na(colvec) & is.na(as.numeric(colvec)))
    }
    if(dtype == "date"){
      # test for date
      failrows <- which(!is.ISOdate(colvec, sepstr="-"))
    }
    
    if(length(failrows) > 0){
      listitem <- list(failrows)
      names(listitem) <- colname
      outlist <- append(outlist, listitem)
      listitem <- list(dtype)
      names(listitem) <- colname
      out_dlist <- append(out_dlist, listitem)
      mysep <- ifelse(cnames=="", "",";")
      cnames <- paste(cnames,colname, sep=mysep)
    }  
      
  }# closes loop over columns
  if(length(outlist) > 0){
    checkrow$pass <- FALSE
    checkrow$message <- fail_message
    checkrow$problem_list <- list(outlist)
    checkrow$dtype_problem_list <- list(out_dlist)
    checkrow$problem_cols <- cnames
  }else{
    checkrow$pass <- TRUE
    checkrow$message <- pass_message
  }
  # append
  checkdf <- rbind(checkdf, checkrow)
  
  return(checkdf)
}

#' Check for mistakes in treatment components and years.
#' 
#' Harmonization steps require that:
#' 1) Expanding by start and end years in experimental_unit_treatments
#' results in a single row per treatment_id per unit_id per year (one for treatmentID1, one for treatmentID2)
#' 2) Expanding by start and end years in treatment_id_components results
#' in 1 level per treatment type and treatment id per year.
#' 3) When these expanded tables are combined, there is only 1 level per treatment type, unit id, treatment id, and year.
#' @param db 
#' A list containing named data frames treatment_id_info, treatment_id_components,
#' and experimental_unit_treatments. Because this function is intended to check for mistakes before
#' uploading to the database, this list must be supplied by the user. 
#' @returns
#' A list with subsetted problem rows of experimental_unit_treatments, treatment_id_components, and the
#' combined dataframe with treatment components by experimental unit. 
#' @export
#' @import dplyr
#'
#' @examples
#' # not run trt_check <- check_treatment_years(db)
check_treatment_years <- function(db = NULL){
  
  ## set up-------
  outlist <- vector(mode = "list",length = 3)
  names(outlist) <- c("experimental_unit_treatments","treatment_id_components","combined")
  treatment_id_components <- suppressMessages(dplyr::left_join(db$treatment_id_components, db$treatment_id_info))
  experimental_unit_treatments <- suppressMessages(dplyr::left_join(db$experimental_unit_treatments, db$treatment_id_info))
  
  suppressWarnings({
    ## 1: experimental_unit_treatments--------
    ## expand experimental_unit_treatments by year. 
    cat("\nCondition 1: Single row per unit_id, year, and treatment_id_type? ")
    ut <- expand_years(experimental_unit_treatments)
    utdups <- check_dups(ut,checkcols = c("unit_id","year","treatment_id_type"))
    if(utdups$ndups==0){
      cat("Yes!")
    }
    if(utdups$ndups > 0){
      cat("No. See output list.")
      outlist[[1]] <- experimental_unit_treatments[which(experimental_unit_treatments$uid %in% unique(utdups$dupdf$uid)),]
    }
    
    ## 2: treatment_id_components--------
    ## Get wide version of treatment components (yearly)
    # separate trt type 1 and 2
    cat("\nCondition 2: Single row per treatment_id, treatment_level, and year? ")
    tc <- expand_years(treatment_id_components)
    tcdups <- check_dups(tc, c("site_id","treatment_id","year","treatment_type"))
    if(tcdups$ndups == 0){
      cat("Yes!")
    }
    if(tcdups$ndups > 0){
      cat("No. See output list.")
      outlist[[2]] <- treatment_id_components[which(treatment_id_components$uid %in% unique(tcdups$dupdf$uid)),]
       
    }
    
    ## 3: combined experimental_unit_treatments and treatment_id_components-------
    cat("\nCondition 2: Single row per unit_id, treatment_level, and year? ")
    ## left join excludes treatments not assigned to a unit_id.
    combodf <- suppressMessages(
      dplyr::left_join(ut,tc,
                        by = c("site_id","treatment_id_type","treatment_id","year"),
                        suffix = c(".experimental_unit_treatment",".treatment_id_components")))
    combodups <- check_dups(combodf, checkcols = c("unit_id","year","treatment_type","treatment_id_type"))
    if(combodups$ndups ==0){
      cat("Yes!")
    }
    if(combodups$ndups > 0){
      cat("No. See output list.")
      outlist[[3]] <- combodups$dupdf
    }
  })#ends suppressWarnings.
  return(outlist)
}


#' Check foreign key values for a staged table.
#'
#' @param table_name 
#' Name of the table in the DRIVES database.
#' @param inputdf 
#' Data frame containing foreign key colulmns.
#' @param mytoken 
#' Directus token.
#' @param myurl
#' Directus base url.
#' @returns
#' A list of columns with foreign key constraints and 
#' a dataframe of subsetted rows from inputdf that violate
#' foreign key constraints--or NULL if all rows pass. 
#' The function throws out messages about the results of each foreign
#' key columns. 
#' 
#' @export
#'
#' @examples
#' # not run: fkcheck <- check_fk_values(
#' #"crop_variety_info", inputdf = staged_df)
check_fk_values <- function(table_name = NULL,
                            inputdf = NULL,
                            mytoken = getOption("drivesR.default.directustoken"),
                            myurl = getOption("drivesR.default.url")){
  # import schema information
  table_schema <- get_db_info(glue::glue("fields/{table_name}"),mytoken = mytoken, flatten = TRUE,
                              output_format = "data.frame" )
  fk_schema <- table_schema[which(!is.na(table_schema$schema.foreign_key_table)),]
  # make a list of foreign key options.
  fklist <- vector(mode = "list", length = nrow(fk_schema))
  names(fklist) <- fk_schema$field # named by field name in input table.
  for(i in 1:nrow(fk_schema)){
    fktable = fk_schema$schema.foreign_key_table[i]
    # skip internal foreign keys
    if(fktable == table_name) next()
    
    fkfield = fk_schema$schema.foreign_key_column[i]
    fkq <- jsonlite::toJSON(list("fields" = fkfield),auto_unbox = FALSE)
    
    fkreq <- httr::GET(
      glue::glue("{myurl}/items/{fktable}?fields={fkfield}&limit=-1"),
      httr::add_headers(
        "Authorization" = mytoken
      )
    )
    fkoptions <- get_table_from_req(fkreq)
    fklist[[i]] <- fkoptions[[1]]# removes from data frame
  }
  # check fk columns in inputdf against fklist.
  outlist <-vector(mode = "list", length = length(fklist))
  names(outlist) <- fk_schema$field
  for(i in 1:nrow(fk_schema)){
    fkfield <- fk_schema$field[i]
    
    ## skip if it's an internal fk
    if(is.null(fklist[[fkfield]])) next()
    fkTF <- inputdf[[fkfield]] %in% fklist[[fkfield]]
    ## if the field is nullable, set NAs to true. 
    if(fk_schema$schema.is_nullable[i] == TRUE &
       any(is.na(inputdf[[fkfield]]))){
      fkTF[which(is.na(inputdf[[fkfield]]))] <- TRUE
    }
    if(all(fkTF == TRUE)){
      message(glue::glue("No foreign key violations in column {fkfield}."))
    }else{
      message(glue::glue("Foreign key violations in column {fkfield}."))
      outlist[[i]] <- inputdf[which(fkTF==FALSE),]
    }
  }# closes loop
  return(outlist)
}# closes function

#' Order tables by foreign key dependencies
#' This is used for posting items to collections with 
#' foreign key constraints. This is to help determine
#' the order of items to post to avoid foreign key violations.
#' Tables with internal foreign keys should use order_rows_by_internal_fk
#' @param column_dictionary 
#' A subset of the column_dictionary describing tables to be ordered.
#' @returns
#' A data frame with table names in an order that is compatible with between-table
#' foreign key constraints, along with a TRUE/FALSE column indicating which tables
#' have internal foreign keys. 
#' @export
#' @import dplyr
#'
#' @examples
#' # not run:
#' # dict <- import_dictionary_tables()
#' # fkorderdf <- order_tables_by_fk(dict$column_dictionary)
#' 
order_tables_by_fk <- function(column_dictionary = NULL){
  if(is.null(column_dictionary)){
    stop("column dictionary must be supplied")  
  }
  
  fkdict <- column_dictionary %>% group_by(table_name) %>%
    summarize(num_fk_tables = length(unique(foreign_key_table[!is.na(foreign_key_table)])),
              has_internal_fk = sum(table_name == foreign_key_table,na.rm=TRUE)>0,
              fktables = list(unique(foreign_key_table[!is.na(foreign_key_table)])))
  #make a queue
  table_queue <- c()
  waiting_area <- fkdict$table_name
  
  while(length(waiting_area) > 0) {# repeat until all tables are removed from waiting area
    qTF <- c()
    for(i in seq_along(waiting_area)){
      mytable = waiting_area[i]  
      addToQ <- FALSE
      numfktabs = fkdict$num_fk_tables[which(fkdict$table_name==mytable)] 
      if(numfktabs== 0){ # if there are no foreign key tables, add straight to queue
        addToQ <- TRUE
      }
      if(numfktabs > 0){
        fktabs <- fkdict$fktables[which(fkdict$table_name==mytable)] %>% unlist()
        # if all FK tables are already queued, or self-referential, add to queue.
        if(all(fktabs %in% c(mytable,table_queue))){
          addToQ <- TRUE
        }else{
          addToQ <- FALSE
        }
      }#closes else
      qTF <- c(qTF, addToQ)
    } #closes for loop
    # update queue
    table_queue <- c(table_queue, waiting_area[qTF])
    # update waiting area
    waiting_area <- waiting_area[!qTF]
  }# closes while
  outdf <- data.frame(table_order = table_queue) %>%
    left_join(fkdict, by = c("table_order" = "table_name"))
  
  return(outdf)
}

#' Order rows based on an internal foreign key
#' For use in uploading rows to database tables. This
#' specifies a row order for uploading that avoids violating
#' internal foreign key constraints within a table. 
#' @param inputdf
#' A dataframe of rows to be added to a table with an 
#' internal foreign key constraint. 
#' @param idcol 
#' The primary key column name (e.g., crop_id).
#' @param fkcol 
#' The internal foreign key column name (e.g., parent_crop_id)
#'
#' @returns
#' A vector of row indices for the inputdf, indicating
#' the order they should be added in.
#' @export
#'
#' @examples
#' #not run: roworder <- order_rows_by_internal_fk(
#' #inputdf = crop_info,idcol = "crop_id",fkcol = "parent_crop_id")
#' # not run: walk(roworder,~post_rows("crop_info",inputdf[[.x]] ))
order_rows_by_internal_fk <- function(inputdf = NULL,
                                      idcol = NULL,
                                      fkcol = NULL){
  waitingrows <- 1:nrow(inputdf)
  rowqueue <- c()
  while(length(waitingrows) > 0){
    qTF <- c()
    for(i in waitingrows){
      # if internal fk value is empty, add to queue 
      addToQ <- FALSE
      if(is.na(inputdf[i,fkcol])){
        addToQ <- TRUE
      }
      # if fk column is present
      if(!is.na(inputdf[i,fkcol])){
        #add to queue if fk value is in queued rows as pk
        if(inputdf[i,fkcol] %in% inputdf[rowqueue,idcol]){
          addToQ <- TRUE
        }
      }
      qTF <- c(qTF, addToQ)
    }# closes for loop.
    #### update queue and waiting area
    rowqueue <- c(rowqueue,waitingrows[qTF])
    waitingrows <- waitingrows[!qTF]
  }# closes while
  return(rowqueue)
}
