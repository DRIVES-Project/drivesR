

#' Delete rows in a table based on their primary key value
#'
#' @param table_name 
#' The table identifier in directus
#' @param pkvec 
#' A vector of primary keys for rows you want to delete in the table.
#' @param check_only
#' TRUE or FALSE indicating whether you want to inspect the rows instead of deleting them.
#' The default is TRUE as a protective measure.
#' @param pkfield
#' Name of the primary key column. The default is "uid."
#' @param mytoken
#' Directus api token, formatted as "Bearer myapitoken"
#' @returns
#' If check_only = TRUE, returns a data frame of the table subset corresponding to the primary 
#' keys in pkvec. if check_only = FALSE, the function performs delete operations on primary keys
#' within the specified table.
#' @export
#' @import httr
#' @importFrom glue glue
#' @examples
#' # not run: fakerowjson <- make_insert_json(data.frame("site_id" = c("fake_id1","fake_id2")))
#' # not run: api_request("POST","items/site_info", fakerowjson)
#' # not run: testrun <- delete_rows("site_info",c("fake_id1","fake_id2"), check_only = TRUE)
#' # not run: delete_rows("site_info",c("fake_id1","fake_id2"), check_only = FALSE)

delete_rows <- function(table_name = NULL, 
                        pkvec = NULL,
                        check_only = TRUE,
                        pkfield = "uid",
                        mytoken = getOption("drivesR.default.directustoken")){
  if(check_only == TRUE){
    # query for primary key.
    checkdf <- query_table_by_pk(table_name = table_name,
                                 pkvec = pkvec,
                                 pkfield = pkfield,
                                 mytoken = mytoken)
      ## with only one row, empty values are coded as null.
      # they need to be converted to NAs.
      # I looked for more elegant solutions, but couldn't find anything.
    return(checkdf)
  }# closes if
  
  if(check_only == FALSE){
    for(pk in pkvec){
      api_request("DELETE",glue::glue("items/{table_name}/{pk}"),mytoken = mytoken)
    }# closes for loop
  }# closes if 
} #closes function


#' Modify content in existing rows
#' 
#' For a large number of rows, it may be a good idea to run content changes through 
#' QC functions before performing this step. 
#' 
#' @param table_name
#' Table identifier within Directus 
#' @param editdf 
#' A data frame with columns for the primary key and whatever is to be modified.
#' @param idcol 
#' Name of the primary key column in editdf.
#' @param batchsize 
#' Number of rows to complete per batch. Recommended to cap at 1000.
#' @returns
#' Loops through rows of edit df and performs a PATCH request on each corresponding 
#' item in the database. if the patch request does not work, it returns ids for problem rows.
#' @export
#'
#' @examples
#' testdf1 <- data.frame("id"=c(6,7,8), "cat_name"= c("Thing1","Thing2","Thing3"),"cat_age" = c(1,2,"spam"))
#' # Not run: testpatch <- modify_rows(table_name = "test_cat_info", editdf = testdf1, idcol = "id")
#' # print(testpatch) # id 8 will have a status_code 500 error due to non-integer in cat_age
#' testdf2 <- data.frame("id"=c(6,7,8), "cat_name"= c("Thing1","Thing2","Thing3"),"cat_age" = c(1,2,3))
#' # Not run: testpatch <- modify_rows(table_name = "test_cat_info", editdf = testdf2, idcol = "id")
#' # print(testpatch) # NULL with no errors.
#' 
modify_rows <- function(table_name = NULL, 
                      editdf = NULL,
                      batchsize = 1000,
                      idcol = "uid",
                      mytoken = getOption("drivesR.default.directustoken")){
  nitems = nrow(editdf)
  nbatches = ceiling(nitems/batchsize)
  start_i = 1
  end_i = 0
  batch_i = 1
  problemrows <- c()
  for(i in 1:nbatches){
    end_i <- min(start_i + batchsize -1 , nitems)
    subsetdf <- editdf[start_i:end_i,]
    fixjson <- make_row_insert_json(subsetdf)  
    fixreq <- api_request("PATCH",
                          glue::glue("items/{table_name}"),
                          fixjson,mytoken = mytoken)
    if(fixreq$status_code != 200){
      addrows <- data.frame(pk = subsetdf[,idcol], status_code = fixreq$status_code)
      problemrows <- rbind(problemrows, addrows)
    }# closes if
    message(paste("Batch",batch_i,"of",nbatches,"status",fixreq$status_code))
    #cat(paste0("\nstart_i = ",start_i,", end_i = ",end_i,", batch_i = ",batch_i))
    start_i <- end_i + 1
    batch_i <- batch_i + 1
  }# closes loop
  if(length(problemrows) > 0){
    names(problemrows)[1] <- idcol
    return(problemrows)
    np = nrow(problemrows)
    message(glue::glue("Problems found in {np} rows."))
  }else{
    message("No problems.")
  }
}
