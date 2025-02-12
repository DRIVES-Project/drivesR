

#' Delete rows in a table based on their primary key value
#'
#' @param table_name 
#' The table identifier in directus
#' @param pkvec 
#' A vector of primary keys for rows you want to delete in the table.
#' @param check_only
#' TRUE or FALSE indicating whether you want to inspect the rows instead of deleting them.
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
                        check_only = FALSE,
                        mytoken = NULL,
                        myurl = "https://data.drives-network.org"){
  if(check_only == TRUE){
    checkdf <- c()
    for(pk in pkvec){
      testrow <- get_db_info(glue::glue("items/{table_name}/{pk}"),
                             output_format = "data.frame",
                             mytoken = mytoken,
                             myurl = myurl)
      checkdf <- rbind(checkdf, testrow)
    }# closes for loop
    return(checkdf)
  }# closes if
  
  if(check_only == FALSE){
    for(pk in pkvec){
      api_request("DELETE",glue::glue("items/{table_name}/{pk}"),
                  mytoken = mytoken,
                  myurl = myurl)
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
#' @param mytoken 
#' Directus token.
#' @param myurl 
#' Directus URL.
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
                      idcol = "uid",
                      mytoken = NULL,
                      myurl = "https://data.drives-network.org"){
  problemrows <- c()
  for(i in 1:nrow(editdf)){
    mypk <- editdf[i,idcol]
    changecols <- names(editdf)[which(!names(editdf) %in% idcol)]
    fixlist <- as.list(editdf[i,changecols]) # exclude primary key, since it can't be patched      
    #as list makes it so there are no square brackets around.
    if(is.null(names(fixlist))){
      names(fixlist) <- changecols  
      ## if there is only one column to be changed, as.list gets rid of the name.
    }
    
    fixjson <-  jsonlite::toJSON(fixlist,pretty=T, auto_unbox = TRUE)
    fixreq <- api_request("PATCH",
                          glue::glue("items/{table_name}/{mypk}"),
                          fixjson,
                          myurl = myurl,
                          mytoken = mytoken)
    if(fixreq$status_code != 200){
      addrow <- data.frame(pk = mypk, status_code = fixreq$status_code)
      problemrows <- rbind(problemrows, addrow)
    }# closes if
  }# closes loop
  if(length(problemrows) > 0){
    names(problemrows)[1] <- idcol
    return(problemrows)
  }
  
}
