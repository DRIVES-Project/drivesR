
#' API dry run
#' 
#' Tests an API request without sending it. Useful for troubleshooting.
#' @param r 
#' An API request through the httr package. More convenient with piping.
#' @returns
#' 
#' @export
#' @import httr
#' @examples
#' testreq <- httr_dry_run(httr::GET(url = "https://data.drives-network.org/collections",
#'               httr::add_headers("Authorization" = "Bearer {myAPItoken}")))
#' 
#'
httr_dry_run <- function(r) {
  old_cb <- httr::get_callback("request")
  on.exit(httr::set_callback("request", old_cb))
  
  httr::set_callback("request", function(req) req)
  r
}

#' Set default API token
#' 
#' Set a user-specific API token as the default for functions making API requests. 
#' Only works in R studio.
#' 
#' @param mytoken 
#' User-specific API token formated as "Bearer {myAPI token"}
#' @returns
#' Send
#' Resets default api token for relevant functions in drivesR by sending commands to the R console.
#' For now, it just specifies relevant functions in separate lines of code. 
#' There's probably a fancier way to do this with lists or whatever.
#' @export
#' @import glue
#' @importFrom rstudioapi sendToConsole
#' @examples
#' directus_token = "Bearer abunchofnumbersandletters"
#' ## This token should be read from a script or text file that is not synced to github
#' 
#' # see what defaults are before and after running
#' formals(api_request)$mytoken
#' # Not run: set_default_token(directus_token)
#' formals(api_request)$mytoken
#' 
set_default_token <- function(usertoken){
  apifuns <- c("api_request",
               "get_db_table",
               "get_db_info",
               "check_dictionary",
               "check_categories",
               "check_column_names",
               "check_table_contents",
               "post_rows_in_batches",
               "delete_rows",
               "modify_rows")
  for(af in apifuns){
    mycode = glue::glue("formals({af})$mytoken <- '{usertoken}'")
    rstudioapi::sendToConsole(mycode)  
  }
  
}
