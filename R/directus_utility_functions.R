
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
#' Resets default api token for relevant functions in drivesR. 
#' @export
#' @examples
#' directus_token = "Bearer abunchofnumbersandletters"
#' ## This token should be read from a script or text file that is not synced to github
#' 
#' # see what defaults are before and after running
#' formals(api_request)$mytoken # shows name of option used to set default.
#' getOption("drivesR.default.directustoken") # shows default option.
#' # Not run: set_default_token(directus_token)
#' getOption("drivesR.default.directustoken") # shows new default
#' 
set_default_token <- function(usertoken){
  options("drivesR.default.directustoken"= usertoken)
}
