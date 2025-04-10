
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

#' Set default public access settings
#' This can be used to change the default public access settings
#' from what is set up during package loading. 
#' For the publicly-released package, the default will be
#' public access.
#' 
#' @param public 
#' TRUE for public access, FALSE for private access.
#' @returns
#' Changes the option for "drivesR.default.directustoken"
#' that is used by various functions.  
#' @export
#'
#' @examples
set_public_access<- function(public = TRUE){
  if(!public %in% c(TRUE,FALSE) | length(public) != 1){
    stop("function argument 'public' must be TRUE or FALSE")
  }
  options("drivesR.default.public" = public)
}

#' Verify Directus API token
#' Used within other functions to check for errors.
#' @param mytoken 
#' Directus API token, formatted as "Bearer APItoken."
#' @param myurl
#' Directus database url. Set with defaults as https://data.drives-network.org
#' @param silent
#' Indicates whether messages should be printed. Default FALSE.
#' @returns
#' TRUE or FALSE indicating whether mytoken  
#' produces a successful api request. 
#' @export
#' @import httr
#' @import glue
#' @examples
#' test_api_token(mytoken = "notavalidtoken", silent=FALSE)
test_api_token <- function(mytoken = getOption("drivesR.default.directustoken"),
                           myurl = getOption("drivesR.default.url"),
                           silent = TRUE){
  testreq <- httr::GET(glue::glue("{myurl}/collections"),
                       httr::add_headers(
                         "Authorization" = mytoken
                       )
  )# ends GET
  validCode <-testreq$status_code == 200
  outmessage <- ifelse(validCode, "Valid API token","Invalid API token")
  if(silent != TRUE){
    message(outmessage)
  }
  return(validCode)
}
