
#' API dry run
#' 
#' Tests an API request without sending it. Useful for troubleshooting.
#' @param r 
#' An API request through the httr package. More convenient with piping.
#' @returns
#' A "request" object, containing a list of fields pertaining to the HTTP request. 
#' @export
#' @import httr
#' @examples
#' # testreq <- httr_dry_run(httr::GET(url = "https://data.drives-network.org/collections",
#' #               httr::add_headers("Authorization" = "Bearer myAPItoken")))
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
#' @param usertoken 
#' User-specific API token. For directus, this is formated as "Bearer {myAPI token"}.
#' For Dataverse, it's a string of letters and numbers.
#' @param api
#' Sets which API to set the token for. Default is "Directus". "Dataverse" 
#' is also an option (for troubleshooting non-published tables in the Canadian
#' repository).
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
set_default_token <- function(usertoken, api = c("Directus","Dataverse")[1]){
  if(api == "Directus"){
    options("drivesR.default.directustoken"= usertoken)
  }
  if(api == "Dataverse"){
    options("drivesR.default.dataversetoken"= usertoken)
  }
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


#' Create Directus Personal Access Token
#' 
#' This function generates a personal access token (PAT) for a user 
#' who has been registered on data.drives-network.org. The PAT is
#' an alternative to an API key, meant for short-term use within the API. 
#' There are two ways to generate a PAT. The first is to enter user credentials
#' (email and password) to generate a new API session (fetch_option = "regenerate").
#' The second is to extend an API session without re-entering credentials (fetch_option = "refresh"). 
#' The "regenerate" option is recommended for new users and users with a week or more of inactivity. 
#' The "refresh" option is recommended as a more secure option for users with a valid refresh token.
#' The PAT expires after about 15 minutes. The refresh token expires after seven days.
#' 
#' By default, this function saves and loads the PAT to and from an external file. 
#' This file should not be edited by the user. If you're using Git, 
#' be sure to add this file to your git ignore. 
#'
#' @param useremail
#' Email address registered with data.drives-network.org.
#' Not needed if fetch_option = "refresh".
#' @param userpassword 
#' Account password. Not needed if fetch_option = "refresh".
#' @param fetch_option
#' Options are "regenerate", "refresh", or "load". 
#' "regenerate" means that a new personal access token and refresh token are generated for the user.
#' This option should be used for first-time access, or 
#' "refresh" means that a new personal access token and refresh token are generated without needing to 
#' re-enter user credentials. Previous PATs and refresh tokens will not work after refreshing.  
#' 
#' @param refresh_token
#' An optional string input for the refresh token used to update the PAT. Only applies if
#' fetch_option = "refresh". If NULL, the refresh token is read from the file specified in savedir and savename.
#' Because the refresh token is reset after each use, we recommend reading from a file. 
#' @param save
#' If TRUE, the personal access token is saved to a file specified in savedir and savename.
#' @param output_type
#' If set to "auth" (default), the function returns a character string ready for 
#' Directus API calls ("Bearer insertaccesstoken"). 
#' If set to "list" the function returns a list of variables supplied by the Directus API 
#' (expiration time, refresh_token, access_token). The list output is included for troubleshooting
#' and in case there is some issue with saving data to files. 
#' @param savedir
#' Directory where the text file containing the personal access token should be saved. 
#' Defaults to the working directory.
#' @param savename
#' Name of the file containing the PAT. Default is directus_PAT.txt.
#' @param myurl 
#' Default url (data.drives-network.org)
#' @returns
#' Returns a text object "Bearer {newPAT}" that can be used as 
#' the input for set_default_token(). 
#' @export
#' @import httr
#'
#' @examples
#' # to begin a new API session:
#' #not run: mypat <- generate_directus_pat(useremail = "yourDirectusEmail", 
#' #                                          userpassword = "yourDirectusPassword",
#' #                                           fetch_option = "regenerate",
#' #                                             savedir = "mysupersafefolder", # default is working directory
#' #                                           savename = "directus_pat.txt")
#' #
#' #
#' # to extend an existing API session:
#' # not run: mypat <- generate_directus_pat(fetch_option = "refresh",
#' #                                            savedir = "mysupersafefolder", # dir and 
#' #                                           savename = "directus_pat.txt) # name must match initial run
#' # not run: set_default_token(mypat)
#' # not run: test_api_token(mypat) # to check if it works. 
 
generate_directus_pat <- function(useremail = NULL,
                                userpassword = NULL,
                                fetch_option = c("regenerate","refresh","load")[1],
                                refresh_token = NULL,
                                save = TRUE,
                                output_type = c("auth","list")[1],
                                savedir = ".",
                                savename = "directus_PAT.txt",
                                myurl = getOption("drivesR.default.url")){
  # check inputs.
  if(!output_type %in% c("auth","list")){
    stop("output_type must be 'auth' or 'list'.")
  }
  if(fetch_option=="load" |( fetch_option == "refresh" & is.null(refresh_token))){
    outputdf <- try(read.table(file = file.path(savedir, savename), header=TRUE,sep = "\t"),silent = TRUE)
    if(class(outputdf)=="try-error"){
      stop("File path ",file.path(savedir,savename)," not found. Make sure savedir and savename point to the correct file.")
    }
    if(class(outputdf) != "data.frame"){
      stop("Contents of ", file.path(savedir,savename), "are formatted incorrectly.
           Rerun with fetch_option = 'regenerate' and save = TRUE to create a new PAT file.")
    }
    if(!all(names(outputdf) %in% c("expires","refresh_token","access_token"))){
      stop("Contents of ", file.path(savedir,savename), "are formatted incorrectly.
           Rerun with fetch_option = 'regenerate' and save = TRUE to create a new PAT file.")
    }
  }# checks for valid PAT file content
          
   
  if(fetch_option == "refresh"){
    if(is.null(refresh_token)){
      refresh_token <- outputdf$refresh_token
      refreshjson = jsonlite::toJSON(list("refresh_token" = refresh_token), auto_unbox = TRUE)
      myreq <- POST(glue::glue("{myurl}/auth/refresh"),
                    body = refreshjson, content_type_json())
      if(myreq$status_code != 200){
        stop("Invalid refresh token.")
      }
      outputlist <- jsonlite::fromJSON(content(myreq,type = "text",encoding = "UTF-8"))[["data"]]
      outputdf <- as.data.frame(outputlist)
      if(save == TRUE){
        write.table(outputdf, file = file.path(savedir, savename),sep = "\t")
      }
    }
  }# if refresh
  
  if(fetch_option == "regenerate"){
    userlist <- list(email = useremail, password = userpassword)
    userjson <- jsonlite::toJSON(userlist, auto_unbox = TRUE, pretty = TRUE)
    myreq <- POST(glue::glue("{myurl}/auth/login"),
                body = userjson, content_type_json())
    outputlist <- jsonlite::fromJSON(content(myreq,type = "text",encoding = "UTF-8"))[["data"]]
    outputdf <- as.data.frame(outputlist)
    if(save == TRUE){
      write.table(outputdf, file = file.path(savedir, savename),sep = "\t")
    }
  } # if regenerate
  
  ## test if token is valid. throw warning message if not
  outtoken <- paste0("Bearer ",outputdf$access_token)
  validToken <- test_api_token(outtoken)
  if(validToken == FALSE){
    warning("Personal access token no longer valid. Refresh or regenerate using this function to obtain a valid token.")
  }
  if(output_type == "list"){
    return(as.list(outputdf))
  }
  if(output_type == "auth"){
    return(outtoken)
  }

}
