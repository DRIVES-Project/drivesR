% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directus_utility_functions.R
\name{set_default_token}
\alias{set_default_token}
\title{Set default API token}
\usage{
set_default_token(usertoken, api = c("Directus", "Dataverse")[1])
}
\arguments{
\item{usertoken}{User-specific API token. For directus, this is formated as "Bearer {myAPI token"}.
For Dataverse, it's a string of letters and numbers.}

\item{api}{Sets which API to set the token for. Default is "Directus". "Dataverse" 
is also an option (for troubleshooting non-published tables in the Canadian
repository).}
}
\value{
Send
Resets default api token for relevant functions in drivesR.
}
\description{
Set a user-specific API token as the default for functions making API requests. 
Only works in R studio.
}
\examples{
directus_token = "Bearer abunchofnumbersandletters"
## This token should be read from a script or text file that is not synced to github

# see what defaults are before and after running
formals(api_request)$mytoken # shows name of option used to set default.
getOption("drivesR.default.directustoken") # shows default option.
# Not run: set_default_token(directus_token)
getOption("drivesR.default.directustoken") # shows new default

}
