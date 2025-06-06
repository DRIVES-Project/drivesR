% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directus_utility_functions.R
\name{httr_dry_run}
\alias{httr_dry_run}
\title{API dry run}
\usage{
httr_dry_run(r)
}
\arguments{
\item{r}{An API request through the httr package. More convenient with piping.}
}
\description{
Tests an API request without sending it. Useful for troubleshooting.
}
\examples{
testreq <- httr_dry_run(httr::GET(url = "https://data.drives-network.org/collections",
              httr::add_headers("Authorization" = "Bearer {myAPItoken}")))


}
