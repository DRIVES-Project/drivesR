% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_type_functions.R
\name{is_integer}
\alias{is_integer}
\title{Check if a number is an integer}
\usage{
is_integer(x, non_numbers_as_na = TRUE)
}
\arguments{
\item{x}{A vector of one or more values to test.}

\item{non_numbers_as_na}{The vector is coerced to numeric before checking it is an integer. 
If this option is set to TRUE (default), non-numeric values in the vector are returned as NA. 
If it is set to FALSE, non-numeric values are returned as FALSE}
}
\description{
Check if a number is an integer
}
