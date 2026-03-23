# Download content from an API fetch request into a data frame

This is useful for api_request() that involves a json_body object, such
as filtering for conditions in a table.

## Usage

``` r
get_table_from_req(apirequest = NULL)
```

## Arguments

- apirequest:

  An API request made with httr or api_request() with GET or SEARCH.

## Value

A data-frame with contents from the API request.

## Examples

``` r
# not run:
# this example demonstrates how a query filter might be set up 
# in an api request.
# querylist <- 
#   list("query"= list("filter" = 
#   list("table_name" =
#   list("_eq" = "site_info")),
#   "limit" = -1))
# queryjson <- toJSON(querylist, auto_unbox = TRUE)
# myreq <- api_request("SEARCH","items/column_dictionary", queryjson)
# outdf <- get_table_from_req(myreq)
```
