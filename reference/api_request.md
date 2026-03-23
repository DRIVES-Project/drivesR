# Send a request to the Directus API https://docs.directus.io/ https://directus.io/docs/api

Send a request to the Directus API https://docs.directus.io/
https://directus.io/docs/api

## Usage

``` r
api_request(
  myverb = "POST",
  mytarget = "collections",
  jsonbody = NULL,
  myurl = getOption("drivesR.default.url"),
  mytoken = getOption("drivesR.default.directustoken")
)
```

## Arguments

- myverb:

  Verb in the http::VERB function. Typically POST, PATCH, SEARCH, GET,
  or DELETE

- mytarget:

  The part of the URL string pointing to the content you want to read or
  modify.

- jsonbody:

  A JSON object containing the content of the request, if applicable.
  For POST and PATCH, this is content you want to modify. For SEARCH,
  the content will be some kind of filter query. This argument will
  typically be NULL for DELTE and GET requests.

- myurl:

  Root URL for the DRIVES database. Set as a default when the package
  loads ("https://data.drives-network.org/)

- mytoken:

  API token for the user. This is formatted as "Bearer APItoken"
  (without the curly brackets). It is recommended that the user make a
  script that loads the API token and database URL.

## Value

POST, PATCH, and DELETE requests will modify content on Directus and
return a status message (https://directus.io/docs/guides/connect/errors)
Usually, 200 means the request was successful and anything else means
the request failed. These kinds of requests can be saved to an R object
or run as interactive code. Both methods

Requests that retrieve content from the database (GET, SEARCH) must be
passed on to httr::content and downstream functions used to organize
content.

## See also

\[\$1:make_collection_json()\]

## Examples

``` r
# Accessing database content (here, it is information about collections)
# not run: collectionsreq <- api_request("GET", mytarget = "collections")
# not run: collectionjson <- jsonlite::toJSON(httr::content(collectionsreq),
# pretty=TRUE, auto_unbox = TRUE) 

# Modifying database content (here it is adding a new collection)
#not run: testcollection <- make_collection_json()
# default with sample data dictionary 
# not run: new_collection_req <- api_request("POST", "collections",
# testcollection)
```
