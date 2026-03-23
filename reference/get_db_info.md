# Fetch schema information from directus

Fetch schema information from directus

## Usage

``` r
get_db_info(
  mytarget = "collections",
  output_format = c("data.frame", "json")[1],
  myurl = getOption("drivesR.default.url"),
  mytoken = getOption("drivesR.default.directustoken"),
  flatten = FALSE
)
```

## Arguments

- mytarget:

  The part of the URL indicating the part of the database you want
  schema information about. "collections" returns information about all
  tables. "collections/table_name" returns metadata on a specific
  collection.

  For information about columns within a table, use "fields/table_name".
  For information about relations in a table, use
  "relations/table_name/column_name

- output_format:

  By default, the output is formatted as a data frame. There is also the
  option to format as a prettified json string.

- myurl:

  Base URL for the drives database.

- mytoken:

  User-specific Directus API token, in the format "Bearer myAPItoken"
  without curly brackets.

- flatten:

  If FALSE (default), the data dataframe will be nested. If TRUE, it
  will be flattened. Irrelevant if output is json.

## Value

A dataframe (default) or json string with schema and other metadata
information pulled from directus

## Examples

``` r
# collection_info <- get_db_info("collections")
# site_field_info <- get_db_info("fields/site_info")
# foreign_key_info <- get_db_info("relations")
```
