# Delete rows in a table based on their primary key value

Delete rows in a table based on their primary key value

## Usage

``` r
delete_rows(
  table_name = NULL,
  pkvec = NULL,
  check_only = TRUE,
  pkfield = "uid"
)
```

## Arguments

- table_name:

  The table identifier in directus

- pkvec:

  A vector of primary keys for rows you want to delete in the table.

- check_only:

  TRUE or FALSE indicating whether you want to inspect the rows instead
  of deleting them. The default is TRUE as a protective measure.

- pkfield:

  Name of the primary key column. The default is "uid."

## Value

If check_only = TRUE, returns a data frame of the table subset
corresponding to the primary keys in pkvec. if check_only = FALSE, the
function performs delete operations on primary keys within the specified
table.

## Examples

``` r
# not run: fakerowjson <- make_insert_json(data.frame("site_id" = c("fake_id1","fake_id2")))
# not run: api_request("POST","items/site_info", fakerowjson)
# not run: testrun <- delete_rows("site_info",c("fake_id1","fake_id2"), check_only = TRUE)
# not run: delete_rows("site_info",c("fake_id1","fake_id2"), check_only = FALSE)
```
