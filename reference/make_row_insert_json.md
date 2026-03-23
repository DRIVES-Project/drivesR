# Convert a dataframe or tibble to a json object suitable to insert rows (items) Basically just the jsonlite::toJSON function with options set to non-defaults for conventience

Convert a dataframe or tibble to a json object suitable to insert rows
(items)

Basically just the jsonlite::toJSON function with options set to
non-defaults for conventience

## Usage

``` r
make_row_insert_json(mydf)
```

## Arguments

- mydf:

  A dataframe or tibble with quality control steps complete

## Value

A prettified json object

## Examples

``` r
testdf <- data.frame(x = 1:5, y = LETTERS[1:5])
insert_json <- make_row_insert_json(testdf)
# example with API request:
test_insert <- make_row_insert_json(test_cat_info)
# insert_req <- api_request("POST","items/test_cat_info",test_insert)
```
