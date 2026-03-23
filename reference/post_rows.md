# Add rows to a database table

This adds all rows in an input dataframe in a single API request. To add
more than 100 rows, use post_rows_in_batches. For use in an interactive
R session.

## Usage

``` r
post_rows(table_name = NULL, inputdf = NULL)
```

## Arguments

- table_name:

  The name of the database table.

- inputdf:

  The dataframe containing rows to be added. Contents should have been
  checked.

## Value

Converts the input df to a json and runs an API post request on the
specified table. The status code of that request is returned as a
message.

## Examples

``` r
# not run: post_rows("my_table", tabledf)
```
