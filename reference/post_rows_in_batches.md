# Post rows in batches

To get around issues of posting limits.

## Usage

``` r
post_rows_in_batches(
  table_name = "crop_yields",
  inputdf = NULL,
  batchsize = 1000
)
```

## Arguments

- table_name:

  Table identifier in the database.

- inputdf:

  Data frame of rows to be added. This should have passed quality
  control checks.

- batchsize:

  Number of rows to be added at a time. Suggested amount is 1000 rows.
  Progress is printed to the console.

## Value

Silent.

## Examples

``` r
# not run: post_rows_in_batches("my_table", bigdf, 1000)
```
