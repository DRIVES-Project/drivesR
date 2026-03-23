# Expand data frame by start and end years

Converts a table with time ranges specified as start- and end-year
column into a longer table with separate rows for each year.

## Usage

``` r
expand_years(
  mydf = NULL,
  na_end_year = 2021,
  sycol = "start_year",
  eycol = "end_year",
  ycol = "year"
)
```

## Arguments

- mydf:

  A data frame containing start- and end-year columns

- na_end_year:

  A year to use when end-year is NA. Start years must not have any NAs.

- sycol:

  Name of the start-year column. Default is start_year.

- eycol:

  Name of the end-year column. Default is end_year.

- ycol:

  Name of the new column for individual years. Default is year.

## Value

A data-frame with separate rows for each year between start and end
year. Start and end year columns are removed.

## Examples

``` r
#Not run: exp_unit_trt <- get_db_table("experimental_units_treatments") 
#Not run: exp_trt_yearly <- expand_years(exp_unit_trt)
```
