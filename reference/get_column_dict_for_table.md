# Get column dictionary for a specified table As an alternative to subsetting.

Get column dictionary for a specified table As an alternative to
subsetting.

## Usage

``` r
get_column_dict_for_table(
  table_name = "site_info",
  myurl = getOption("drivesR.default.url")
)
```

## Arguments

- myurl:

  Base url for the Directus database.

## Value

A dataframe consisting of the subsetted column dictionary for the
corresponding table.

## Examples

``` r
#not run: cdict <- get_column_dict_for_table("site_info")
```
