# Check column names for a staged table.

Quality control step to check that column names in a data frame to be
imported into Directus match what is in the table schema.

## Usage

``` r
check_column_names(
  table_name = "site_info",
  inputdf = NULL,
  mytoken = getOption("drivesR.default.directustoken")
)
```

## Arguments

- table_name:

  Table identifier, such as "site_info"

- inputdf:

  Dataframe to be evaluated.

- mytoken:

  directus API token, as "Bearer apitoken"

## Value

A dataframe of checks, outcomes, and relevant fields.

## Examples

``` r
#Not run: check_column_names("site_info", mydf)
```
