# Fetch an entire table from Directus

Fetch an entire table from Directus

## Usage

``` r
get_db_table(
  table_name = "site_info",
  myurl = getOption("drivesR.default.url"),
  mytoken = getOption("drivesR.default.directustoken")
)
```

## Arguments

- table_name:

  The table identifier within Directus

- myurl:

  The database url. By default, "https://data.drives-network.org"

- mytoken:

  The user-specific API token, in the format "Bearer insertAPItoken",
  without curly brackets. This can be set with set_default_token.

## Value

A data frame containing all rows and columns of the specified table. If
public is set to TRUE, the data will exclude sites and years not
approved for public access. Also, data from Canadian sites will be
fetched from a Canadian-hosted repository (Borealis).

## Examples

``` r
#not run: testdf <- get_db_table("site_info") 
```
