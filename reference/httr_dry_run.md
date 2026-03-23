# API dry run

Tests an API request without sending it. Useful for troubleshooting.

## Usage

``` r
httr_dry_run(r)
```

## Arguments

- r:

  An API request through the httr package. More convenient with piping.

## Value

A "request" object, containing a list of fields pertaining to the HTTP
request.

## Examples

``` r
# testreq <- httr_dry_run(httr::GET(url = "https://data.drives-network.org/collections",
#               httr::add_headers("Authorization" = "Bearer myAPItoken")))

```
