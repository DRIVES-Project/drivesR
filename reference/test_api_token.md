# Verify Directus API token Used within other functions to check for errors.

Verify Directus API token Used within other functions to check for
errors.

## Usage

``` r
test_api_token(
  mytoken = getOption("drivesR.default.directustoken"),
  myurl = getOption("drivesR.default.url"),
  silent = TRUE
)
```

## Arguments

- mytoken:

  Directus API token, formatted as "Bearer APItoken."

- myurl:

  Directus database url. Set with defaults as
  https://data.drives-network.org

- silent:

  Indicates whether messages should be printed. Default FALSE.

## Value

TRUE or FALSE indicating whether mytoken produces a successful api
request.

## Examples

``` r
test_api_token(mytoken = "notavalidtoken", silent=FALSE)
#> Invalid API token
#> [1] FALSE
```
