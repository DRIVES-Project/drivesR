# Map data types from Postgresql to Directus schema

Map data types from Postgresql to Directus schema

## Usage

``` r
pg_to_directus_type(pgtype)
```

## Arguments

- pgtype:

  A postgres data type as specified in the column dictionary

## Value

A character string with the corresponding datatype recognized by the
Directus API.

## Examples

``` r
pg_to_directus_type("array")
#> [1] "text"
```
