# Check if dates are in ISO format.

Returns TRUE if a vector of character string dates is in a YYYY-MM-DD
format recognized by as.Date. as.Date recognizes - or / separators. It
Also works with single digit month or day.

## Usage

``` r
is.ISOdate(d, sepstr = NULL)
```

## Arguments

- d:

  a vector of dates in character format

- sepstr:

  Specified separator character (usually -). Useful for applications
  that don't recognize / separator.

## Value

a vector of TRUE and FALSE indicating whether each date is in ISO
format. NAs are returns as NA.

## Examples

``` r
myd = c("2024-01-01","2022/04/01","2023-15-01","6/23/22","24-01-01", NA,"jbs")
cbind(myd, is.ISOdate(myd), is.ISOdate(myd, sep="-"))
#>      myd                         
#> [1,] "2024-01-01" "TRUE"  "TRUE" 
#> [2,] "2022/04/01" "TRUE"  "FALSE"
#> [3,] "2023-15-01" "FALSE" "FALSE"
#> [4,] "6/23/22"    "FALSE" "FALSE"
#> [5,] "24-01-01"   "TRUE"  "FALSE"
#> [6,] NA           NA      NA     
#> [7,] "jbs"        "FALSE" "FALSE"
is.ISOdate(myd)
#> [1]  TRUE  TRUE FALSE FALSE  TRUE    NA FALSE
```
