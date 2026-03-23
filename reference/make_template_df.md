# Make a template data frame from a vector of column names

Make a template data frame from a vector of column names

## Usage

``` r
make_template_df(cnames = NULL, numrows = 1)
```

## Arguments

- cnames:

  A vector of column names

- numrows:

  Number of rows the template should have.

## Value

An empty data frame with specified column names and number of rows.

## Examples

``` r
cnames <- c("a","b","c")
templatedf <- make_template_df(cnames, 5)
print(templatedf) 
#>    a  b  c
#> 1 NA NA NA
#> 2 NA NA NA
#> 3 NA NA NA
#> 4 NA NA NA
#> 5 NA NA NA
```
