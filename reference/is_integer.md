# Check if a number is an integer

Check if a number is an integer

## Usage

``` r
is_integer(x, non_numbers_as_na = TRUE)
```

## Arguments

- x:

  A vector of one or more values to test.

- non_numbers_as_na:

  The vector is coerced to numeric before checking it is an integer. If
  this option is set to TRUE (default), non-numeric values in the vector
  are returned as NA. If it is set to FALSE, non-numeric values are
  returned as FALSE

## Value

A logical vector the same length as X with TRUE for integers, FALSE for
non-numeric integers, and either FALSE or NA for non-numeric values.

## Examples

``` r
x1 <- c(1,2.1,"a")
is_integer(x1, non_numbers_as_na = TRUE)
#> Warning: NAs introduced by coercion
#> [1]  TRUE FALSE    NA
is_integer(x1, non_numbers_as_na = FALSE)
#> Warning: NAs introduced by coercion
#> [1]  TRUE FALSE FALSE
```
