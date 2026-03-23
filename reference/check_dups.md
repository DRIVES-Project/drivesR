# Subset a dataframe for rows that are duplicated across a set of columns.

Subset a dataframe for rows that are duplicated across a set of columns.

## Usage

``` r
check_dups(mydf, checkcols = NULL, dupvec = NULL, dupTFmatchesInput = TRUE)
```

## Arguments

- mydf:

  A dataframe.

- checkcols:

  A vector of column names that you want to check for duplicates. Will
  also work with column indices. Alternative to dupvec.

- dupvec:

  A vector to use to check for duplicate values. Length(dupvec) =
  nrow(mydf). Alternative to column names.

- dupTFmatchesInput:

  If TRUE, the TRUE/FALSE vector indicating duplicated values
  corresponds to the input dataframe. If FALSE, it corresponds to the
  subsetted dataframe containing duplicate values. values

## Value

Returns a list containing the number of unique column combinations that
have duplicate values (ndups), a subsetted dataframe containing all
duplicated values, and a TRUE FALSE vector to facilitate removal of
duplicate rows (created by duplicated). This vector can correspond to
the input dataframe (mydf) or the output dataframe (dupdf), depending on
the choice for dupTFmatchesInput.

## Examples

``` r
testdf <- data.frame("col1" = c("spam","eggs","spam","spam"),
                     "col2" = c(1,2,1,1),
                     "col3" = c("cat","fish","dog","cat"),
                     "col4" = c(1,2,3,4))
check_dups(testdf, checkcols = c("col1","col2"))
#> $ndups
#> [1] 1
#> 
#> $dupdf
#>   col1 col2 col3 col4
#> 1 spam    1  cat    1
#> 3 spam    1  dog    3
#> 4 spam    1  cat    4
#> 
#> $dupTF
#> [1] FALSE FALSE  TRUE  TRUE
#> 
check_dups(testdf, checkcols = c(1,2,3))
#> $ndups
#> [1] 1
#> 
#> $dupdf
#>   col1 col2 col3 col4
#> 1 spam    1  cat    1
#> 4 spam    1  cat    4
#> 
#> $dupTF
#> [1] FALSE FALSE FALSE  TRUE
#> 
```
