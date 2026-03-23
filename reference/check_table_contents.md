# Check table contents before import

This function checks contents of a table (usually imported as a CSV) for
possible failures in validation constraints. The constraints checked
are: 1) is_nullable 2) max_length (if applicable) 3) is_unique 4) data
type. Column names and foreign key values are checked with different
functions.

## Usage

``` r
check_table_contents(table_name = NULL, inputdf = NULL)
```

## Arguments

- table_name:

  Name of the table in Directus

- inputdf:

  data frame intended for the Directus table

## Value

A nested data frame (tibble) with results for each constraint. If the
constraint does not pass, the problem_list column contains a named list
of row indices that break the constraint e.g., list("column1" =
c(1,2,3), "column2" = c(3,4,5))

The dtype_problem_list column contains a list of the data types for each
columns that does not pass the constraint. e.g., list("column1" =
"boolean", "column2" = "date")

1\) is_nullable 2) max_length 3) is_unique 4) data type.

## Examples

``` r
#not run: mydf <- read.csv("site_info_to_add.csv")
#not run: checkdf <-  check_table_contents("site_info", inputdf = mydf)
```
