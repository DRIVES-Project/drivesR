# Modify content in existing rows

For a large number of rows, it may be a good idea to run content changes
through QC functions before performing this step.

## Usage

``` r
modify_rows(table_name = NULL, editdf = NULL, batchsize = 1000, idcol = "uid")
```

## Arguments

- table_name:

  Table identifier within Directus

- editdf:

  A data frame with columns for the primary key and whatever is to be
  modified.

- batchsize:

  Number of rows to complete per batch. Recommended to cap at 1000.

- idcol:

  Name of the primary key column in editdf.

## Value

Loops through rows of edit df and performs a PATCH request on each
corresponding item in the database. if the patch request does not work,
it returns ids for problem rows.

## Examples

``` r
testdf1 <- data.frame(
"id"=c(6,7,8),
"cat_name"= c("Thing1","Thing2","Thing3"),
 "cat_age" = c(1,2,"spam"))
# Not run: testpatch <- modify_rows(
#table_name = "test_cat_info", 
#editdf = testdf1, idcol = "id")
# print(testpatch) 
# id 8 will have a status_code 500 
#error due to non-integer in cat_age
testdf2 <- data.frame(
"id"=c(6,7,8), 
"cat_name"= c("Thing1","Thing2","Thing3"),
"cat_age" = c(1,2,3))
# Not run: testpatch <- 
#modify_rows(table_name = "test_cat_info", editdf = testdf2, idcol = "id")
# print(testpatch) # NULL with no errors.
```
