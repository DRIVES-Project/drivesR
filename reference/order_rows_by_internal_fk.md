# Order rows based on an internal foreign key For use in uploading rows to database tables. This specifies a row order for uploading that avoids violating internal foreign key constraints within a table.

Order rows based on an internal foreign key For use in uploading rows to
database tables. This specifies a row order for uploading that avoids
violating internal foreign key constraints within a table.

## Usage

``` r
order_rows_by_internal_fk(inputdf = NULL, idcol = NULL, fkcol = NULL)
```

## Arguments

- inputdf:

  A dataframe of rows to be added to a table with an internal foreign
  key constraint.

- idcol:

  The primary key column name (e.g., crop_id).

- fkcol:

  The internal foreign key column name (e.g., parent_crop_id)

## Value

A vector of row indices for the inputdf, indicating the order they
should be added in.

## Examples

``` r
#not run: roworder <- order_rows_by_internal_fk(
#inputdf = crop_info,idcol = "crop_id",fkcol = "parent_crop_id")
# not run: walk(roworder,~post_rows("crop_info",inputdf[[.x]] ))
```
