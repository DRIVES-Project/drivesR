# Order tables by foreign key dependencies This is used for posting items to collections with foreign key constraints. This is to help determine the order of items to post to avoid foreign key violations. Tables with internal foreign keys should use order_rows_by_internal_fk

Order tables by foreign key dependencies This is used for posting items
to collections with foreign key constraints. This is to help determine
the order of items to post to avoid foreign key violations. Tables with
internal foreign keys should use order_rows_by_internal_fk

## Usage

``` r
order_tables_by_fk(column_dictionary = NULL)
```

## Arguments

- column_dictionary:

  A subset of the column_dictionary describing tables to be ordered.

## Value

A data frame with table names in an order that is compatible with
between-table foreign key constraints, along with a TRUE/FALSE column
indicating which tables have internal foreign keys.

## Examples

``` r
# not run:
# dict <- import_dictionary_tables()
# fkorderdf <- order_tables_by_fk(dict$column_dictionary)
```
