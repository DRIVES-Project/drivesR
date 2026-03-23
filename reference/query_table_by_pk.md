# Fetch table contents by primary keys Function to query contents of a database table with a vector of primary key values. Useful for double-checking before running 'modify_rows' or 'delete_rows'. It could be useful for querying information across tables.

Fetch table contents by primary keys Function to query contents of a
database table with a vector of primary key values. Useful for
double-checking before running 'modify_rows' or 'delete_rows'. It could
be useful for querying information across tables.

## Usage

``` r
query_table_by_pk(table_name = NULL, pkvec = NULL, pkfield = "uid")
```

## Arguments

- table_name:

  The table identifier in the DRIVES database. If public = TRUE, the
  table name is automatically modified to query the public version of
  the database table.

- pkvec:

  A vector of primary key values.

- pkfield:

  The name of the column name that holds the table's primary key. For
  most tables, this is 'uid'.

## Value

A dataframe of the specified tables subsetted for rows matching the
primary key vector.

## Examples

``` r
#not run: qtable <- query_table_by_pk("harvest_dates",pkvec = 1:100, pkfield = "uid)
```
