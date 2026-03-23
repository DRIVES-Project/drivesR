# Sample column dictionary

A data frame with database schema information representative of DRIVES
tables.

## Usage

``` r
test_column_dict
```

## Format

A data frame with 8 rows and 14 variables:

- column_id:

  Unique column identifier

- table_name:

  Table identifier

- column_name:

  Name of column within the table

- column_order:

  Integer

- description:

  Text

- postgres_data_type:

  Text category

- primary_key:

  TRUE or FALSE

- record_identifier:

  TRUE or FALSE

- foreign_key_table:

  Table name or NA

- foreign_key_column:

  Column name or NA

- nullable:

  TRUE or FALSE

- unique_value:

  TRUE or FALSE

- is_category:

  TRUE or FALSE

- auto_increment:

  TRUE or FALSE
