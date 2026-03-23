# Make a collection schema json object from data dictionary tables

Make a collection schema json object from data dictionary tables

## Usage

``` r
make_collection_json(
  columndf = test_column_dict[which(test_column_dict$table_name == "test_cat_info"), ],
  tablerow = test_table_dict[1, ]
)
```

## Arguments

- columndf:

  A dataframe of schema information about each column (field) from the
  column dictionary.

- tablerow:

  A one-row dataframe of schema information about the table (collection)
  from the table dictionary.

## Value

A character string for the corresponding data type recognized by the
Directus API.

## Examples

``` r
my_collection_json <- 
make_collection_json(columndf = 
                   test_column_dict[which(test_column_dict$table_name==
                                            "test_cat_info"),],
                                            test_table_dict[1,])

```
