# Convert column dictionary content into json-formatted field schema. This is for adding fields to existing collections. Once a collection exists, only one field can be added at a time with an API request.

Convert column dictionary content into json-formatted field schema. This
is for adding fields to existing collections. Once a collection exists,
only one field can be added at a time with an API request.

## Usage

``` r
make_field_json(column_dictionary_row = NULL)
```

## Arguments

- column_dictionary_row:

  A row from the column dictionary, filled out with information for the
  new field.

## Value

A json-formatted object containing schema information for a new field in
an existing collection.

## Examples

``` r
column_dictionary <- test_column_dict
## model the new field on an existing field (for demo purposes)
newrow <- test_column_dict[which(test_column_dict$column_name == "cat_name"),]
newrow$column_id <- "test_cat_info:favorite_food"
newrow$column_name <- "favorite_food"
newrowjson <- make_field_json(newrow)
# not run: newfieldreq <- api_request("POST",mytarget ="fields/test_cat_info",jsonbody= newrowjson)
```
