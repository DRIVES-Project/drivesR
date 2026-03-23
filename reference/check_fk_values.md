# Check foreign key values for a staged table.

Check foreign key values for a staged table.

## Usage

``` r
check_fk_values(
  table_name = NULL,
  inputdf = NULL,
  mytoken = getOption("drivesR.default.directustoken"),
  myurl = getOption("drivesR.default.url")
)
```

## Arguments

- table_name:

  Name of the table in the DRIVES database.

- inputdf:

  Data frame containing foreign key colulmns.

- mytoken:

  Directus token.

- myurl:

  Directus base url.

## Value

A list of columns with foreign key constraints and a dataframe of
subsetted rows from inputdf that violate foreign key constraints–or NULL
if all rows pass. The function throws out messages about the results of
each foreign key columns.

## Examples

``` r
# not run: fkcheck <- check_fk_values(
#"crop_variety_info", inputdf = staged_df)
```
