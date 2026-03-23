# List treatment components by management practice.

Uses information in the site_treatment_type_info table

## Usage

``` r
list_treatments_by_management_practice(db = NULL, match_trt = TRUE)
```

## Arguments

- db:

  A named list of data frames containing the site_treatment_type_info
  and treatment_id_components table. If left as NULL, these tables are
  imported from directus.

- match_trt:

  Specifies whether to remove treatment types that are not referenced in
  the treatment_id_components table. Defaults to TRUE, with a warning
  mentioning which are removed.

## Value

A named list of treatment types corresponding to a set of core
management practices described for all sites. Useful for identifying
columns in harmonized dataframes with all treatment types.

## Examples

``` r
#not run: management_practice_list <- 
# list_treatments_by_management_practice()
# not run: harmonized_treatments <- 
# harmonize_treatments()
# If I want to do something with N fertility treatments, 
#for example.
#not run: nfert <- harmonized_treatments[,c("site_id",
                                           #"treatmentID2","year"
                                          #,management_practice_list$`N fertility)]
```
