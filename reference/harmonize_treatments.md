# Harmonize treatment components

Combines information stored in the tables treatment_id_info and
treatment_id_components into a time-series dataset describing combined
experimental components for every year.

## Usage

``` r
harmonize_treatments(db = NULL, maxyear = 2022)
```

## Arguments

- db:

  A list of database tables containing named dataframes
  treatment_id_info and treatment_id_components. If left NULL, these
  tables are imported from Directus.

- maxyear:

  The maximum year to use for ongoing experiments. The default is 2022.

## Value

A data frame with columns for site, year, treatmentID1, and treatmentID2
and all treatment types described in the table site_treatment_type_info

## Examples

``` r
#not run: tcw <- harmonize_treatments()
```
