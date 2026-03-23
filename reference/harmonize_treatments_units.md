# Harmonize treatment and unit information

Combines information from database tables to describe which treatment
components were applied to which experimental units in which years.

## Usage

``` r
harmonize_treatments_units(db = NULL, maxyear = 2022)
```

## Arguments

- db:

  A list of database tables containing named dataframes
  treatment_id_info, treatment_id_components, and
  experimental_unit_treatments. If left NULL, these tables are imported
  from Directus.

- maxyear:

  The maximum year to use for ongoing experiments. The default is 2022.

## Value

A data frame with one row per unit ID per year and all treatment
components in columns.

## Examples

``` r
# not run: trt_units <- harmonize_treatment_units()
```
