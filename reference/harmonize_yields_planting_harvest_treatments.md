# Harmonize crop yields with planting, harvest, and treatment data.

This performs the steps in harmonize_treatment_units and
harmonize_yields_planting_harvest and combines the output.

## Usage

``` r
harmonize_yields_planting_harvest_treatments(db = NULL)
```

## Arguments

- db:

  A named list containing the tables
  "crop_yields","harvest_dates","planting_info","treatment_id_info","treatment_id_components",
  and "experimental_unit_treatments".

## Value

A single data frame with combined output from all these tables.

## Examples

``` r
# not run: monsterdf <- harmonize_yields_planting_harvest_treatments()
```
