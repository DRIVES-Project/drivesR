# Harmonize yield with planting and harvest dates.

Harmonize yield with planting and harvest dates.

## Usage

``` r
harmonize_yields_planting_harvest(
  db = NULL,
  primary_crop_fractions = getOption("drivesR.primary_crop_fractions")
)
```

## Arguments

- db:

  Can be provided as a named list of database tables with crop_yields,
  harvest_dates, and planting_info. If NULL (the default) these tables
  are downloaded from Directus.

## Value

A harmonized data frame with one row per unit/year/crop. ... multiple
fractions per from are included in separate columns with the suffix
\_f1, \_f2, etc. ...does not show component crops or multiple planting
dates. ... shows multiple harvest dates.

## Examples

``` r
# not run: set_default_token("Bearer blahblahblah")
# not run: yph <- harmonize_yield_planting_harvest() # no db.
# not run: yph <- harmonize_yield_planting_harvest(db) # if list is in the environment.
```
