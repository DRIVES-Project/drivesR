# Harmonize harvest dates

Does some light processing of the harvest_dates table.

## Usage

``` r
harmonize_harvest_dates(
  harvest_dates = NULL,
  crop_fractions_as_columns = FALSE,
  primary_crop_fractions = getOption("drivesR.primary_crop_fractions")
)
```

## Arguments

- harvest_dates:

  A data frame of the harvest_dates table from the DRIVES database. If
  NULL, this table is downloaded from Directus.

- crop_fractions_as_columns:

  If TRUE, multiple fractions from the same crop are organized in
  separate columns. If FALSE, multiple fractions from the same crop are
  organized in separate rows, as in the database table. Although
  multiple crop fractions are rare, this option helps to merge with the
  yield data.

- primary_crop_fractions:

  A vector of crop fractions to select as the primary fraction, when
  there is more than one. So far, this only pertains to grain and tomato
  fruit. The default is set with the option
  drivesR.primary_crop_fractions.

## Value

A data frame of harvest date data with minor changes with one row per
unit/year/crop. Multiple harvests are separated into columns. Multiple

## Examples

``` r
# not run: harv1 <- harmonize_harvest_dates(crop_fractions_as_columns = TRUE)
# not run: harv2 <- harmonize_harvest_dates(crop_fractions_as_columns = FALSE)
```
