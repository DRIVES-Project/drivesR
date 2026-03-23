# Harmonize yield data with experimental treatment data.

Harmonize yield data with experimental treatment data.

## Usage

``` r
harmonize_yields_treatments(db = NULL, crop_fractions_as_columns = FALSE)
```

## Arguments

- db:

  \#' A list of database tables containing named dataframes
  treatment_id_info, treatment_id_components,
  experimental_unit_treatments, and crop_yields. If left NULL, these
  tables are imported from Directus.

- crop_fractions_as_columns:

  \#' If TRUE, multiple fractions from the same crop are organized in
  separate columns. If FALSE, multiple fractions from the same crop are
  organized in separate rows, as in the database table.

## Value

A dataframe of yield data combined with experimental treatment data,
with each treatment type in a separate column. If
crop_fractions_as_columns is set to TRUE, data describing fractions from
the same crop will be described in separate columns, with suffix \_1 for
the primary fraction and \_2, \_3, etc. for other fractions.

## Examples

``` r
 # not run: longyield <- harmonize_yields_treatments(crop_fractions_as_columns = FALSE)
# not run: wideyield <- harmonize_yields(crop_fractions_as_columns = TRUE)
```
