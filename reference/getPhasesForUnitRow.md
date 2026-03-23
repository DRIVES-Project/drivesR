# Generate rotation phase data for a unit_id, rotation_id, and start-end year range

This is a helper function for harmonize_treatments_units_crops(). It
takes a one-row data-frame with unit_id, entryPhase, rotation_id,
minYear, maxYear, and rotation_length_years and uses the rotation_phases
table to generate an expected crop sequence over that year range. This
is applied row-wise within the main function.

## Usage

``` r
getPhasesForUnitRow(unitrow = NULL, rotationphasetable = db$rotation_phases)
```

## Arguments

- unitrow:

  One row of a dataframe called "rotstartend" produced within the
  harmonize_treatments_units_crops function.

- rotationphasetable:

  The rotation_phases table within a list called db (defined in the main
  function).

## Value

A dataframe with a year-by-year cropping sequence for the
unit/entryphase/rotation/year range. It includes all of the columns of
the rotation_phases table and an additional column called mergeYear,
indicating the calendar year that will be used to merge with treatment
data (usually the harvest year, sometimes the planting year for
over-winter cover crops grouped with the preceding cash crop).
