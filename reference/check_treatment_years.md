# Check for mistakes in treatment components and years.

Harmonization steps require that: 1) Expanding by start and end years in
experimental_unit_treatments results in a single row per treatment_id
per unit_id per year (one for treatmentID1, one for treatmentID2) 2)
Expanding by start and end years in treatment_id_components results in 1
level per treatment type and treatment id per year. 3) When these
expanded tables are combined, there is only 1 level per treatment type,
unit id, treatment id, and year.

## Usage

``` r
check_treatment_years(db = NULL)
```

## Arguments

- db:

  A list containing named data frames treatment_id_info,
  treatment_id_components, and experimental_unit_treatments. Because
  this function is intended to check for mistakes before uploading to
  the database, this list must be supplied by the user.

## Value

A list with subsetted problem rows of experimental_unit_treatments,
treatment_id_components, and the combined dataframe with treatment
components by experimental unit.

## Examples

``` r
# not run trt_check <- check_treatment_years(db)
```
