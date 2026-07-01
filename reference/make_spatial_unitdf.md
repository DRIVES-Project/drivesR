# Make a spatial dataframe for experimental units

This function takes a dataframe with geojson geometry and converts it to
a spatial dataframe (class sf) with a 'geometry' list column It uses
functions in the sf and geojsonsf packages. By default, the geometry
will have the same coordinate reference system as the specified geojson
column (see geomcol). There is an option to transform the geometry to a
different coordinate reference system. For example, for applications
requiring spatial distances at a field scale, it may be desirable to
transform to a local Coordinate Projection System.

## Usage

``` r
make_spatial_unitdf(
  unitdf,
  geomcol = c("xy_geometry_m", "latlon_geometry_dd")[1],
  new_crs = NULL
)
```

## Arguments

- unitdf:

  A dataframe containing the columns "xy_geometry_m" and/or
  "latlon_geometry_dd" from the experimental_unit_info table.

- geomcol:

  Specifies whether which geojson geometry column should be used for the
  output. xy_geometry_m uses a Projected Coordinate System: NAD 1983
  North America Albers Equal Area Conic projection (ESRI:102008). X-Y
  coordinates are northing and easting distances in meters from a global
  center point at 40degN and 96degW.

  latlon_geometry_dd uses a Geographic Coordinate System: WGS 1984
  (EPSG:4326)

- new_crs:

  If left as NULL, the output will be in the same coordinate reference
  system (crs) as the specified geojson column (see geomcol). Otherwise,
  specify the desired crs as an integer or string recognized by
  st_transform in the sf package. See the documentation for st_transform
  for more information.

## Value

A spatial dataframe of class 'sf' with all the columns in the input
unitdf, along with a geometry list column of class "sfc_GEOMETRY"

## Examples
