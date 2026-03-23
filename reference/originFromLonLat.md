# Get origin XY coordinates from longitude and latitude points. Used to estimate georeferenced plot geometry from map dimensions and points selected from Google Earth imagery.

Get origin XY coordinates from longitude and latitude points. Used to
estimate georeferenced plot geometry from map dimensions and points
selected from Google Earth imagery.

## Usage

``` r
originFromLonLat(
  lonlatvec,
  lonlatcrs = 4326,
  outcrs = "ESRI:102008",
  outputtype = c("coordinates", "sf")[1]
)
```

## Arguments

- lonlatvec:

  A numeric vector of length 2 containing longitude followed by latitude
  in decimal degrees. Usually a point taken from Google Earth imagery.

- lonlatcrs:

  The Coordinate Reference System for the longitude latitude points. The
  default is WGS 84, ID EPSG 4326.

- outcrs:

  The Coordinate Reference System for the output coordinates. Usually
  this is a projection to meters. The default is ESRI:102008.

- outputtype:

  An option for whether the output should be a 1-row matrix of X Y
  coordinates (default) or an sf dataframe.

## Value

A 1-row matrix or sf object with the input lon/lat points projected to a
specified CRS. Used as the origin for georeferencing.

## Examples

``` r
bottomright <- c("lon" = -98.846982 ,"lat" =  19.532234)
myorigin <- originFromLonLat(bottomright)
print(myorigin) 
#>              X        Y
#> [1,] -299436.7 -2347768
```
