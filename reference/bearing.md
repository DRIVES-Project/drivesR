# Bearing angle Calculate the angle between two geographic points relative to true north (clockwise)

Bearing angle Calculate the angle between two geographic points relative
to true north (clockwise)

## Usage

``` r
bearing(lat1, lon1, lat2, lon2)
```

## Arguments

- lat1:

  Latitude of starting point in decimal degrees.

- lon1:

  Longitude of starting point in decimal degrees

- lat2:

  Latitude of end point in decimal degrees

- lon2:

  Longitude of end point in decimal degrees.

## Value

A single number in degrees, representing the direction from start to end
point. 0 degrees means the end point is due north of the start point. 90
degrees means it is due east.

## Examples

``` r
p1 <- c("lon"= -80.411367,"lat" = 43.640741)
p2 <- c("lon" = -80.410466 , "lat" =  43.641394)
mybearing <- bearing(
  lat1 = p1["lat"], 
  lon1 = p1["lon"], 
  lat2 = p2["lat"], 
  lon2 = p2["lon"])
```
