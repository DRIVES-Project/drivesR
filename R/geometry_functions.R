
#' Bearing angle
#' Calculate the angle between two geographic points relative to true north (clockwise)
#' @param lat1 
#' Latitude of starting point in decimal degrees.
#' @param lon1 
#' Longitude of starting point in decimal degrees
#' @param lat2 
#' Latitude of end point in decimal degrees
#' @param lon2 
#' Longitude of end point in decimal degrees.
#'
#' @returns
#' A single number in degrees, representing the direction from start to end point. 
#' 0 degrees means the end point is due north of the start point. 90 degrees means it is due east.
#' 
#' @export
#'
#' @examples
#' p1 <- c("lon"= -80.411367,"lat" = 43.640741)
#' p2 <- c("lon" = -80.410466 , "lat" =  43.641394)
#' mybearing <- bearing(
#'   lat1 = p1["lat"], 
#'   lon1 = p1["lon"], 
#'   lat2 = p2["lat"], 
#'   lon2 = p2["lon"])
#' 
bearing <- function(lat1, lon1, lat2, lon2) {
  delta_lon <- (lon2 - lon1) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  x <- sin(delta_lon) * cos(lat2)
  y <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(delta_lon)
  angle <- atan2(x, y) * 180 / pi
  (angle + 360) %% 360  # Normalize to 0–360°
}




#' Shift and rotate a geometry object.
#' To help translate plot layout information into spatial coordinates.
#' @param sfgeom 
#' A feature collection of class sfc
#' @param counterclockwise_angle_deg
#' The rotation angle counterclockwise from true north (negative bearing). 
#' @param origin 
#' A 1-row matrix of X followed by Y coordinates of the origin 
#' (usually lon, lat projected to meters).
#'
#' @returns
#' A geometry object of the same type
#' @export
#' @import sf
#' @examples
#' # coordinates derived from plot metadata and an origin on the bottom-left of the map.
#' cornerpoints <- rbind(c(0,0),
#'                      c(0,10),
#'                      c(10,10),
#'                      c(10,0),
#'                      c(0,0))
#' mygeom <- sf::st_sfc(sf::st_polygon(list(cornerpoints)))
#'# points used to estimate rotation:
#'# Eyeballed from Googled maps imagery.
#' topleft <- c("lon" = -80.410466 , "lat" =  43.641394)
#' bottomleft <- c("lon"= -80.411367,"lat" = 43.640741)
#' myorigin <- sf::st_coordinates(
#' sf::st_transform(
#'    sf::st_sfc(sf::st_point(c(bottomleft['lon'], bottomleft['lat'])), crs = 4326),
#'    "ESRI:102008"))

#'## calculate bearing (degrees clockwise from true north)
#'mybearing <- bearing(lon1 = bottomleft['lon'],
#'                     lat1 = bottomleft['lat'],
#'                     lon2 = topleft['lon'],
#'                     lat2 = topleft['lat'])
#'
#'newpolygon <- transform_geometry(mygeom, -mybearing,myorigin)

transform_geometry <- function(sfgeom, counterclockwise_angle_deg, origin,outcrs = "ESRI:102008") {
  # Convert angle to radians (negative for clockwise if needed)
  angle_rad <- counterclockwise_angle_deg * pi / 180
  
  # Build rotation matrix (counterclockwise)
  rotation_matrix <- matrix(c(
    cos(angle_rad), -sin(angle_rad),
    sin(angle_rad),  cos(angle_rad)
  ), nrow = 2, byrow = TRUE)
  
  # Extract coordinates and apply transformation
  coords <- st_coordinates(sfgeom)[, 1:2] %*% t(rotation_matrix)
  translated <- sweep(coords, 2, origin, "+")
  
  # Rebuild geometry based on original type
  geom_type <- st_geometry_type(sfgeom, by_geometry = FALSE)
  
  transformed_geom <- switch(as.character(geom_type),
                             "POINT"      = st_point(translated[1, ]),
                             "LINESTRING" = st_linestring(translated),
                             "POLYGON"    = st_polygon(list(translated)),
                             "MULTIPOINT" = st_multipoint(translated),
                             "MULTILINESTRING" = st_multilinestring(list(translated)),
                             "MULTIPOLYGON" = st_multipolygon(list(list(translated))),
                             stop("Unsupported geometry type")
  )
  
  return(st_sfc(transformed_geom, crs = outcrs))
}

#' Get origin XY coordinates from longitude and latitude points.
#' Used to estimate georeferenced plot geometry from map dimensions and 
#' points selected from Google Earth imagery.
#' @param lonlatvec 
#' A numeric vector of length 2 containing longitude followed by latitude 
#' in decimal degrees. Usually a point taken from Google Earth imagery. 
#' @param lonlatcrs 
#' The Coordinate Reference System for the longitude latitude points. 
#' The default is WGS 84, ID EPSG 4326.
#' @param outcrs 
#' The Coordinate Reference System for the output coordinates.
#' Usually this is a projection to meters. The default is ESRI:102008.
#' @param outputtype 
#' An option for whether the output should be a 1-row matrix of 
#' X Y coordinates (default) or an sf dataframe.
#'
#' @import sf
#' @returns
#' A 1-row matrix or sf object with the input lon/lat points projected to 
#' a specified CRS. Used as the origin for georeferencing.
#' @export
#' 
#' @examples
#' bottomright <- c("lon" = -98.846982 ,"lat" =  19.532234)
#' myorigin <- originFromLonLat(bottomright)
#' print(myorigin) 
originFromLonLat <- function(lonlatvec, 
                             lonlatcrs = 4326,
                             outcrs = "ESRI:102008",
                             outputtype = c('coordinates','sf')[1]){
  if(class(lonlatvec) != "numeric" & length(lonlatvec)!=2){
    stop("lonlatvec must be a numeric vector of length 2 with decimal degree longitude followed by latitute")
  }
  lonlatdf <- as.data.frame(t(lonlatvec))
  names(lonlatdf) <- c('lon','lat')
  lonlatsf <- st_as_sf(lonlatdf,coords = c("lon","lat"),crs = lonlatcrs)
  outsf <- st_transform(lonlatsf, crs = outcrs)
  if(outputtype =="sf"){
    return(outsf)
  }
  if(outputtype=="coordinates"){
    outcoord <-  st_coordinates(outsf)
    return(outcoord)
  }
}

