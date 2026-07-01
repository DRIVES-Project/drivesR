

#' Make a spatial dataframe for experimental units
#' 
#' This function takes a dataframe with geojson geometry and converts it to 
#' a spatial dataframe (class sf) with a 'geometry' list column It uses functions in the 
#' sf and geojsonsf packages. By default, the geometry will have the same coordinate
#' reference system as the specified geojson column (see geomcol). 
#' There is an option to transform the geometry to a different coordinate reference system.
#' For example, for applications requiring spatial distances at a field scale, it may be desirable
#' to transform to a local Coordinate Projection System.   
#' 
#' @param unitdf
#' A dataframe containing the columns "xy_geometry_m" and/or 
#' "latlon_geometry_dd" from the experimental_unit_info table.
#'  
#' @param geomcol
#' Specifies whether which geojson geometry column should be used for the output.

#' xy_geometry_m uses a Projected Coordinate System: NAD 1983 North America Albers Equal Area Conic projection (ESRI:102008).
#' X-Y coordinates are northing and easting distances in meters from a global center point at 40degN 
#' and 96degW.
#' 
#' latlon_geometry_dd uses a Geographic Coordinate System: WGS 1984 (EPSG:4326)  
#'  
#' @param new_crs 
#' If left as NULL, the output will be in the same coordinate reference system (crs) as the specified geojson
#' column (see geomcol). Otherwise, specify the desired crs as an integer or string recognized by st_transform in the sf package. 
#' See the documentation for st_transform for more information. 
#' @returns
#' A spatial dataframe of class 'sf' with all the columns in the input unitdf, along with 
#' a geometry list column of class "sfc_GEOMETRY" 
#' 
#' @export
#' @import sf
#' @import geojsonsf
#' @examples
#' myunitdf <- data.frame("unit_id" = "MYSITE_101", 
#'                       "xy_geometry_m" = '{\"type\":\"Polygon\",\"coordinates\":[[[-2100564.1137,117049.3811],[-2100579.6413,116983.3086],[-2100520.734,116970.1587],[-2100505.2064,117036.2312],[-2100564.1137,117049.3811]]]}')
## keep original geometry
#' geomdf <- make_spatial_unitdf(myunitdf, "xy_geometry_m")
#' 
#' project to local CRS
#' geomdf2 <- make_spatial_unitdf(myunitdf, "xy_geometry_m", new_crs = 26942)                     

make_spatial_unitdf <- function(unitdf, 
                                geomcol = c("xy_geometry_m","latlon_geometry_dd")[1],
                                new_crs = NULL){
  crsvec <- c("xy_geometry_m"= "ESRI:102008" , 
              "latlon_geometry_dd" = "EPSG:4326")
  
  dbcrs <- as.character(crsvec[geomcol])
  if(is.null(new_crs)){
    mycrs <- dbcrs
  }else{
    mycrs <- try(sf::st_crs(new_crs), silent=TRUE)
    if(class(mycrs) != "crs"){
      stop("Invalid CRS")
    }

    if(sf::st_can_transform(dbcrs,mycrs) == FALSE){
      stop("Cannot transform to new CRS")
    }
  } 
  
  geometry <- sf::st_sfc(lapply(1:nrow(unitdf), function(x) sf::st_geometrycollection()),crs = mycrs)
  geomdf <- sf::st_sf(unitdf, geometry = geometry)
  
  if(is.null(new_crs)){
    for(i in 1:nrow(unitdf)){
      if(!is.na(geomdf[[geomcol]][i])){
          geomdf$geometry[i] <- geojsonsf::geojson_sfc(unitdf$xy_geometry_m[i]) 
        }
    }
  }else{
    for(i in 1:nrow(unitdf)){
      if(!is.na(geomdf[[geomcol]][i])){
        geomdf$geometry[i] <- geojsonsf::geojson_sfc(unitdf$xy_geometry_m[i]) |>
          sf::st_set_crs(dbcrs) |> # sets crs to what it is in the db
          sf::st_transform(crs = mycrs) # converts to the new crs
          
      }
    }
    
  }
  return(geomdf)
}
