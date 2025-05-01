
.onLoad <- function(libname, pkgname){
  options("drivesR.default.directustoken" = NULL)
  options("drivesR.default.dataversetoken" = NULL)
  options("drivesR.default.url" = "https://data.drives-network.org")
  options("drivesR.primary_crop_fractions" = c("grain","fruit","aboveground biomass","cumulative aboveground biomass"))
  options("drivesR.default.public" = TRUE)
  options("drivesR.default.tablevec" = c("crop_info" ,"crop_variety_info" ,"crop_yields" ,"experimental_unit_info" ,"experimental_unit_treatments" ,"harvest_dates" ,"planting_info" ,"rotation_id_info" ,"rotation_phases" ,"site_info" ,"site_treatment_level_info" ,"site_treatment_type_info" ,"treatment_id_components" ,"treatment_id_info" ,"weather_daily" ,"weather_station_info"))
}