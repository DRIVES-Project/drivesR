
.onLoad <- function(libname, pkgname){
  options("drivesR.default.directustoken" = "Bearer myAPItoken")
  options("drivesR.primary_crop_fractions" = c("grain","fruit","aboveground biomass","cumulative aboveground biomass"))
}