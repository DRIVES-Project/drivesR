
.onLoad <- function(libname, pkgname){
  options("drivesR.default.directustoken" = NULL)
  options("drivesR.default.url" = "https://data.drives-network.org")
  options("drivesR.primary_crop_fractions" = c("grain","fruit","aboveground biomass","cumulative aboveground biomass"))
  options("drivesR.default.public" = FALSE)
}