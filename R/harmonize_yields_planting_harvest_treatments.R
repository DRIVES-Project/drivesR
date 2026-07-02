#' Harmonize crop yields with planting, harvest, and treatment data.
#' 
#' This performs the steps in harmonize_treatment_units and harmonize_yields_planting_harvest and combines the output.
#'
#' @param db 
#' A named list containing the tables "crop_yields","harvest_dates","planting_info","treatment_id_info","treatment_id_components", and "experimental_unit_treatments".
#' @returns
#' A single data frame with combined output from all these tables. 
#' @export
#' @import dplyr
#' @examples
#' # not run: monsterdf <- harmonize_yields_planting_harvest_treatments()
#' 
harmonize_yields_planting_harvest_treatments <- function(db = NULL){
  # test input
  dbtables <- c("crop_yields","harvest_dates","planting_info","treatment_id_info","treatment_id_components","experimental_unit_treatments","experimental_unit_info")
  if(!is.null(db)){
    if(any(!dbtables %in% names(db))){
      stop(paste0("List supplied to db must contain data frames named ",paste(dbtables, collapse=", ") ))
    }
  }
  if(is.null(db)){
    db <- import_db_tables(dbtables, fetch_option = "download.only")
  }
  trt <- harmonize_treatments_units(db)
  yph <- harmonize_yields_planting_harvest(db)
  outdf <- dplyr::right_join(trt, yph, by = c("site_id","unit_id","year"="harvest_year"))
  return(outdf)
}