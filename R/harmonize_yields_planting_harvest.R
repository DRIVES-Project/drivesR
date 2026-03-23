#' Harmonize yield with planting and harvest dates.
#'
#' @param db 
#' Can be provided as a named list of database tables
#' with crop_yields, harvest_dates, and planting_info.
#' If NULL (the default) these tables are downloaded from 
#' Directus.
#'
#' @returns
#' A harmonized data frame with one row per unit/year/crop. 
#' ... multiple fractions per from are included in separate columns
#'  with the suffix _f1, _f2, etc. 
#' ...does not show component crops or multiple planting dates. 
#' ... shows multiple harvest dates.
#' @export
#' @import dplyr
#' @import tidyr
#' @examples
#' # not run: set_default_token("Bearer blahblahblah")
#' # not run: yph <- harmonize_yield_planting_harvest() # no db.
#' # not run: yph <- harmonize_yield_planting_harvest(db) # if list is in the environment.
harmonize_yields_planting_harvest <- function(db = NULL, primary_crop_fractions = getOption("drivesR.primary_crop_fractions")){
  dbtables <- c("crop_yields","harvest_dates","planting_info")
  if(!is.null(db)){
    if(any(!dbtables %in% names(db))){
      stop(paste0("List supplied to db must contain data frames named ",paste(dbtables, collapse=", ") ))
    }
  }
  if(is.null(db)){
    db <- import_db_tables(dbtables, fetch_option = "download.only")
  }
  # harmonize each table. 
  yield <- harmonize_yields(crop_yields= db$crop_yields, crop_fractions_as_columns = FALSE)
  harvest <- harmonize_harvest_dates(harvest_dates = db$harvest_dates,crop_fractions_as_columns = FALSE)
  planting <- harmonize_planting_info(planting_info = db$planting_info,replant_dates = "latest", include_component_crops = FALSE)
  
  ## change uid column name
  names(yield)[which(names(yield)=="uid")] <- "crop_yields_uid"
  names(harvest) <- gsub("^uid","harvest_dates_uid", names(harvest))
  names(planting) <- gsub("uid","planting_info_uid", names(planting))
  
  # merge yield and harvest
  yield_harv <- dplyr::left_join(yield, harvest, 
                                 by = c("site_id","unit_id",
                                        "rotation_phase",
                                        "stand_year",
                                        "harvest_year","expected_crop_id","actual_crop_id","measured_fraction"="harvested_fraction"),)
  #pivot wider by crop fraction
  yield_harv <- yield_harv %>% group_by(site_id,actual_crop_id,unit_id,harvest_year)%>%
    mutate(num_fractions = length(unique(measured_fraction)))
  cond1 <- yield_harv$num_fractions==1 | yield_harv$measured_fraction %in% primary_crop_fractions
  yield_harv$primary_fraction <- cond1
  yield_harv <- yield_harv %>% group_by(site_id, actual_crop_id,unit_id, harvest_year) %>%
    mutate(fraction_index = ifelse(primary_fraction,1,2:length(unique(measured_fraction))))
  idcolvec =  c("site_id","unit_id","harvest_year","expected_crop_id",
                "actual_crop_id","stand_year","num_harvests","rotation_phase",
                "stand_year","cover_crop",
                "termination_date",
                "termination_notes")
  valuevec <- setdiff(names(yield_harv), c(idcolvec,"fraction_index","num_fractions","primary_fraction"))
  wide_yield_harv <- tidyr::pivot_wider(yield_harv, 
                                        id_cols = all_of(idcolvec),
                                        names_from = "fraction_index",
                                        values_from = all_of(valuevec),
                                        names_sep = "_f",
                                        unused_fn = list(data_processing_note = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                         harvest_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";"))
  ) %>% select_if(function(x) !all(is.na(x)))  ## remove empty columns.
  
  # merge with planting data
  mergenames <- intersect(names(wide_yield_harv), names(planting))
  wide_planting_yield_harv <- left_join(wide_yield_harv, planting, 
                                        by = mergenames)
  
  return(wide_planting_yield_harv)
}
