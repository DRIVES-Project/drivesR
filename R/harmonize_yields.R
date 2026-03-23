#' Harmonize crop yields
#' 
#' Performs initial data processing steps for crop yield data. 
#' These include:
#' - Filling in missing actual_crop_id with expected_crop_id. 
#' - Calculating dry yield from yield and yield_percent_moisture (for crops 
#' with yields reported at standardized moisture).
#' - Adding a TRUE FALSE column for cover crops.
#' - Optionally, the function can reshape the data into a wide format,
#' with multiple crop fractions (e.g., grain and straw) in separate columns instead of
#' separate rows, as they are organized in the database. This option may be convenient 
#' for harvest index calculations and such.
#' 
#' @param crop_yields 
#' A data frame of the crop_yields table from the DRIVES database. If left as NULL, 
#' this table will be downloaded via the Directus API.
#' @param crop_fractions_as_columns
#' If TRUE, multiple fractions from the same crop are organized in separate 
#' columns. If FALSE, multiple fractions from the same crop are organized in separate rows, 
#' as in the database table.
#' @param primary_crop_fractions
#' A vector of crop fractions to select as the primary fraction, when there is more than one.
#' So far, this only pertains to grain and tomato fruit. The default is set with the 
#' option drivesR.primary_crop_fractions. 
#' @returns
#' A data frame of lightly processed crop yield data. Information stored in the table
#' is used to generate new columns, including dry yield (in kg/ha) and a TRUE/FALSE 
#' column for cover crops.
#' 
#' If crop_fractions_as_columns is set to TRUE, 
#' data describing fractions from the same crop will be described in separate columns, with 
#' suffix _1 for the primary fraction and _2, _3, etc. for other fractions. 
#' @export
#' @import dplyr
#' @import tidyr
#' @examples
#' # not run: longyield <- harmonize_yields(crop_fractions_as_columns = FALSE)
#' # not run: wideyield <- harmonize_yields(crop_fractions_as_columns = TRUE)
harmonize_yields <- function(crop_yields = NULL,
                             crop_fractions_as_columns = FALSE,
                             primary_crop_fractions = getOption("drivesR.primary_crop_fractions")){
  
  if(is.null(crop_yields)){
    crop_yields <- get_db_table("crop_yields")
    
  }
  ## fill in NAs for actual_crop_id
  narows <- which(is.na(crop_yields$actual_crop_id))
  crop_yields$actual_crop_id[narows] <- crop_yields$expected_crop_id[narows]
  
  ## calculate dry yield
  crop_yields <- dplyr::mutate(crop_yields, 
                               dry_yield_kg_ha = yield_kg_ha - yield_kg_ha*(yield_percent_moisture)/100)
  # move next to yield column.
  crop_yields <- dplyr::relocate(crop_yields, dry_yield_kg_ha, .before = yield_kg_ha)
  
  # add column indicating cover crops
  
  crop_yields <- dplyr::mutate(
    dplyr::group_by(crop_yields,unit_id,harvest_year,actual_crop_id),
    cover_crop = all(removed_from_field_tf == FALSE) & actual_crop_id != "fallow"
  )
  
  
  # Stop here for the long option. 
  if(crop_fractions_as_columns == FALSE){
    return(crop_yields)
  }
  if(crop_fractions_as_columns == TRUE){
    ## option for wide vs. long by fraction.
    ## May need some finessing.
    crop_yields <- crop_yields %>% group_by(site_id, actual_crop_id,unit_id, harvest_year) %>%
      mutate(num_fractions = length(unique(measured_fraction)))
    cond1 <- crop_yields$num_fractions==1 | crop_yields$measured_fraction %in% primary_crop_fractions
    crop_yields$primary_fraction <- cond1
    crop_yields <- crop_yields %>% group_by(site_id, actual_crop_id,unit_id, harvest_year) %>%
      mutate(fraction_index = ifelse(primary_fraction,1,2:length(unique(measured_fraction))))
    idcolvec =  c("site_id","unit_id","harvest_year","actual_crop_id","stand_year","num_harvests","rotation_phase")
    valuevec <- setdiff(names(crop_yields), c(idcolvec,"fraction_index","num_fractions","primary_fraction"))
    wide_yields <- tidyr::pivot_wider(crop_yields,id_cols = all_of(idcolvec),
                                      names_from = "fraction_index", 
                                      values_from = all_of(valuevec))
    return(wide_yields)
    
  }
}