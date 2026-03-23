#' Harmonize harvest dates
#' 
#' Does some light processing of the harvest_dates table.
#'
#' @param harvest_dates 
#' A data frame of the harvest_dates table from the DRIVES database. 
#' If NULL, this table is downloaded from Directus.
#' @param crop_fractions_as_columns
#' If TRUE, multiple fractions from the same crop are organized in separate 
#' columns. If FALSE, multiple fractions from the same crop are organized in separate rows, 
#' as in the database table. Although multiple crop fractions are rare, this option
#' helps to merge with the yield data.
#' @param primary_crop_fractions
#' A vector of crop fractions to select as the primary fraction, when there is more than one.
#' So far, this only pertains to grain and tomato fruit. The default is set with the 
#' option drivesR.primary_crop_fractions. 
#' @returns
#' A data frame of harvest date data with minor changes with one row per unit/year/crop.
#' Multiple harvests are separated into columns. Multiple 
#' @export
#' @import dplyr
#' @examples
#' # not run: harv1 <- harmonize_harvest_dates(crop_fractions_as_columns = TRUE)
#' # not run: harv2 <- harmonize_harvest_dates(crop_fractions_as_columns = FALSE)
harmonize_harvest_dates <- function(harvest_dates = NULL,
                                    crop_fractions_as_columns = FALSE,
                                    primary_crop_fractions = getOption("drivesR.primary_crop_fractions")
){
  if(is.null(harvest_dates)){
    harvest_dates <- get_db_table("harvest_dates")
  }
  ## Step 1: fill in missing actual_crop_id with expected_crop_id----
  # fill in missing crop fractions with 'none'
  nacrop <- which(is.na(harvest_dates$actual_crop_id))
  if(length(nacrop) > 0){
    harvest_dates$actual_crop_id[nacrop] <- harvest_dates$expected_crop_id[nacrop]
  }
  nafrac <- which(is.na(harvest_dates$harvested_fraction))
  if(length(nafrac) > 0){
    harvest_dates$harvested_fraction[nafrac] <- "none"
  }
  ## Step 2: if crop fractions are separated into columns-----
  if(crop_fractions_as_columns== TRUE){
    # add fraction_index (similar to harmonize_crop_yields)
    harvest_dates <- harvest_dates %>% group_by(site_id,unit_id,actual_crop_id,harvest_year) %>%
      mutate(num_fractions = length(unique(harvested_fraction)))
    
    cond1 <- harvest_dates$num_fractions==1 | harvest_dates$harvested_fraction %in% primary_crop_fractions
    harvest_dates$primary_fraction <- cond1
    harvest_dates <- harvest_dates %>% group_by(site_id, actual_crop_id,unit_id, harvest_year) %>%
      mutate(fraction_index = ifelse(primary_fraction,1,2:length(unique(harvested_fraction))))
    
    # adjust harvest number using fraction index.
    harvest_dates$harvest_number <- harvest_dates$harvest_number + harvest_dates$fraction_index-1
    ## remove columns I added. 
    harvest_dates <- select(harvest_dates, -num_fractions,-primary_fraction,-fraction_index)
    ## set up columns for pivoting to wide
    idcols = c("site_id","unit_id","expected_crop_id","actual_crop_id","harvest_year","stand_year",
               "rotation_phase","termination_date")
    valcols = c("harvest_date","harvested_fraction","uid")
  }else{
    valcols = c("harvest_date","uid")
    idcols = c("site_id","unit_id","expected_crop_id","actual_crop_id","harvest_year",
               "harvested_fraction","stand_year",
               "rotation_phase","termination_date")
  }
  
  ## Step 3: pivot to wide ------
  namecols = c("harvest_number")
  wide_harvest <- tidyr::pivot_wider(harvest_dates, 
                                     names_from = all_of(namecols),
                                     values_from = all_of(valcols),
                                     unused_fn = list(harvest_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                      termination_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";")))
  return(wide_harvest)
}
