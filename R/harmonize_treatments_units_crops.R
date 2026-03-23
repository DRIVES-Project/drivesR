
#' Generate rotation phase data for a unit_id, rotation_id, and start-end year range
#' 
#' This is a helper function for harmonize_treatments_units_crops(). It takes a one-row data-frame with unit_id, entryPhase, 
#'  rotation_id, minYear, maxYear, and rotation_length_years and uses the rotation_phases table to generate
#'  an expected crop sequence over that year range. This is applied row-wise within the main function.
#' 
#' @param unitrow 
#'  One row of a dataframe called "rotstartend" produced within the harmonize_treatments_units_crops function. 
#' @param rotationphasetable 
#' The rotation_phases table within a list called db (defined in the main function).
#' @import dplyr
#' @returns
#' A dataframe with a year-by-year cropping sequence for the unit/entryphase/rotation/year range. 
#' It includes all of the columns of the rotation_phases table and an additional column called mergeYear,
#' indicating the calendar year that will be used to merge with treatment data (usually the harvest year, sometimes
#' the planting year for over-winter cover crops grouped with the preceding cash crop).
#' 

getPhasesForUnitRow <- function(unitrow = NULL,
                                rotationphasetable = db$rotation_phases){
  ## make sure -999 is NA
  pycols <- c("phase_year_planting","phase_year_harvest","phase_year_termination","typical_planting_month","typical_termination_month")
  rotationphasetable <- rotationphasetable %>%
    mutate(across(all_of(pycols), ~na_if(., -999)))
  
  myrot <- filter(rotationphasetable, rotation_id == unitrow$rotation_id)
  ## get the rotation phases for that row
  ## designate whether the merge year is planting or harvest
  myrot$mergeYear_PorH <- NA
  phasevec <- sort(unique(myrot$phase))
  for(i in seq_along(phasevec)){
    minpharv <- min(myrot$phase_year_harvest[which(myrot$phase== phasevec[i])],na.rm=TRUE)
    maxpharv <- max(myrot$phase_year_harvest[which(myrot$phase == phasevec[i])],na.rm=TRUE)
    if(minpharv == maxpharv){
      myrot$mergeYear_PorH[which(myrot$phase == phasevec[i])] <- "harvest_year"
    }
    if(minpharv < maxpharv){
      myrot$mergeYear_PorH[which(myrot$phase == phasevec[i] &
                                   myrot$phase_year_harvest == minpharv)] <- "harvest_year"
      myrot$mergeYear_PorH[which(myrot$phase == phasevec[i] &
                                   myrot$phase_year_harvest == maxpharv)] <- "planting_year"
    }
  }
  
  entryPhase <- as.numeric(unitrow$entryPhase) # was coded as character
  ## if the entry phase has multiple crops or cover crops, there will be multiple rows with the entry 
  # phase year harvest. Use the minimum (hopefully that works)
  entryPhaseYearHarvest <- min(myrot$phase_year_harvest[which(myrot$phase==entryPhase)], na.rm=TRUE)
  # could be changed to a number upstream, but it might create uncessesary merge issues. 
  # revisit if needed.
  # Get the earliest start year for phase 1. If the entry phase is > 1, 
  # this year will be before the minYear.
  phase1StartYear <- unitrow$minYear - entryPhaseYearHarvest + 1
  ## get adjusted number of cycles for starting at phase 1
  numCycles <- ceiling((unitrow$maxYear - phase1StartYear + 1)/unitrow$rotation_length_years)
  cycleStartYears <- seq(from = phase1StartYear, by = unitrow$rotation_length_years, length.out = numCycles)
  
  # Start at phase 1 and trim off the years outside the range.
  myrot$unit_id <- unitrow$unit_id
  outputdf <- c()
  for(i in seq_along(cycleStartYears)){
    y <- cycleStartYears[i]
    zz1 <- myrot
    zz1$planting_year <- y + zz1$phase_year_planting - 1
    zz1$harvest_year <- y + zz1$phase_year_harvest - 1
    ## put cumulative phases to help with trimming years
    ## in the subsequent cycle, the cumulative phase 1 will be 1 + numphases per cycle * cycles elapsed
    phase1_cumstart  <- 1 + max(zz1$phase)*(i-1)
    gphase1_cumstart  <- 1 + max(zz1$granular_phase)*(i-1)
    zz1$cumulative_phase <- zz1$phase + phase1_cumstart -1
    zz1$cumulative_granular_phase <- zz1$granular_phase + gphase1_cumstart - 1
    outputdf <- rbind(outputdf, zz1)
  }
  
  ## Trim rows outside the year range
  
  ## start with the minimum row for the start year within the entry phase.
  startrow <- min(which(outputdf$phase == entryPhase & outputdf$harvest_year== unitrow$minYear))
  ## In order to include over-winter crops that fall into the next harvest year,
  ## find the maximum cumulative phase with harvest year as the end year
  endPhase <-max(outputdf$cumulative_phase[which(outputdf$harvest_year==unitrow$maxYear)])
  #next, find the maximum granular cumulative phase within the end phase
  endGphase <- max(outputdf$cumulative_granular_phase[which(outputdf$cumulative_phase == endPhase)])
  ## the end row is the maximum row index for the end cumulative granular phase
  endrow <- max(which(outputdf$cumulative_granular_phase == endGphase))
  ## trim to desired start and end years
  outputdf <- outputdf[startrow:endrow,]
  
  ## make a column to indicate the year used for merging with treatment data
  useP <- which(outputdf$mergeYear_PorH=="planting_year")
  useH <- which(outputdf$mergeYear_PorH == "harvest_year")
  outputdf$mergeYear <- NA
  outputdf$mergeYear[useP] <- outputdf$planting_year[useP]
  outputdf$mergeYear[useH] <- outputdf$harvest_year[useH]
  
  ## reorder and prune columns
  outputdf <- dplyr::relocate(outputdf,all_of(c("unit_id","planting_year","harvest_year","mergeYear")),.before = 1) %>%
    select(-cumulative_phase, -cumulative_granular_phase,-mergeYear_PorH) %>%
    rename("rotation_phases_uid" = "uid")
  return(outputdf)
}

#' Harmonize treatments, units, and expected cropping sequences
#' 
#' This function combines data from multiple experimental design database tables into a combined table with the 
#' experimental treatment components, rotation phases, and expected crops for every unit_id and year. 
#'
#' @param db 
#' A named list containing the database tables "treatment_id_info","treatment_id_components","experimental_unit_info",
#' "experimental_unit_treatments","rotation_phases", and "rotation_id_info". If the list is not supplied, it will be imported from
#' Directus (assuming a valid token as been set).
#' 
#' @param includeMixComponents 
#' TRUE or FALSE indicating whether components of crop mixtures should be included. If TRUE, the output dataframe will have 
#' separate rows for the crop mixture (e.g., crop_id = perennial mix) and the crop mixture. These are identified in the columns
#'  is_a_mix and is_a_mix_component from the rotation_phases table. Mix components are identified separately in the planting_info table
#'  for seeding rates.
#'  
#' @param maxyear 
#' Which year to use as the end year for ongoing experiments. The default is 2022.
#' 
#' @returns
#' A data-frame with one row per unit_id/year/crop that includes treatmentIDs, treatment components, and information from 
#' the rotation_phases table. The crop_id column from rotation_phases is renamed expected_crop_id. The output includes planting_year
#' and harvest_year, as in the yield, harvest, and planting data tables. There is another column, mergeYear, that indicates the year used to 
#' merge crop-phase data with treatment data (usually the harvest year, except for over-winter cover crops that extend into the following 
#' calendar year).
#'  
#' @export
#' @import dplyr
#' @examples
#' # not run: 
#' # directus_token <- "Bearer XXXXX" # import Directus credentials 
#' # set_default_token(directus_token) # set credentials
#' # outdf <- harminoze_treatments_units_crops(includeMixComponents = FALSE, maxyear = 2023)
#' 
harmonize_treatments_units_crops <- function(db = NULL, includeMixComponents = TRUE, maxyear = 2022){
  trttables <- c("treatment_id_info","treatment_id_components","experimental_unit_info","experimental_unit_treatments","rotation_phases","rotation_id_info")
  if(!is.null(db)){
    if(any(!trttables %in% names(db))){
      stop(paste("List supplied to db must contain data frames named" ,paste(trttables, collapse=", ")))
    }
  }
  if(!includeMixComponents %in% c(TRUE,FALSE)){
    stop("includeMixComponents must be TRUE or FALSE.")
  }
  
  if(is.null(db)){
    # Import from directus
    #prefix <- ifelse(public ==TRUE,"public_","")
    #dltables <-  paste0(prefix, trttables)
    db <- import_db_tables(tablevec = trttables,
                           fetch_option = "download.only")
  }
  
  ## make sure rotation phase data coded as -999 are converted to NA
  pycols <- c("phase_year_planting","phase_year_harvest","phase_year_termination","typical_planting_month","typical_termination_month")
  db$rotation_phases <- db$rotation_phases %>%
    mutate(across(all_of(pycols), ~na_if(., -999)))
  
  ## get yearly unit -treatment data
  unit_trt <- harmonize_treatments_units(db = db, maxyear = maxyear)
  
  # get start and end years for each unit-rotation combo.
  ## Note: there are overlaps if a rotation is switches back. 
  # This is resolved by re-merging with yearly unit - treatment data
  rotstartend <- unit_trt %>% filter(!is.na(rotation_id))  %>% 
    group_by(site_id, unit_id, rotation_id,entryPhase) %>%
    summarize(minYear = min(year, na.rm=TRUE),
              maxYear = max(year, na.rm=TRUE)) %>% ungroup()
  ## merge in rotation length
  rotstartend <- left_join(rotstartend, select(db$rotation_id_info, rotation_id, rotation_length_years), by = "rotation_id")
  ## each row of rotstartend is the "unitrow" input for the helper function getPhasesForUnitRow
  
  ## get a dataframe of rotation phases for year unit year, based on the start and end years
  # for each rotation in unit_trt.
  unitphasedf <- rotstartend %>%
    dplyr::rowwise() %>%
    dplyr::do(getPhasesForUnitRow(.,rotationphasetable = db$rotation_phases)) %>% ungroup()
  
  # inner join should get rid of any unit/rotation/year combos that don't exist in the harmonized unit_trt dataframe
  unitphasedf2 <- inner_join(unitphasedf, unit_trt, by = c("unit_id","rotation_id","mergeYear" = "year")) #
  # rename columns. 
  unitphasedf3 <- unitphasedf2 %>% rename("expected_crop_id" = "crop_id") %>%
    # prune columns that can easily be merged in with rotation_phases_uid
    select(-crop_substitution_note, -phase_year_harvest, -phase_year_planting, -typical_planting_month, 
           -typical_termination_month, -phase_year_termination, -intercropping, -crop_function_1,
           -crop_function_2, -crop_function_3) %>% 
    ## move a few columns earlier
    relocate(site_id, .before = 1) %>%
    relocate(all_of(c("treatmentID1","treatmentID2","entryPhase")), .after = "unit_id")
  
  if(includeMixComponents == TRUE){
    return(unitphasedf3)
  }
  if(includeMixComponents == FALSE){
    return(filter(unitphasedf3,is_a_mix_component == FALSE))
  }
  
}# END FUNCTION

