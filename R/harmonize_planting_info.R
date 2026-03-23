#' Harmonize planting info
#' Reshapes the planting info table based on specifications
#' set by the user. To aid in merging with yield data.
#' @param planting_info 
#' A data frame of the planting_info table from the DRIVES database.
#' If NULL, this will be downloaded from the Directus database.
#' @param replant_dates
#' Indicates how multiple planting dates should be organized in the output. 
#' There are three options: latest, rows, and columns.
#' 
#' "latest" means that the output dataframe will exclude earlier dates from 
#' replanted plots, assuming that these were unsuccessful. 
#'
#' "rows" means that the output dataframe will include organize multiple planting dates into 
#' separate rows.
#' 
#' "columns" means that the output dataframe will organize multiple planting dates into separate columns.
#' 
#' @param include_component_crops
#' TRUE or FALSE indicating whether the output should include information about 
#' components of crop mixtures (component_crop_id). If TRUE, the output dataframe will include
#' component crop information in separate columns.
#' @returns
#' A dataframe of planting information arranged as either: 
#' 1) one row per unit_id/harvest_year/actual_crop_id (if replant_dates is "latest" or "columns")
#' 2) one row per unit_id/harvest_year/actual_crop_id/planting_date (if replant_dates is "rows")
#' 
#' If include_component_crops is TRUE, output columns will include suffixes "_1", "_2", etc. for
#' multiple components, based on the component_crop_index column. If replant_dates is also set to 
#' "columns", output columns will include suffixes "_d1_c1","d1_c2","_d2_c1","_d2_c2, etc. for
#' multiple components within each date.
#' 
#' If include_component_crops is FALSE, the output will include an additional integer
#' column, num_component_crops indicating the number of component crops planted on that date.
#' This does not consider component crops planted on earlier planting dates. Also,
#' the output keeps variety_id and planting_rate information for dates with only one component.
#'  
#' If replant_dates is set to "latest", the output will include an added logical column, 
#' replantedTF indicating whether a plot was replanted. The value of date_index indicates how many times
#' the plot was replanted. 
#' 
#' 
#' 
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # latestWithComponents <- harmonize_plantinfo(replant_dates = "latest", include_component_crops = TRUE)
#' # latestWithoutComponents <- harmonize_plantinfo(replant_dates = "latest", include_component_crops = FALSE)
#' # rowsWithComponents <- harmonize_plantinfo(replant_dates = "rows", include_component_crops = TRUE)
#' # rowsWithoutComponents <- harmonize_plantinfo(replant_dates = "rows", include_component_crops = FALSE)
#' # columnsWithComponents <- harmonize_plantinfo(replant_dates = "columns", include_component_crops = TRUE) 
#' # columnsWithoutComponents <- harmonize_plantinfo(replant_dates = "columns", include_component_crops = FALSE

harmonize_planting_info <- function(planting_info = NULL,
                                    replant_dates = c("latest","rows","columns")[1],
                                    include_component_crops = TRUE){
  if(is.null(planting_info)){
    planting_info <- get_db_table("planting_info")
    
  }# closes if
  
  if(replant_dates == "latest"){
    ## "latest" set up ------
    # Filter for the latest date per unit/year/crop
    platest <- planting_info %>% group_by(unit_id,actual_crop_id, harvest_year) %>%
      mutate(latest_date_index = ifelse(any(!is.na(planting_date)),max(date_index[which(!is.na(planting_date))]),
                                        max(date_index)),
             is_latest_date = date_index == latest_date_index,
             replantedTF = latest_date_index > 1,
             planting_notes = paste(unique(planting_notes[!is.na(planting_notes)]),collapse=";")# consolidate notes
      ) %>%
      filter(is_latest_date == TRUE)
    
    # double check:
    if(include_component_crops == TRUE){
      
      # reshape with component crop info in separate columns
      idcols =c(
        "site_id",
        "unit_id",
        "harvest_year",
        "expected_crop_id",
        "actual_crop_id",
        "stand_year",
        "rotation_phase",
        "planting_date",
        "date_index",
        "replantedTF")
      valuecols = c(
        "component_crop_id",
        "variety_id",
        "planting_rate",
        "uid"
      )
      
      outdf <- tidyr::pivot_wider(platest, 
                                  id_cols = all_of(idcols),
                                  names_from = component_crop_index,
                                  values_from = all_of(valuecols),
                                  unused_fn = list(planting_units = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                   material_planted = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                   planting_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";")))
    }
    if(include_component_crops == FALSE){
      
      # remove rows with component crop index > 1 
      ##(double check that this leaves 1 row per unit/year/crop. 
      # otherwise, use a different approach.)
      # remove columns referring to component crops. 
      # maybe leave in planting rate and variety if there's only one component.
      ## QC check
      # checkdf <- platest %>% group_by(unit_id,actual_crop_id, harvest_year) %>%
      #           summarize(nrows = n(),
      #                     maxcomponents = max(component_crop_index),
      #                     ncomponents = length(unique(component_crop_index)),
      #                     num_comp1 = sum(component_crop_index==1))
      #  table(checkdf$nrows==checkdf$maxcomponents)# all true
      #  table(checkdf$nrows==checkdf$ncomponents)# all true
      #  table(checkdf$num_comp1)# all have index 1
      outdf <- platest %>% group_by(unit_id,actual_crop_id, harvest_year) %>%
        mutate(num_components = n()) %>%
        filter(component_crop_index==1) %>%
        # remove variety and planting information if there are multiple component.
        mutate(variety_id = ifelse(num_components==1,variety_id,NA ),
               planting_rate = ifelse(num_components==1,planting_rate,NA),
               planting_units = ifelse(num_components==1,planting_units,NA)) %>%
        select(#-uid,
          -is_latest_date,
          -latest_date_index,
          -component_crop_id,
          -component_crop_index,
          -actual_or_estimated_planting_date)          
      
    }
  }# closes 'latest'
  
  if(replant_dates == "rows"){
    ##"rows"-----
    if(include_component_crops == TRUE){
      # reshape with component crop info in separate columns
      
      idcols = c(
        "site_id" ,
        "unit_id" ,
        "expected_crop_id" ,
        "actual_crop_id" ,
        "stand_year" ,
        "rotation_phase" ,
        "harvest_year",
        "planting_date",
        "date_index"
      )
      valuecols = c(
        "component_crop_id",
        "variety_id",
        "planting_rate",
        "uid"
      )
      outdf <- pivot_wider(planting_info,
                           id_cols = all_of(idcols),
                           names_from = component_crop_index,
                           values_from = all_of(valuecols),
                           values_fn = list,
                           unused_fn = list(planting_notes = ~paste(unique(.x[!is.na(.x)]),collapse="; "),
                                            planting_units = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                            material_planted = ~paste(unique(.x[!is.na(.x)]),collapse=";"))) %>% 
        unnest(-all_of(idcols))
      
    }
    if(include_component_crops == FALSE){
      
      # remove rows with component crop index > 1 
      ##(double check that this leaves 1 row per unit/year/crop. 
      # otherwise, use a different approach.)
      # remove columns referring to component crops. 
      # maybe leave in planting rate and variety if there's only one component.
      outdf <- planting_info %>% 
        group_by(unit_id,actual_crop_id, harvest_year,date_index) %>%
        # count components and consolidate notes
        mutate(num_components = n(),
               planting_notes = paste(unique(planting_notes[!is.na(planting_notes)]),collapse=";") ) %>%
        filter(component_crop_index==1) %>%
        # remove variety and planting information if there are multiple component.
        mutate(variety_id = ifelse(num_components==1,variety_id,NA ),
               planting_rate = ifelse(num_components==1,planting_rate,NA),
               planting_units = ifelse(num_components==1,planting_units,NA)) %>%
        select(#-uid,
          -component_crop_id,
          -component_crop_index,
          -actual_or_estimated_planting_date)          
    }
  }# closes 'rows'
  
  if(replant_dates == "columns"){
    ##"columns"-----
    if(include_component_crops == TRUE){
      # reshape with component crop info in separate columns
      idcols = c(
        "site_id" ,
        "unit_id" ,
        "expected_crop_id" ,
        "actual_crop_id" ,
        "stand_year" ,
        "rotation_phase" ,
        "harvest_year"
      )
      valuecols = c(
        "component_crop_id",
        "planting_date",
        "variety_id",
        "planting_rate",
        "uid"
      )
      outdf <- tidyr::pivot_wider(planting_info,
                                  id_cols = all_of(idcols),
                                  names_from = c(date_index,component_crop_index),
                                  names_glue = "{.value}_d{date_index}_c{component_crop_index}",
                                  values_from = all_of(valuecols),
                                  #values_fn = ~paste(unique(.x),collapse=";")
                                  unused_fn = list(planting_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                   planting_units = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                                   material_planted = ~paste(unique(.x[!is.na(.x)]),collapse=";"))                              
      )   
    }
    if(include_component_crops == FALSE){
      
      # remove rows with component crop index > 1 
      ##(double check that this leaves 1 row per unit/year/crop. 
      # otherwise, use a different approach.)
      # remove columns referring to component crops. 
      # maybe leave in planting rate and variety if there's only one component.
      
      without_components <- planting_info %>% 
        group_by(unit_id,actual_crop_id, harvest_year,date_index) %>%
        # count components and consolidate notes
        mutate(num_components = n(),
               planting_notes = paste(unique(planting_notes[!is.na(planting_notes)]),collapse=";") ) %>%
        filter(component_crop_index==1) %>%
        # remove variety and planting information if there are multiple component.
        mutate(variety_id = ifelse(num_components==1,variety_id,NA ),
               planting_rate = ifelse(num_components==1,planting_rate,NA),
               planting_units = ifelse(num_components==1,planting_units,NA)) %>%
        select(#-uid,
          -component_crop_id,
          -component_crop_index,
          -actual_or_estimated_planting_date)
      
      idcols = c(
        "site_id" ,
        "unit_id" ,
        "expected_crop_id" ,
        "actual_crop_id" ,
        "stand_year" ,
        "rotation_phase" ,
        "harvest_year",
        "num_components"
      )
      valuecols = c(
        "planting_date",
        "planting_rate",
        "variety_id",
        "uid"
      )# planting units and mateiral planted will be consolidated.
      outdf <- tidyr::pivot_wider(without_components,
                                  id_cols = all_of(idcols),
                                  names_from = date_index,
                                  values_from = all_of(valuecols),
                                  unused_fn = list(
                                    planting_notes = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                    planting_units = ~paste(unique(.x[!is.na(.x)]),collapse=";"),
                                    material_planted = ~paste(unique(.x[!is.na(.x)]),collapse=";"))
      )
    }
  }# closes 'columns'
  return(outdf)
} # closes function
