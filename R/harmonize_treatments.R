#' Harmonize treatment components
#' 
#' Combines information stored in the tables treatment_id_info and treatment_id_components into a time-series dataset
#' describing combined experimental components for every year.
#' @param db
#' A list of database tables containing named dataframes treatment_id_info and treatment_id_components.
#' If left NULL, these tables are imported from Directus.
#' @param maxyear
#' The maximum year to use for ongoing experiments. The default is 2022.
#' @returns
#' A data frame with columns for site, year, treatmentID1, and treatmentID2 and all treatment types
#' described in the table site_treatment_type_info
#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' #not run: tcw <- harmonize_treatments()
harmonize_treatments <- function(db = NULL, maxyear = 2022){
  # if db is supplied, check for required tables. 
  trttables <- c("treatment_id_info","treatment_id_components")
  if(!is.null(db)){
    if(any(!trttables %in% names(db))){
      stop("List supplied to db must contain data frames named treatment_id_info and treatment_id_components")
    }
  }
  if(is.null(db)){
    # Import from directus
    #prefix <- ifelse(public ==TRUE,"public_","")
    #dltables <-  paste0(prefix, trttables)
    db <- import_db_tables(tablevec = trttables,
                           fetch_option = "download.only")
    
  }
  # add treatment_id_info to components
  treatment_id_components <- dplyr::left_join(db$treatment_id_components, db$treatment_id_info)
  # make a wide version, separating treatment types 1 and 2. 
  tc1 <- treatment_id_components[which(treatment_id_components$treatment_id_type=="treatmentID1"),] %>% expand_years(na_end_year = maxyear)
  tc2 <- treatment_id_components[which(treatment_id_components$treatment_id_type=="treatmentID2"),] %>% expand_years(na_end_year = maxyear)
  tc1w <- dplyr::select(tc1,-uid,-treatment_level_uid,-note,-treatment_id_type) %>% 
    tidyr::pivot_wider(id_cols = c("site_id","treatment_id","year"),
                       names_from = "treatment_type",
                       values_from = "treatment_level") %>%
    rename("treatmentID1" = 'treatment_id')
  tc2w <- dplyr::select(tc2,-uid,-treatment_level_uid,-note,-treatment_id_type) %>% 
    tidyr::pivot_wider(id_cols = c("site_id","treatment_id","year"),
                       names_from = "treatment_type",
                       values_from = "treatment_level") %>%
    dplyr::rename("treatmentID2" = 'treatment_id')
  
  # step 1: merge wide yearly treatment components.
  mergenames <- c("site_id","treatmentID1","year")## cols to merge on
  step1names <- c("treatmentID2","entryPhase") # cols added in step 1
  step2names <- setdiff(names(tc2w), c(mergenames,step1names)) # cols overwritten in step 2.
  #tcw <- dplyr::left_join(tc2w, tc1w, by = mergenames, suffix = c(".t2",".t1")) %>% as.data.frame()
  tcw <- dplyr::full_join(tc2w, tc1w, by = mergenames, suffix = c(".t2",".t1")) %>% as.data.frame()
  ## overlapping treatment components will be given .t2 from tc2 and .t1 from tc1
  ## step 2: overwrite any columns specified differently in components of treatmentID2 
  # (usually due to discrepencies in rotation sequences across phases)
  for(i in seq_along(step2names)){
    # note: tcw must be a dataframe for this to work (not a tibble)
    t2name <- paste0(step2names[i],".t2")
    t1name <- paste0(step2names[i],".t1")
    tcw[,step2names[i]]<- ifelse(is.na(tcw[,t2name]),tcw[,t1name],tcw[,t2name])
  }
  # remove duplicated treatment component columns. 
  dupnames <- purrr::map(step2names, ~paste0(.x, c(".t1",".t2"))) %>% unlist()
  tcw <- tcw %>% dplyr::select(!all_of(dupnames))
  # relocate some columns
  tcw <- tcw %>% dplyr::relocate(treatmentID1, .before = year) %>%
    dplyr::relocate(rotation_id, .before = entryPhase)
  
  return(tcw)   
}# ends function
