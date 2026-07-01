#' Harmonize treatment and unit information
#' 
#' Combines information from database tables to describe which treatment components
#' were applied to which experimental units in which years. 
#' @param db 
#' A list of database tables containing named dataframes treatment_id_info,
#' treatment_id_components, and experimental_unit_treatments.
#' If left NULL, these tables are imported from Directus.
#' @param maxyear
#' The maximum year to use for ongoing experiments. The default is 2022.
#' @returns
#' A data frame with one row per unit ID per year and all treatment components in columns. 
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' # not run: trt_units <- harmonize_treatment_units()
harmonize_treatments_units <- function(db = NULL, maxyear = 2022){
  trttables <- c("treatment_id_info","treatment_id_components","experimental_unit_treatments","experimental_unit_info")
  if(!is.null(db)){
    if(any(!trttables %in% names(db))){
      stop("List supplied to db must contain data frames named treatment_id_info,
           treatment_id_components, experimental_unit_treatments, and experimental_unit_info.")
    }
  }
  if(is.null(db)){
    #prefix <- ifelse(public ==TRUE,"public_","")
    #dltables <-  paste0(prefix, trttables)
    db <- import_db_tables(trttables, fetch_option = "download.only")
    
  }
  ## get harmonized treatments.
  tcw <- harmonize_treatments(db = db)
  ## add treatment_id info
  experimental_unit_treatments <- dplyr::left_join(db$experimental_unit_treatments, db$treatment_id_info)
  ## add in the experimental replicate and parent unit id
  experimental_unit_treatments <- dplyr::left_join(experimental_unit_treatments, dplyr::select(db$experimental_unit_info,unit_id,parent_unit_id, experimental_replicate))
  ## expand to a yearly time series and put treatment types in separate columns. 
  yearly_unit_trt <- expand_years(experimental_unit_treatments, na_end_year = maxyear) |>
    tidyr::pivot_wider(id_cols = c('site_id',"unit_id","parent_unit_id","experimental_replicate","year"), 
                       names_from = "treatment_id_type",
                       values_from = "treatment_id" )
  # merge yearly treatment components with yearly_unit_trt.
  yearly_unit_trt2 <- dplyr::left_join(yearly_unit_trt, tcw, by = c("site_id", "treatmentID1","treatmentID2", "year"))
  return(yearly_unit_trt2) 
}# end function