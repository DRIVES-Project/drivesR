#' Harmonize yield data with experimental treatment data.
#'
#' @param db 
#' #' A list of database tables containing named dataframes treatment_id_info,
#'  treatment_id_components, experimental_unit_treatments, and crop_yields.
#' If left NULL, these tables are imported from Directus.
#' @param crop_fractions_as_columns
#' #' If TRUE, multiple fractions from the same crop are organized in separate 
#' columns. If FALSE, multiple fractions from the same crop are organized in separate rows, 
#' as in the database table. 
#' @returns
#' A dataframe of yield data combined with experimental treatment data, with each treatment
#' type in a separate column. 
#' If crop_fractions_as_columns is set to TRUE, 
#' data describing fractions from the same crop will be described in separate columns, with 
#' suffix _1 for the primary fraction and _2, _3, etc. for other fractions. 
#' @export
#' @import dplyr
#' @examples
#'  # not run: longyield <- harmonize_yields_treatments(crop_fractions_as_columns = FALSE)
#' # not run: wideyield <- harmonize_yields(crop_fractions_as_columns = TRUE)
harmonize_yields_treatments <- function(
    db = NULL,
    crop_fractions_as_columns = FALSE){
  ytrttables <- c("treatment_id_info","treatment_id_components","experimental_unit_treatments","crop_yields")
  if(!is.null(db)){
    if(any(!ytrttables %in% names(db))){
      stop(paste0("List supplied to db must contain data frames named ",paste(ytrttables, collapse=", ") ))
    }
  }
  if(is.null(db)){
    db <- import_db_tables(ytrttables, fetch_option = "download.only")
    
  }
  treatmentunits <- harmonize_treatments_units(db = db)
  yields <- harmonize_yields(crop_yields = db$crop_yields, 
                             crop_fractions_as_columns = crop_fractions_as_columns)
  outyield <- dplyr::left_join(yields, treatmentunits, by = c("site_id","unit_id","harvest_year"="year"))
  return(outyield)
}
