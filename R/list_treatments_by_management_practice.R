#' List treatment components by management practice.
#' 
#' Uses information in the site_treatment_type_info table 
#' @param db 
#' A named list of data frames containing the site_treatment_type_info
#' and treatment_id_components table. If left as
#' NULL, these tables are imported from directus. 
#' @param match_trt
#' Specifies whether to remove treatment types that are not 
#' referenced in the treatment_id_components table. Defaults to TRUE,
#' with a warning mentioning which are removed.
#' @returns
#' A named list of treatment types corresponding to a set of core management practices described for
#' all sites. Useful for identifying columns in harmonized dataframes with all treatment types. 
#' @export
#'
#' @examples
#' #not run: management_practice_list <- 
#' # list_treatments_by_management_practice()
#' # not run: harmonized_treatments <- 
#' # harmonize_treatments()
#' # If I want to do something with N fertility treatments, 
#' #for example.
#' #not run: nfert <- harmonized_treatments[,c("site_id",
#'                                            #"treatmentID2","year"
#'                                           #,management_practice_list$`N fertility)]
list_treatments_by_management_practice <- function(db = NULL,
                                                   match_trt = TRUE){
  
  dbtables <- c("site_treatment_type_info","treatment_id_components")
  if(!is.null(db)){
    if(any(!dbtables %in% names(db))){
      stop("List supplied to db must contain data frames named site_treatment_type_info and treatment_id_components.")
    }
  }
  if(is.null(db)){
    db <- import_db_tables(dbtables,fetch_option = "download.only")
  }
  typesToRemove <- setdiff(db$site_treatment_type_info$treatment_type,
                           db$treatment_id_components$treatment_type)
  if(length(typesToRemove) > 0 & match_trt == TRUE ){
    site_treatment_type_info <- 
      db$site_treatment_type_info[which(!db$site_treatment_type_info$treatment_type %in% typesToRemove),]
    warning("Removed ", length(typesToRemove)," treatment types not matched to a treatment id: ",
            paste(typesToRemove, collapse=", "))
  }else{
    site_treatment_type_info <- db$site_treatment_type_info
  }
  management_practice_list <- tapply(site_treatment_type_info$treatment_type,
                                     site_treatment_type_info$management_practice, unique)
  return(management_practice_list)
}