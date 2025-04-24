# Functions to process database tables from directus.

#' Expand data frame by start and end years
#' 
#' Converts a table with time ranges specified as start- and end-year column into 
#' a longer table with separate rows for each year.
#'
#' @param mydf 
#' A data frame containing start- and end-year columns
#' @param na_end_year 
#' A year to use when end-year is NA. 
#' Start years must not have any NAs.
#' @param sycol
#' Name of the start-year column. Default is start_year. 
#' @param eycol 
#' Name of the end-year column. Default is end_year.
#' @param ycol 
#' Name of the new column for individual years. Default is year.
#' @returns
#' A data-frame with separate rows for each year between start and end year. 
#' Start and end year columns are removed. 
#' @export
#'
#' @examples
#' #Not run: exp_unit_trt <- get_db_table("experimental_units_treatments") 
#' #Not run: exp_trt_yearly <- expand_years(exp_unit_trt)
expand_years <- function(mydf = NULL,
                         na_end_year = 2021, 
                         sycol = "start_year",
                         eycol = "end_year", 
                         ycol = "year"){
  # replace NA end years with na_end_year
  narows <- which(is.na(mydf[,eycol]))
  mydf[narows,eycol] <- ifelse(mydf[narows,sycol] > na_end_year, 
                               mydf[narows,sycol], na_end_year)
  
  if(any(is.na(mydf[,sycol]))){
    stop("start_year column must not contain NAs")
  }
  
  yearvec <- apply(mydf[,c(sycol,eycol)],1, function(x){return(x[1]:x[2])})
  repvec <- sapply(yearvec, length)
  rowreps <- sapply(1:nrow(mydf), function(i){rep(i,repvec[i])})
  expanded_df <- mydf[unlist(rowreps),]
  expanded_df[,ycol] <- unlist(yearvec)
  # remove start and end year columns
  outdf <- expanded_df[,!names(expanded_df) %in% c(sycol,eycol)]
  return(outdf)
}


#' Import dictionary tables
#' These do not require an API key.
#' @param myurl 
#' URL for the Directus database. Set as a default option (data.drives-network.org)
#' @returns
#' A list of three dictionary tables. One describing database tables, one 
#' describing columns within tables, and one describing categories within columns. 
#' @export
#' @import purrr
#' @import httr
#' @examples
#' # not run: dict <- import_dictionary_tables()
import_dictionary_tables <- function(myurl = getOption("drivesR.default.url")){
## dictionaries do not require an API key to access.
  dictvec <- paste0(c("table","column","category"),"_dictionary")
  outlist <- purrr::walk(dictvec, ~ suppressMessages(get_db_table(table_name = .x,mytoken = NULL)))
  names(outlist) <- dictvec
  return(outlist)
}




#' Bulk download database tables
#'
#' @param tablevec
#' A vector of table names to download. If NULL, the function downloads all DRIVES tables
#' available to the user.  
#' @param fetch_option 
#' Indicates how tables are fetched and stored. Options are:
#' "download.save": Tables are imported via Directus API and saved as a list object in 
#' an Rda file. 
#' "download.only": Tables are imported via the Directus API into an object in the global environment.
#' "upload": The function loads an R data object with the path specified by savedir and savename.
#' Because the download step takes a while, this option saves time when re-running a the script.  
#' @param savedir 
#' Directory path for saving locally. Defaults to the working directory.
#' @param savename 
#' File name for saving locally, excluding .Rda. Defaults to "drives_dblist"
##' @param public 
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES 
#' database. FALSE otherwise.
#' Can be set directly or though options. 
#' @param mytoken
#' Directus API token, formatted as "Bearer apitoken". Can be set with set_default_token()
#' @returns
#' A list of DRIVES database tables. 
#' @export
#' @import jsonlite
#' @import purrr
#' @examples
#' # not run: db <- import_db_tables()
import_db_tables <- function(tablevec = getOption("drivesR.default.tablevec"), 
                             fetch_option =c("download.save","download.only","upload")[1],
                             savedir = ".", 
                             savename = "drives_dblist",
                             public = getOption("drivesR.default.public"),
                             mytoken = getOption("drivesR.default.directustoken"),
                             dataverse_api = getOption("drivesR.default.dataversetoken")){
  if(!fetch_option %in% c("download.save","download.only","upload")){
    stop("fetch_option must be 'download.save', 'download.only', or 'upload'")
  }
  if(fetch_option != "download.only" & !dir.exists(savedir)){
    stop("savedir must be a valid directory if saving or uploading data.")    
  }
  if(fetch_option == "upload"){
    load(file.path(savedir,paste0(savename,".Rda")))
    cat(paste0("\nImported list db with tables:\n"))
    purrr::walk(names(db), function(x){cat(paste0("\n",x))})
    return(db)
  }
  if(fetch_option %in% c("download.save","download.only")){
    if(is.null(tablevec)){ 
      ## query the database for all table names available to the user.
      ## Note: I tried excluding internal directus collections as a query, but it 
      # didn't work.
      collection_info <- get_db_info(mytarget = "collections",output_format = "data.frame",mytoken = mytoken )
      ## remove internal directus tables.
      tablevec <- collection_info$collection[which(!grepl("^directus_", collection_info$collection))]
      } # ends if(is.null(tablevec)
    
    db <- purrr::map(tablevec, ~get_db_table(table_name = .x,mytoken = mytoken,public = public, dataverse_api = dataverse_api))
    names(db) <- tablevec ## 
    if(fetch_option == "download.save"){
      save(db, file = file.path(savedir, paste0(savename,".Rda")))
    }
    return(db)
  }# ends if(save_locally == FALSE)
}# ends function


#' Harmonize treatment components
#' 
#' Combines information stored in the tables treatment_id_info and treatment_id_components into a time-series dataset
#' describing combined experimental components for every year.
#' @param db
#' A list of database tables containing named dataframes treatment_id_info and treatment_id_components.
#' If left NULL, these tables are imported from Directus.
#' @param public 
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES database.
#' FALSE otherwise. Can be set directly or though options.
#' @param mytoken 
#' Directus API token, formatted as "Bearer myapitoken". Defaults to option set for "drivesR.default.directustoken"
#' @returns
#' A data frame with columns for site, year, treatmentID1, and treatmentID2 and all treatment types
#' described in the table site_treatment_type_info
#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' #not run: tcw <- harmonize_treatments()
harmonize_treatments <- function(db = NULL, 
                                 public = getOption("drivesR.default.public"),
                                 mytoken = getOption("drivesR.default.directustoken")){
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
      db <- import_db_tables(tablevec = trttables, mytoken = mytoken,fetch_option = "download.only")
      
  }
  # add treatment_id_info to components
  treatment_id_components <- dplyr::left_join(db$treatment_id_components, db$treatment_id_info)
  # make a wide version, separating treatment types 1 and 2. 
  tc1 <- treatment_id_components[which(treatment_id_components$treatment_id_type=="treatmentID1"),] %>% expand_years()
  tc2 <- treatment_id_components[which(treatment_id_components$treatment_id_type=="treatmentID2"),] %>% expand_years()
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
  tcw <- dplyr::left_join(tc2w, tc1w, by = mergenames, suffix = c(".t2",".t1")) %>% as.data.frame()
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


#' Harmonize treatment and unit information
#' 
#' Combines information from database tables to describe which treatment components
#' were applied to which experimental units in which years. 
#' @param db 
#' A list of database tables containing named dataframes treatment_id_info,
#' treatment_id_components, and experimental_unit_treatments.
#' If left NULL, these tables are imported from Directus.
#' @param public 
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES 
#' database. FALSE otherwise. Can be set directly or though options.
#' @param mytoken 
#' Directus token, formatted as "Bearer myapitoken". Can be set with set_default_token()
#' @returns
#' A data frame with one row per unit ID per year and all treatment components in columns. 
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' # not run: trt_units <- harmonize_treatment_units()
harmonize_treatments_units <- function(db = NULL,
                                       public = getOption("drivesR.default.public"),
                                       mytoken = getOption("drivesR.default.directustoken")){
  trttables <- c("treatment_id_info","treatment_id_components","experimental_unit_treatments")
  if(!is.null(db)){
    if(any(!trttables %in% names(db))){
      stop("List supplied to db must contain data frames named treatment_id_info,
           treatment_id_components, and experimental_unit_treatments.")
    }
  }
  if(is.null(db)){
    #prefix <- ifelse(public ==TRUE,"public_","")
    #dltables <-  paste0(prefix, trttables)
    db <- import_db_tables(trttables, mytoken = mytoken, fetch_option = "download.only")
    
  }
  ## get harmonized treatments.
  tcw <- harmonize_treatments(db = db)
  ## add treatment_id info
  experimental_unit_treatments <- dplyr::left_join(db$experimental_unit_treatments, db$treatment_id_info)
  ## expand to a yearly time series and put treatment types in separate columns. 
  yearly_unit_trt <- expand_years(experimental_unit_treatments) %>%
    tidyr::pivot_wider(id_cols = c('site_id',"unit_id","year"), 
                names_from = "treatment_id_type",
                values_from = "treatment_id" )
  # merge yearly treatment components with yearly_unit_trt.
  yearly_unit_trt2 <- left_join(yearly_unit_trt, subset(tcw, select = -c(treatmentID1)),
                                by = c("site_id", "treatmentID2", "year"))
  return(yearly_unit_trt2) 
}


#' List treatment components by management practice.
#' 
#' Uses information in the site_treatment_type_info table 
#' @param site_treatment_type_info 
#' A data frame with the site_treatment_type_info table. If left as
#' NULL, this table is imported from directus. 
#' @param mytoken 
#' Directus token, formatted as "Bearer myapitoken". Set by set_default_token().
#' @returns
#' A named list of treatment types corresponding to a set of core management practices described for
#' all sites. Useful for identifying columns in harmonized dataframes with all treatment types. 
#' @export
#'
#' @examples
#' #not run: management_practice_list <- list_treatments_by_management_practice()
#' # not run: harmonized_treatments <- harmonize_treatments()
#' # If I want to do something with N fertility treatments, for example.
#' #not run: nfert <- harmonized_treatments[,c("site_id","treatmentID2","year",management_practice_list$`N fertility`)]
list_treatments_by_management_practice <- function(site_treatment_type_info = NULL,
                                                   mytoken = getOption("drivesR.default.directustoken")){
  if(is.null(site_treatment_type_info)){
    site_treatment_type_info <- get_db_table("site_treatment_type_info",mytoken = mytoken)
  }
  management_practice_list <- tapply(site_treatment_type_info$treatment_type,
                                     site_treatment_type_info$management_practice, unique)
  return(management_practice_list)
}

#' Harmonize crop yields
#' Performs initial data processing steps for crop yield data. 
#' These include:
#' - Filling in missing actual_crop_id with expected_crop_id. 
#' - Calculating dry yield from yield and yield_percent_moisture (for crops 
#' with yields reported at standardized moisture).
#' - Optionally, the function can reshape the data into a wide format,
#' with multiple crop fractions (e.g., grain and straw) in separate columns instead of
#' separate rows, as they are organized in the database. This option may be convenient 
#' for harvest index calculations and such.
#' 
#' @param crop_yields 
#' A data frame of the crop_yields table from the DRIVES database. If left as NULL, 
#' this table will be downloaded via the Directus API.
#' @param mytoken 
#' Directus token, formatted as "Bearer myapitoken". Can be set with set_default_token().
#' @param crop_fractions_as_columns
#' If TRUE, multiple fractions from the same crop are organized in separate 
#' columns. If FALSE, multiple fractions from the same crop are organized in separate rows, 
#' as in the database table.
#' @param public 
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES 
#' database. FALSE otherwise. Can be set directly or though options.
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
                             public = getOption("drivesR.default.public"),
                             primary_crop_fractions = getOption("drivesR.primary_crop_fractions"),
                             mytoken = getOption("drivesR.default.directustoken")){
  
  if(is.null(crop_yields)){
    dltable <- ifelse(public == TRUE, "public_crop_yields","crop_yields")
    crop_yields <- get_db_table(dltable, mytoken = mytoken)
    
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
#' @param public
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES 
#' database. FALSE otherwise. Can be set directly or though options.
#' @param mytoken 
#' Directus token, formatted as "Bearer myapitoken". Set by set_default_token().
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
    crop_fractions_as_columns = FALSE,
    public =  getOption("drivesR.default.public"),
    mytoken = getOption("drivesR.default.directustoken")){
  ytrttables <- c("treatment_id_info","treatment_id_components","experimental_unit_treatments","crop_yields")
  if(!is.null(db)){
    if(any(!ytrttables %in% names(db))){
      stop(paste0("List supplied to db must contain data frames named ",paste(ytrttables, collapse=", ") ))
    }
  }
  if(is.null(db)){
    #prefix <- ifelse(public==TRUE,"public_","")
    #dltables <- paste0(prefix, ytrttables)
    db <- import_db_tables(ytrttables, mytoken = mytoken, fetch_option = "download.only")
    
  }
  treatmentunits <- harmonize_treatments_units(db = db)
  yields <- harmonize_yields(crop_yields = db$crop_yields, 
                             crop_fractions_as_columns = crop_fractions_as_columns,
                             mytoken = mytoken)
  outyield <- dplyr::left_join(yields, treatmentunits, by = c("site_id","unit_id","harvest_year"="year"))
  return(outyield)
}


#' Harmonize daily weather data
#'
#' @param weather_daily
#' A dataframe corresponding to the weather_daily table
#' in the DRIVES database. If NULL, this table is downloaded from Directus.
#'  
#' @param mytoken
#' Directus token, formatted as "Bearer myapitoken". This can be set 
#'  with set_default_token(). 
#'   
#' @param public 
#' TRUE if weather data is from the publicly available part of the DRIVES database.
#' FALSE otherwise. Can be set directly or though options.
#' 
#' @returns
#' A data frame of mildly processed weather data with weather variables in separate columns
#' (instead of in separate rows as in the database). The processed table also excludes
#' weather_station_id, flag, and uid columns from the original database table. 
#' @export
#' @import tidyr
#' @examples
harmonize_weather <- function(weather_daily=NULL,
                              mytoken = getOption("drivesR.default.directustoken"),
                              public = getOption("drivesR.default.public")){
  if(is.null(weather_daily)){
    ## import from directus
    tablename <- ifelse(public == TRUE,"public_weather_daily","weather_daily")
    weather_daily <- get_db_table(tablename, mytoken = mytoken)
    
  }
  wide_weather <- tidyr::pivot_wider(weather_daily, 
                                     id_cols = c("site_id","year","date","day_of_year"),
                                     names_from = variable,
                                     values_from = value)
  return(wide_weather)
}

#' Harmonize harvest dates
#' Does some light processing of the harvest_dates table.
#'
#' @param harvest_dates 
#' A data frame of the harvest_dates table from the DRIVES database. 
#' If NULL, this table is downloaded from Directus.
#' @param public 
#' TRUE if data are to be downloaded from the publicly available part of the DRIVES 
#' database. FALSE otherwise. Can be set directly or though options.
#' @param crop_fractions_as_columns
#' If TRUE, multiple fractions from the same crop are organized in separate 
#' columns. If FALSE, multiple fractions from the same crop are organized in separate rows, 
#' as in the database table. Although multiple crop fractions are rare, this option
#' helps to merge with the yield data.
#' @param mytoken
#' Directus API token, formatted as "Bearer apitoken". Can be set with set_default_token() 
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
                                    public = getOption("drivesR.default.public"),
                                    crop_fractions_as_columns = FALSE,
                                    primary_crop_fractions = getOption("drivesR.primary_crop_fractions"),
                                    mytoken = getOption("drivesR.default.directustoken")
                                    ){
  if(is.null(harvest_dates)){
    tablename <- ifelse(public == TRUE,"public_harvest_dates","harvest_dates")
    harvest_dates <- get_db_table(tablename, mytoken = mytoken)
    
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


#' Harmonize planting info
#' Reshapes the planting info table based on specifications
#' set by the user. To aid in merging with yield data.
#' @param planting_info 
#' A data frame of the planting_info table from the DRIVES database.
#' If NULL, this will be downloaded from the Directus database based on the 
#' arguments provided under 'public' and 'mytoken'.
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
#' 
#' @param public 
#' TRUE if the user has public approved access, FALSE for internal DRIVES users. 
#' Can be set directly or though options.
#' @param mytoken 
#' Directus token formatted as "Bearer APITOKEN". Can be set with set_default_token().
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
                                    include_component_crops = TRUE,
                                    public = getOption("drivesR.default.public"),
                                    mytoken = getOption("drivesR.default.directustoken")){
  if(is.null(planting_info)){
    tablename <- ifelse(public == TRUE,"public_planting_info","planting_info")
    planting_info <- get_db_table(tablename, mytoken = mytoken)
    
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
  -
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

# harmonize_yields_planting_harvest <- function(db = NULL,
# ){
#   
# }