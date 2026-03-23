#' Import database tables
#'
#' @param tablevec
#' A vector of table names to download. If NULL, the function downloads all DRIVES tables
#' available to the user.  
#' @param fetch_option 
#' Indicates how tables are fetched and stored. Options are:
#' "download.save": Tables are imported via Directus API and saved as a list object in 
#' an Rda file. 
#' "download.only": Tables are imported via the Directus API into an object in the global environment.
#' "save.only": Tables are downloaded one-by-one and saved to the local path, without remaining in the
#' global environment. This may be helpful for systems with limited working memory.
#' "upload": The function loads tables from the path specified by savedir and savename into 
#' a list in the global environment.
#' Because the download step takes a while, this option saves time when re-running a the script.  
#' @param save_option
#' Determines how files are saved on the local system. Options are "list" (the default), "rds", 
#' and "csv". The "list" option combines tables into a single named list, db, saved as an RData
#' file. The "rds" and "csv" options save each table to a separate R data or csv file within the directory
#' specified under savedir. Although the list option is more convenient for coding, it requires 
#' sufficient RAM to store all the data in working memory. If working memory is limiting, 
#' it may be better to load tables individually, as needed. 
#' 
#' If the fetch_option is "upload", the save_option should match how they are stored in the 
#' local file system.
#' 
#' @param savedir 
#' Directory path for saving locally. Defaults to the working directory.
#' @param savename 
#' File prefix for saving locally. If the save_option is "list", 
#' this is the entire file name, excluding extensions. Defaults to "drives".
#' If the save_option is "rds" or "csv", the file name will be the prefix followed by the 
#' table name, separated by "_" (e.g., drives_crop_yields.csv). 
#' @returns
#' A named list of DRIVES database tables. 
#' @export
#' @import jsonlite
#' @import purrr
#' @examples
#' # importing tables to a list in the R envirnoment without saving:
#' # not run: db <- import_db_tables(fetch_option = "download.only",save_option = "list")
#' 
#'# saving tables to separate files without keeping them in the R environment:
#' # not run: db <- import_db_tables(fetch_option = "save.only",save_option = "csv")
#' # in this case, the object db will be NULL.
#' 
#' # importing tables into a list in the R envinvment while saving as separate files:
#' # not run: db <- import_db_tables(fetch_option = "download.only",save_option = "csv")
#'  
#' # uploading previously-saved tables into a named list:
#'  # not run: db <- import_db_tables(fetch_option = "upload",save_option = "csv")

import_db_tables <- function(tablevec = getOption("drivesR.default.tablevec"), 
                             
                             fetch_option =c("download.save","download.only","save.only","upload")[1],
                             save_option = c("list","rds","csv")[1],
                             savedir = ".", 
                             savename = "drives"){
  ## Check inputs --------
  if(!fetch_option %in% c("download.save","download.only","save.only","upload")){
    stop("fetch_option must be 'download.save', 'download.only','save.only', or 'upload'")
  }
  if(!save_option %in% c("list","rds","csv")){
    stop("save_option must be 'list', 'rds', or 'csv'")
  }
  if(fetch_option != "download.only" & !dir.exists(savedir)){
    stop("savedir must be a valid directory if saving or uploading data.")    
  }
  if(fetch_option == "download.only" & save_option != "list"){
    stop("save_option must be 'list' for fetch_option = 'download.only")
  }
  if(fetch_option == "upload"){
    if(save_option == "list"){   
      fpath <- file.path(savedir,paste0(savename,".Rdata"))
      if(!file.exists(fpath)){
        stop("File path does not exist:\n",fpath)
      }
      load(fpath)
      cat(paste0("\nImported list db with tables:\n"))
      purrr::walk(names(db), function(x){cat(paste0("\n",x))})
    }
    if(save_option == "rds"){
      filevec <- paste0(savename,"_",tablevec,".rds")
      if(any(!filevec %in% list.files(savedir))){
        missingfiles <- setdiff(filevec,list.files(savedir))
        stop("Files missing from savedir:\n", paste(missingfiles,collapse="\n"))
      }
      db <- purrr::map(filevec, ~ readRDS(file.path(savedir,.x)))
      names(db) <- tablevec
    }
    if(save_option == "csv"){
      filevec <- paste0(savename,"_",tablevec,".csv")
      if(any(!filevec %in% list.files(savedir))){
        missingfiles <- setdiff(filevec,list.files(savedir))
        stop("Files missing from savedir:\n", paste(missingfiles,collapse="\n"))
      }
      db <- purrr::map(filevec, ~ read.csv(file.path(savedir,.x),na.strings = ""))
      names(db) <- tablevec
    }
    return(db)
  }
  ##  Fetch table names if null ------
  if(fetch_option != "upload"){
    if(is.null(tablevec)){ 
      ## query the database for all table names available to the user.
      ## Note: I tried excluding internal directus collections as a query, but it 
      # didn't work.
      collection_info <- get_db_info(mytarget = "collections",output_format = "data.frame" )
      ## remove internal directus tables.
      tablevec <- collection_info$collection[which(!grepl("^directus_", collection_info$collection))]
      tablevec <- gsub("^public_","", tablevec)
    } # ends if(is.null(tablevec)
    ## list ------
    if(save_option == "list"){
      db <- purrr::map(tablevec, ~get_db_table(table_name = .x))
      names(db) <- tablevec 
      if(fetch_option != "download.only"){
        save(db, file = file.path(savedir, paste0(savename,".Rdata")))
      }
      if(fetch_option != "save.only"){
        return(db)
      }
    }# ends if list
    ## rds ---------
    if(save_option == "rds"){
      fnames <- paste0(savename,"_",tablevec,".rds")
      purrr::walk2(tablevec, fnames, ~ {
        mydf <- get_db_table(table_name = .x)
        saveRDS(mydf,file = file.path(savedir,.y))
      })
      if(fetch_option == "download.save"){
        db <-  purrr::map(fnames, ~ readRDS(file = file.path(savedir,.x)))
        names(db) <- tablevec
        return(db)
      }
    }# ends if rds 
    ## csv-------
    if(save_option == "csv"){
      fnames <- paste0(savename,"_",tablevec,".csv")
      purrr::walk2(tablevec, fnames, ~ {
        mydf <- get_db_table(table_name = .x)
        write.csv(mydf,file = file.path(savedir,.y),row.names = FALSE, na = "")
      })
      if(fetch_option == "download.save"){
        db <-  purrr::map(fnames, ~ read.csv(file = file.path(savedir,.x),na.strings = ""))
        names(db) <- tablevec
        return(db)
      }
    } # ends if csv
    
  } # ends if not upload
  
}# ends function

