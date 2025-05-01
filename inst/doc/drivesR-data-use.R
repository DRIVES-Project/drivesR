## ---- include = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval=FALSE------------------------------------------------------------------------
#  devtools::install_github("DRIVES-project/drivesR", ref = "main")
#  library(drivesR)

## ----creds1, eval = FALSE----------------------------------------------------------------------------
#  usethis::edit_file("directus_creds.R")

## ----creds2, eval = FALSE----------------------------------------------------------------------------
#  directus_token = "Bearer MYAPIKEY"

## ----setup1, warning=FALSE---------------------------------------------------------------------------
library(drivesR)
source("directus_creds.R")
set_default_token(directus_token)

## ----setup2, eval= FALSE-----------------------------------------------------------------------------
#  set_public_access(public = FALSE)

## ----get_db_table------------------------------------------------------------------------------------
crop_info <- get_db_table("crop_info")
str(crop_info)

## ----default.tablevec--------------------------------------------------------------------------------
options("drivesR.default.tablevec")


## ----import_db_tables1, eval=FALSE-------------------------------------------------------------------
#  db <- import_db_tables(tablevec =  c("crop_yields","site_info","experimental_unit_treatments"))

## ----import_db_tables2, eval=FALSE-------------------------------------------------------------------
#  db <- import_db_tables(fetch_option = "download.save",
#                         #fetch_option = "upload",
#                         savedir = "data",
#                         savename = "sampledblist")

## ----import_db_tables3, eval=TRUE--------------------------------------------------------------------
db <- import_db_tables(#fetch_option = "download.save",
                       fetch_option = "upload",
                       savedir = "data",
                       savename = "sampledblist")

## ----import_db_tables4, eval=FALSE-------------------------------------------------------------------
#  db <- import_db_tables(fetch_option = "save.only",
#                         save_option = "csv",
#                         savedir = "myChosenFolder",
#                         savename = "drives"
#                         )

## ----import_dictionary_tables, eval=FALSE------------------------------------------------------------
#  ## separate:
#  table_dictionary <- get_db_table("table_dictionary")
#  column_dictionary <- get_db_table("column_dictionary")
#  
#  ## together:
#  dict <- import_dictionary_tables()

## ----get_db_info-------------------------------------------------------------------------------------
table_dict_info <- get_db_info(mytarget = "collections/table_dictionary",
                             output_format = "json")
print(table_dict_info)

## ----harmonize_yields, eval=FALSE--------------------------------------------------------------------
#  ## without pre-downloaded tables:
#  yields <- harmonize_yields()
#  ## with pre-downloaded tables:
#  crop_yields <- get_db_table("crop_yields")
#  yields <- harmonize_yields(crop_yields)
#  ## or, if in a list:
#  yields <- harmonize_yields(db$crop_yields)

## ----harmonize2, eval = TRUE-------------------------------------------------------------------------
## crop fractions in rows
#crop_yields <- get_db_table("crop_yields")
yields <- harmonize_yields(db$crop_yields) # could be a separate dataframe or part of a list
longyields <- harmonize_yields(db$crop_yields,
                               crop_fractions_as_columns = FALSE)
dim(longyields)
names(longyields)
## crop fractions in columns
wideyields <- harmonize_yields(db$crop_yields,
                               crop_fractions_as_columns = TRUE)
dim(wideyields)
names(wideyields) # columns include suffix _1 for the primary fraction and _2, _3, etc. for other fractions.

## ----harmonize_treatments, eval=FALSE----------------------------------------------------------------
#  ## tables provided
#  trt <- harmonize_treatments(db) # db is a named list with 'treatment_id_info' and 'treatment_id_components'
#  
#  # tables not provided
#  trt <- harmonize_treatments()
#  

## ----harmonize_treatments_units, eval=FALSE----------------------------------------------------------
#  ## tables provided
#  trtunit <- harmonize_treatments_units(db) # db is a named list with 'treatment_id_info', 'treatment_id_components', and 'experimental_unit_treatments'
#  
#  # tables not provided
#  trtunit <- harmonize_treatments_units()
#  
#  

## ----list_treatments_by_management_practice, eval = TRUE---------------------------------------------
trt <- harmonize_treatments(db)
trtlist <- list_treatments_by_management_practice(db$site_treatment_type_info)
# lets say I wanted to extract information on N fertility treatments
trtcolz <- trtlist$`N fertility`
trtsubset <- trt[c("site_id","year", trtcolz)]
head(trtsubset)


## ----harmonize_planting_info, eval=TRUE--------------------------------------------------------------
#planting_info <- get_db_table("planting_info") # can be a separate table or part of a list.
p1 <- harmonize_planting_info(db$planting_info, replant_dates = "latest",include_component_crops = TRUE)
dim(p1)
names(p1)# names have a numbered suffix for component crops in mixtures.
# And since replant_dates = 'latest', there is an added column replantedTF indicating whether a unit was replanted.

p2 <- harmonize_planting_info(db$planting_info, replant_dates = "rows",include_component_crops = FALSE)
dim(p2)
names(p2) # All planting dates are included and organized under a date_index column. Since include_component_crops is FALSE, there is an added column num_components providing the number of component crops within each actual_crop_id.

p3 <- harmonize_planting_info(db$planting_info, replant_dates = "columns",include_component_crops = TRUE)
dim(p3)
names(p3) # names include suffixes d and c for planting dates and component crops, respectively.


## ----harmonize_harvest_dates, eval=TRUE--------------------------------------------------------------
#harvest_dates <- get_db_table("harvest_dates")# can be a separate table or part of a list.
h1 <- harmonize_harvest_dates(db$harvest_dates, crop_fractions_as_columns = FALSE)
dim(h1)
names(h1)

h2 <- harmonize_harvest_dates(db$harvest_dates, crop_fractions_as_columns = TRUE)
dim(h2) # only slightly different.
names(h2)

## ----harmonize_weather, eval=TRUE--------------------------------------------------------------------
#weather_daily <- get_db_table("weather_daily")# can be a separate table or part of a list.
str(db$weather_daily) # long version
weatherdf <- harmonize_weather(db$weather_daily)
str(weatherdf) # wide simplified version


## ----harmonize_yields_treatments, eval=TRUE----------------------------------------------------------
# db <- import_db_tables(c("treatment_id_info","treatment_id_components","experimental_unit_treatments","crop_yields"),fetch_option = "download.only")
hyt <- harmonize_yields_treatments(db, crop_fractions_as_columns = TRUE) 
names(hyt) #names will have numbered suffixes for crop fractions.
dim(hyt)

hyt <- harmonize_yields_treatments(db, crop_fractions_as_columns = FALSE)  
names(hyt) 
dim(hyt)


## ----harmonize_yields_planting_harvest, eval=TRUE----------------------------------------------------
names(db) # full list including yield, planting, and harvest data. 
yph <- harmonize_yields_planting_harvest(db)
dim(yph)
names(yph)

## ----harmonize_yields_planting_harvest_treatments, eval=TRUE-----------------------------------------
monsterdf <- harmonize_yields_planting_harvest_treatments(db)
dim(monsterdf)
names(monsterdf)

