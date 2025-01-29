#' Date formats
#'
#' A list of possible date formats.
#'
#' @format A list of vectors
#' \describe{
#'   \item{mdy}{Vector of month-day-year formats}
#'   \item{yyyymd}{Vector of year-month-day formats}
#'   \item{bmo}{Vector with letter or word months}
#'   \item{dmy}{Vector of day-month-year formats}
#'   \item{unlikely}{Vector of unlikely date formats}
#'  
#'   ...
#' }
"date_formats"

#' Sample column dictionary
#'
#' A data frame with database schema information representative of DRIVES tables.
#'
#' @format A data frame with 8 rows and 14 variables:
#' \describe{
#'   \item{column_id}{Unique column identifier}
#'   \item{table_name}{Table identifier}
#'   \item{column_name}{Name of column within the table}
#'   \item{column_order}{Integer}
#'   \item{description}{Text}
#'   \item{postgres_data_type}{Text category}
#'   \item{primary_key}{TRUE or FALSE}
#'   \item{record_identifier}{TRUE or FALSE}
#'   \item{foreign_key_table}{Table name or NA}
#'   \item{foreign_key_column}{Column name or NA}
#'   \item{nullable}{TRUE or FALSE}
#'   \item{unique_value}{TRUE or FALSE}
#'   \item{is_category}{TRUE or FALSE}
#'   \item{auto_increment}{TRUE or FALSE}
#'
#'   ...
#' }
"test_column_dict"

#' Sample table dictionary
#'
#' A data frame with database schema information representative of DRIVES tables.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#'   \item{data_type}{test}
#'   \item{table_name}{Table identifier}
#'   \item{description}{Text}
#'   \item{organization}{Text}
#'
#'   ...
#' }
"test_table_dict"

#' Sample category dictionary
#'
#' A data frame with database schema information representative of DRIVES tables.
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{table_name}{Table identifier}
#'   \item{column_name}{Column identifier within table}
#'   \item{category_level}{Options for the category}
#'   \item{description}{Text}
#'
#'   ...
#' }
"test_category_dict"
