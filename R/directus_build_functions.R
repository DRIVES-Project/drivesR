
#' Map data types from Postgresql to Directus schema
#'
#' @param pgtype 
#' A postgres data type as specified in the column dictionary
#' @returns 
#' A character string with the corresponding datatype recognized by the Directus API.
#' @export
#'
#' @examples
#' pg_to_directus_type("array")
pg_to_directus_type <- function(pgtype){
  mapping_df <- data.frame(pg_type = c("integer",
                                       "text",
                                       "varchar(255)",
                                       "boolean",
                                       "numeric",
                                       "array",
                                       "date"
                                      ),
                           dir_type = c("integer",
                                        "text",
                                        "string",
                                        "boolean",
                                        "float",
                                        "text",
                                        "date"))
  if(length(pgtype) > 1){
    stop("pgtype must be of length 1")
  }
  if(pgtype %in% mapping_df$pg_type){
    return(as.character(mapping_df$dir_type[which(mapping_df$pg_type == pgtype)]))
  }else{
    stop("pgtype not in list")
  }
}
