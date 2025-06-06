% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directus_build_functions.R
\name{make_relations_json}
\alias{make_relations_json}
\title{Make relations json schema objects from data dictionary tables}
\usage{
make_relations_json(
  columndf = test_column_dict[which(test_column_dict$table_name == "test_favorite_toy"),
    ],
  tablerow = test_table_dict[2, ],
  update_action = "CASCADE",
  delete_action = "NO ACTION"
)
}
\arguments{
\item{columndf}{A dataframe of schema information about each column (field) from the column dictionary.}

\item{tablerow}{A one-row dataframe of schema information about the table (collection) from the table dictionary.}

\item{update_action}{Action if the foreign key value is modified in it's original table. The default is CASCADE, which
means that the update will cascade to related records. NO ACTION prevents records from being modified if they have
dependent records}

\item{delete_action}{Action if the foreign key is deleted in its original table. The default is NO ACTION, which prevents records
from being modified if they have dependent records. Another useful option might be SET DEFAULT or SET NULL.}
}
\value{
A list of json objects. This list must be subsetted to use in api calls
}
\description{
Make relations json schema objects from data dictionary tables
}
\examples{

#testrel <- make_relations_json(columndf = 
 #                                 test_column_dict[
  #                                which(test_column_dict$table_name=="test_favorite_toy"),],
 #                                tablerow = test_table_dict[2,])
# If the table has multiple foreign keys, you can use this in a loop (lapply doesn't work)
#not run: rel_req <- api_request("POST",mytarget = "relations",jsonbody = testrel[[1]])


}
