% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_pipeline.R
\name{harmonize_yields_planting_harvest_treatments}
\alias{harmonize_yields_planting_harvest_treatments}
\title{Harmonize crop yields with planting, harvest, and treatment data.}
\usage{
harmonize_yields_planting_harvest_treatments(db = NULL)
}
\arguments{
\item{db}{A named list containing the tables "crop_yields","harvest_dates","planting_info","treatment_id_info","treatment_id_components", and "experimental_unit_treatments".}
}
\value{
A single data frame with combined output from all these tables.
}
\description{
This performs the steps in harmonize_treatment_units and harmonize_yields_planting_harvest and combines the output.
}
