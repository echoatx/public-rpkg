% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_hmis_verbose.R
\name{load_hmis_verbose}
\alias{load_hmis_verbose}
\title{Loads an HMIS CSV/XML Export from .rda file format into the Environment.}
\usage{
load_hmis_verbose(
  which_extract,
  ...,
  .backward_compatability = FALSE,
  .use_full_extract = FALSE,
  .FY = 24
)
}
\arguments{
\item{which_extract}{the file path to the hmisData.rda file you want to load,
the date (\code{"YYYY-MM-DD"}) of the extract you want to load, -or- type
\code{"newest"} to load the most recent HMIS extract.}

\item{...}{These dots are for future extensions and must be empty.}

\item{.backward_compatability}{OPTIONAL: if loading an extract from before
03-30-2023, switch this to \code{TRUE} to avoid an error due to previous
standard practice not involving loading the Funder.csv file as part of the
extract data. Defaults to \code{FALSE}.}

\item{.use_full_extract}{OPTIONAL: if loading the full extract (the read-in
.csv files with no preliminary data cleanup), switch this to \code{TRUE}.
Defaults to \code{FALSE}.)}

\item{.FY}{OPTIONAL: the fiscal year of the HMIS data standards for the
extract you want to load. This deafults to the current standards (\code{24}).
You can specify a different fiscal year with \verb{.FY = }XX.}
}
\value{
returns a list of 9 tibbles: client, services, entry, exit, project,
healthanddv, incomebenefits, organization, and disabilities.
}
\description{
Returns the list of tibbles saved in the selected hmisData.rda file.
}
