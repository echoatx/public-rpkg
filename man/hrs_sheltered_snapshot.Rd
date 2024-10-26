% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_overview_functions.R
\name{hrs_sheltered_snapshot}
\alias{hrs_sheltered_snapshot}
\title{HRS Sheltered Snapshot}
\usage{
hrs_sheltered_snapshot(
  hmis_extract = NULL,
  ...,
  report_effective_date,
  extract_date = NULL
)
}
\arguments{
\item{hmis_extract}{the HMIS CSV Extract.}

\item{...}{These dots are for future extensions and must be empty.}

\item{report_effective_date}{The date of the snapshot.}

\item{extract_date}{The date of the HMIS CSV Extract. (Defaults to the
extract date of \code{hmis_extract}, but it can be set manually)}
}
\value{
A tibble with an estimate of all of the sheltered clients as of the
\code{report_effective_date}.
}
\description{
This function returns a tibble with all of the sheltered clients, per the
HMIS extract, who have not touched the system in the past 180 days, per the
information available in the CA Reference List.
}
