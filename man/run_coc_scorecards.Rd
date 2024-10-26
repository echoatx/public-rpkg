% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_coc_scorecards.R
\name{run_coc_scorecards}
\alias{run_coc_scorecards}
\title{Generate the CoC Quarterly Scorecards}
\usage{
run_coc_scorecards(
  hmis_extract = NULL,
  ...,
  .year = as.double(format(Sys.Date(), "\%Y")),
  .quarter,
  .hmis_extract_full = NULL,
  .project_list = NULL,
  .nonHmisData = NULL,
  .referrals_report = NULL,
  .exemptions = NULL,
  .straight_to_excel = FALSE,
  .save_result_tables = TRUE,
  .show_diagnostics = FALSE
)
}
\arguments{
\item{hmis_extract}{The standard HMIS CSV Export You may specify the \code{.rda}
filepath to read, or the export variable (\code{list}) in your environment to
use. You may also pipe the export into this function. If no export is
explicitly supplied, this function will automatically use a standard export
called \code{hmis} if there is one in the Global Environment. Otherwise it will
search the Global Environment for any standard HMIS Export and use the
first one it finds, warning/informing you that it did so, and telling you
which one it used.}

\item{...}{These dots are for future extensions and must be empty.}

\item{.year}{The year for which the scorecards are being run. This defaults
to the current year. It can be set to a different year. (This input should
be a number, not a string.)}

\item{.quarter}{The quarter (\code{1}, \code{2}, \code{3}, or \code{4}) for which the scorecards
are being run. (This input should be a number, not a string.)}

\item{.hmis_extract_full}{The \emph{\strong{FULL}} HMIS CSV Export You may specify the
\code{.rda} filepath to read, or the export variable (\code{list}) in your
environment to use. If no export is explicitly supplied, this function will
automatically use an export called \code{hmis_full} if there is one in the
Global Environment. Otherwise it will search the Global Environment for any
full HMIS Export and use the first one it finds, warning/informing you that
it did so and which one it used.}

\item{.nonHmisData}{The Excel spreadsheet containing any non-HMIS data used
for the scorecard analysis (such as client feedback tracking & info). This
defaults to the filepath to where this spreadsheet is stored in the ECHO
Dropbox.}

\item{.referrals_report}{The path to the \verb{CE Referrals.xlsx} file to be used
for the CE Referrals analysis. This defaults to pulling the current version
of the \verb{CE Referrals.xlsx} file in the "Dashboards/data" folder in the ECHO
Dropbox.}

\item{.exemptions}{OPTIONAL: Any project IDs that should be excluded from the
scorecard analysis. (This input should be a number or vector of numbers,
not a string or vector of strings.)}

\item{.straight_to_excel}{OPTIONAL: If set to \code{TRUE}, the function will open
the resulting output in an Excel spreadsheet instead of just in R. Defaults
to \code{FALSE}.}

\item{.save_result_tables}{OPTIONAL: If set to \code{TRUE}, the function will save
the results tables for all metrics by project to your Downloads folder.
Defaults to \code{FALSE}. (This will also save the full scorecard data frame
that \code{.straight_to_excel} would generate, so it is not necessary to set
both \code{.save_result_tables} and \code{.straight_to_excel} to \code{TRUE}.)}

\item{.show_diagnostics}{OPTIONAL: If set to \code{TRUE}, the function will print
additional information to the console when runing metrics. This can help
with troubleshooting errors and finding bugs. Defaults to \code{FALSE}.}
}
\value{
As a tibble, the CoC Scorecards table (optionally, an Excel
spreadsheet if \code{.straight_to_excel} is set to \code{TRUE}) (optionally,
downloading the results by metric by project if \code{.save_result_tables} is
set to \code{TRUE}) for the specified \code{.year}/\code{.quarter} combination.
}
\description{
This function uses both the standard HMIS Export \code{.rda} and the  \emph{full} HMIS
Export \code{.rda} to generate the CoC performance scorecards for the specified
timeframe.
}
