% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/this_project.R
\name{this_project}
\alias{this_project}
\title{Check the Type/Name/ID of a Project}
\usage{
this_project(
  flag,
  project_input,
  ...,
  full_label = FALSE,
  hmis_extract = NULL,
  .FY = 24
)
}
\arguments{
\item{flag}{Either \code{"name"}, \code{"group"}, or \code{"type"}, depending on your
desired output.}

\item{project_input}{Depending on the \strong{flag}:
\itemize{
\item For the \strong{\code{"name"}} flag, the input should be the \code{ProjectID} number.
\item For the \strong{\code{"group"}} flag, the input should be:
\itemize{
\item the group of projects for which you want the project IDs (as a string, i.e. \code{"coc"}), or
\item \code{"ls"} if you need the list of valid group strings.
}
\item For the \strong{\code{"type"}} flag:
\itemize{
\item the \code{ProjectType} label as a string (i.e., \code{"rrh"});
\item the HMIS \code{ProjectType} CSV Code as an integer (i.e., \code{3} for \emph{Permanent Supportive Housing (PSH)});
\item A \code{ProjectID} number to get that project's project type; or
\item \code{"ls"} to get the full list of Project Types.
}
}}

\item{...}{These dots are for future extensions and must be empty.}

\item{full_label}{If set to \code{TRUE} when using the \code{"type"} flag, the function
will return the full title of the project type instead of the shorthand
label (i.e., "Emergency Shelter" instead of "es" ... et cetera).
\emph{\strong{Defaults to \code{FALSE}.}}}

\item{hmis_extract}{The full \verb{CSV Extract} List. This will Automatically use
\code{hmis} if it is in the Global Environment. Otherwise, it will search the
global environment and use the first HMIS extract it finds, or it will call
\link[=load_hmis]{load_hmis("newest")} and use that if there is no HMIS extract
in the environment to use.}

\item{.FY}{The HMIS Data Standards Fiscal Year (entered numerically with two
digits, i.e., \code{22} for FY22). \emph{\strong{Defaults to FY24.}} Can be set backwards
up to FY22 for backwards compatibility with older HMIS Extracts.}
}
\value{
the project \emph{type}, \emph{name}, or \emph{id}.
}
\description{
Returns the \strong{type}, \strong{name}, or \strong{ID} of the project, based on the input
into the function.
}
