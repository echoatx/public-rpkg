#' Chronicity Calculator for a Data Frame of Clients
#'
#' Estimates whether a client may be experiencing chronic homelessness per the
#' available information in the HMIS export.
#'
#' @param entry_file the 'entry' data frame from the list generated/returned by
#'   the import_hmis function.
#' @param extract_date the date the CSV/XML extract was pulled from HMIS:
#'   YYYY-MM-DD.
#'
#' @return List of  `TRUE` or `FALSE` for each row of the Entry.csv file for
#'   whether that row is estimated to be Chronic.
#' @export
maybeChronic_list <- function(entry_file, extract_date)
{
  requireNamespace("tidyr", quietly = TRUE)
  
  if (!"Date" %in% class(extract_date))
  {
    extract_date <- as.Date(extract_date)
  }
  
  # entryData <- entry_file |>
  #   tidyr::replace_na(list(DisablingCondition = 0)) |>
  #   tidyr::replace_na(list(DateToStreetESSH = as.Date(extract_date))) |>
  #   tidyr::replace_na(list(TimesHomelessPastThreeYears = 0)) |>
  #   tidyr::replace_na(list(MonthsHomelessPastThreeYears = 0))
  
  entryData <- entry_file |>
    tidyr::replace_na(list(DisablingCondition = 0,
                           DateToStreetESSH = extract_date,
                           TimesHomelessPastThreeYears = 0,
                           MonthsHomelessPastThreeYears = 0))
  
  disability <- ifelse(entryData$DisablingCondition == 1, TRUE, FALSE)
  
  # timeline <- ifelse((as.integer(as.Date(extract_date) - as.Date(entryData$DateToStreetESSH)) >= 365)
  #                    | ((entryData$TimesHomelessPastThreeYears >= 4) & (entryData$MonthsHomelessPastThreeYears >= 12)),
  #                    TRUE,
  #                    FALSE)
  
  timeline <- ifelse(extract_date - as.Date(entryData$DateToStreetESSH) >= 365
                     | ((entryData$TimesHomelessPastThreeYears == 4) & (entryData$MonthsHomelessPastThreeYears %in% c(112, 113))),
                     TRUE,
                     FALSE)
  
  chronicStatus <- ifelse(disability & timeline, TRUE, FALSE)
  
  return(chronicStatus)
}
