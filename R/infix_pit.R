#' The "PIT Pipe" is a Shortcut to Get Enrollments on a Specific Day from an
#' Existing Dataframe/Tibble
#'
#' Returns two tibbles showing total enrollments and deduplicated client PIT
#' counts on a specific day per any optional arguments.
#'
#' @param .data your data
#' @param .pit the PIT date
#'
#' @return The deduplicated ([%pit%]) or full ([%pit-enrollments%]) PIT data
#'   frame for the piped data.
#' @export
`%pit%` <- function(.data, .pit)
{
  requireNamespace("dplyr", quietly = TRUE)
  
  return(.data <- .data |> 
    dplyr::filter((EntryDate <= .pit) 
                  & ((ExitDate >= .pit) | is.na(ExitDate))) |> 
    dplyr::distinct(PersonalID, .keep_all = TRUE))
}

#' @export
`%pit-enrollments%` <- function(.data, .pit)
{
  requireNamespace("dplyr", quietly = TRUE)
  
  return(.data <- .data |> 
    dplyr::filter((EntryDate <= .pit) 
                  & ((ExitDate >= .pit) | is.na(ExitDate))))
}

# Original
# `%pit%` <- function(.df, .pit)
# {
#   requireNamespace("dplyr", quietly = TRUE)
#   
#   return(.df %>%
#            dplyr::filter(EntryDate <= (.pit) 
#                          & (ExitDate >= (.pit) | is.na(ExitDate))) %>%
#            dplyr::distinct(PersonalID, .keep_all = TRUE))
# }
