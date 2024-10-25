#' The "PIT Pipe" is a Shortcut to Get Enrollments on a Specific Day from an Existing Dataframe/Tibble
#'
#' Returns two tibbles showing total enrollments and deduplicated client PIT counts on a specific day per any optional
#' arguments.
#'
#' @param .variable your data
#' @param .condition the PIT date
#'
#' @return The deduplicated ([%pit%]) or full ([%pit-enrollments%]) PIT data frame for the piped data.

# `%tf%` <- function(.variable, .condition)
# {
#   requireNamespace("dplyr", quietly = TRUE)
#   
#   .condition <- as.expression(.condition)
#   
#   return(dplyr::case_when({{.variable}} eval(.condition) ~ TRUE, .default = FALSE))
# }
