#' Split A single Column Containing Project Name & Project ID into Separate
#' Columns
#'
#' This function returns a column named `ProjectID` (by default) that removes
#' the 4-digit Project ID number that is sometimes combined with the Project
#' Name when working with BusinessObjects reports instead of the HMIS extract.
#' It is a quick alternative to specifying a [dplyr::mutate()] by hand.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param .data The data frame.
#' @param original_column The column containing the combined Project Name and
#'   Project ID to be separated.
#' @param new_column_name The new column containing only the Project ID. If not
#'   specified, this defaults to `ProjectID`.
#' @param rename_original_column If set to `TRUE`, this will rename the original
#'   column to `ProjectName` to match what it is called in the HMIS extract (if
#'   it is not already called that). Defaults to `FALSE`.
#'
#' @return In the input data frame, a new column (named `ProjectID` by default)
#'   containing only the project ID number, and the old column (renamed
#'   `ProjectName` by default) containing only the project name.
#' @export
split_project_name_id <- function(.data,
                                  original_column,
                                  new_column_name = "ProjectID",
                                  ...,
                                  rename_original_column = TRUE)
{
  requireNamespace("dplyr",    quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)
  requireNamespace("stringr",  quietly = TRUE)
  
  rlang::check_dots_empty()
  
  .data %<>%
    dplyr::mutate({{new_column_name}} := as.double(stringr::str_extract({{original_column}}, '\\d{4}(?=\\))')),
           {{original_column}} := stringr::str_squish(stringr::str_sub({{original_column}}, end = -7)))
  
  if (rename_original_column)
  {
    .data %<>%
      dplyr::rename("ProjectName" = {{original_column}})
  }
  
  return(.data)
}
