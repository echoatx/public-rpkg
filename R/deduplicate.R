#' Deduplicate HMIS Data to One Row Per Unique SPID
#'
#' This function returns the data frame input into it with only one row for each
#' unique PersonalID. It defaults to sorting the data descending by EntryDate
#' for this purpose, if not otherwise specified.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param .data The tibble / data frame to be deduplicated.
#' @param arrange_by The value to arrange in descending order before the
#'   distinct function from the dplyr package is applied to deduplicate the
#'   data. This defaults to EntryDate.
#' @return The deduplicated tibble / data frame that was originally given to the
#'   function.
#' @export
deduplicate <- function(.data, ..., arrange_by = "EntryDate")
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr", quietly = TRUE)
  
  dedup <- .data %>%
    dplyr::arrange(dplyr::desc({{arrange_by}})) %>%
    dplyr::distinct(PersonalID, .keep_all = TRUE)
  
  return(dedup)
}
