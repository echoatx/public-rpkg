#' The "See Pipe" Lets You *See* a [base::table()] of a Column in a Data Frame
#'
#' Returns a table with the `useNA = "ifany"` argument showing a column of your
#' data frame.
#'
#' @importFrom rlang .data
#'
#' @param .data your data
#' @param .col the column for which you want to *see* a table.
#'
#' @return Returns a table of `.col` from `.data` with the `useNA = "ifany"`
#'   argument.
#' @export
# `%see%` <- function(.data, .col)
# {
#   requireNamespace("dplyr", quietly = TRUE)
#   requireNamespace("rlang", quietly = TRUE)
#   
#   fct_data <- .data |> 
#     dplyr::select({{.col}}) |> 
#     dplyr::mutate(is_Factor = ifelse(is.factor({{.col}}), TRUE, FALSE))
#   
#   if (all(fct_data$is_Factor))
#   {
#     requireNamespace("forcats", quietly = TRUE)
#     
#     fct_data <- fct_data |>
#       dplyr::mutate({{.col}} := forcats::fct_infreq({{.col}}))
#     
#     .col <- as.character(rlang::ensym(.col))
#     
#     return(table(fct_data[.col], useNA = "ifany"))
#   }
#   
#   .col <- as.character(rlang::ensym(.col))
#   
#   return(table(.data[.col], useNA = "ifany"))
# }
`%see%` <- function(.data, .col)
{
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  
  fct_data <- .data |>
    dplyr::select({{.col}}) |>
    dplyr::mutate(is_Factor = ifelse(is.factor({{.col}}), TRUE, FALSE))
  
  if (all(fct_data$is_Factor))
  {
    requireNamespace("forcats", quietly = TRUE)
    
    fct_data <- fct_data |>
      dplyr::mutate({{.col}} := forcats::fct_infreq({{.col}}))
    
    .col <- as.character(rlang::ensym(.col))
    
    return(table(fct_data[.col], useNA = "ifany"))
  }
  
  .col <- as.character(rlang::ensym(.col))
  
  return(table(.data[.col], useNA = "ifany"))
}
# `%see%` <- function(.data, .col)
# {
#   .col <- as.character(rlang::ensym(.col))
#   
#   return(table(.data[.col], useNA = "ifany"))
# }
