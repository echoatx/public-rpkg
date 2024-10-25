#' Open a Data Frame in Excel
#'
#' Uses MS Excel to open the data frame input as an argument.
#'
#' @param .data the data frame to open in Excel.
#'
#' @return Opens `.data` in Excel.
#' @export
show_in_excel <- function(.data)
{
  requireNamespace("fs",    quietly = TRUE)  
  requireNamespace("readr", quietly = TRUE)
  
  if(interactive())
  {
    tmp <- tempfile(fileext = ".csv")
    readr::write_excel_csv(.data, tmp)
    
    fs::file_show(tmp)
  }
  
  .data
}
