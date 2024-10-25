#' `easy_table()` Gives You a "Tibble" Formatted Table for a Column of Your Data
#'
#' Returns a `table()` of a column of your data frame, formatted as a `tibble`,
#' pre-sorted descending by count with `NA` last unless otherwise specified.
#'
#' @inheritParams rlang::args_dots_empty
#' @importFrom rlang .data
#'
#' @param .data your data
#' @param .col the column for which you want to *see* a table.
#' @param na_last Defaults to `TRUE` and puts `NA` values last in the table
#'   regardless of the value of their total `Count`. If set to `FALSE`, `NA`
#'   will appear in the table where it falls according to its `Count` value.
#' @param ignore_na Defaults to `FALSE` and sets the `useNA` argument in
#'   [table()] to `"ifany"`. If set to `TRUE` [table()] will run without the
#'   `useNA` argument.
#'
#' @return Returns a table of `.col` from `.data` with the `useNA = "ifany"`
#'   argument used unless `ignore_na` is set to `TRUE`.
#' @export
#' @examples
#' \dontrun{
#'
#' hmis <- load_hmis("newest")
#'
#' # This will return a tibble of `Race_Ethnicity` in the Client file
#' # with the count of any NA's last
#' hmis$client %>% easy_table(Race_Ethnicity)
#'
#' # This will return a tibble of `Race_Ethnicity` in the Client file
#' # with the count of any NA's in descending order with other categories
#' hmis$client %>% easy_table(Race_Ethnicity, na_last = FALSE)
#'
#' # This will return a tibble of `Race_Ethnicity` in the Client file
#' # excluding the count of any NA's
#' hmis$client %>% easy_table(Race_Ethnicity, ignore_na = TRUE)
#'
#' }
easy_table <- function(.data, .col, ..., na_last = TRUE, ignore_na = FALSE)
{
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  rlang::check_dots_empty()
  
  .table_col <- as.character(rlang::ensym(.col))
  
  if (!ignore_na)
  {
    if (na_last)
    {
      return(tibble::as_tibble(as.data.frame(table(.data[.table_col], useNA = "ifany"))) |> 
               dplyr::rename(`Count` = `Freq`) |> 
               dplyr::group_by(is.na({{.col}})) |>
               dplyr::arrange(dplyr::desc(`Count`), .by_group = TRUE) |>
               dplyr::ungroup() |>
               dplyr::select(-3))
    }
    else
    {
      return(tibble::as_tibble(as.data.frame(table(.data[.table_col], useNA = "ifany"))) |> 
               dplyr::rename(`Count` = `Freq`) |>
               dplyr::arrange(dplyr::desc(`Count`)))
    }
  }
  else
  {
    return(tibble::as_tibble(as.data.frame(table(.data[.table_col]))) |>
             dplyr::rename(`Count` = `Freq`) |>
             dplyr::arrange(dplyr::desc(`Count`)))
  }
}
