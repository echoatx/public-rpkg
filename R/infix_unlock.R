#' Decrypt Confidential Columns
#'
#' Decrypt one or more columns in a `tibble`. Note: you must enter the password
#' once for each column you are decrypting.
#'
#' @param lhs the tibble with encrypted columns.
#' @param rhs the column or columns to decrypt. Put the Column names in quotes
#'   (""). If decrypting multiple columns, put all quoted colmn names in [c()].
#'   You will need to enter the password for each column you are decrypting.
#'
#' @return returns the original tibble with the encrypted columns unlocked.
#' @export
`%unlock%` <- function(lhs, rhs)
{
  cli::cli_alert_info("NOTE: You will need to enter the password for each column of data you are decrypting.")
  
  user_key_folder <- paste0(paste(strsplit(here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  # assign(as.character(substitute(lhs)),
  #        encryptr::decrypt(lhs,
  #                          tidyselect::all_of(rhs),
  #                          private_key_path = paste0(user_key_folder, "id_rsa_d")),
  #        pos = parent.frame())
  
  # magrittr::`%<>%`(lhs, encryptr::decrypt(tidyselect::all_of(rhs),
  #                                         private_key_path = paste0(user_key_folder, "id_rsa_d")))
  
  result <- lhs |> 
    encryptr::decrypt(tidyselect::all_of(rhs),
                      private_key_path = paste0(user_key_folder, "id_rsa_d"))
  
  rm(user_key_folder)
  
  if (!"HMIS Data File" %in% class(lhs))
  {
    return(result)
  }
  
  lhs_string <- strsplit(as.character(substitute(lhs)), "$", fixed = TRUE)
  list_name <- lhs_string[[2]]
  object_name <- lhs_string[[3]]
  
  return(.GlobalEnv[[list_name]][[object_name]] <- result)
}
