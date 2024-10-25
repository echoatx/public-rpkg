#' Append the Inventory.csv File to the HMIS Extract List
#'
#' This function reads the `Inventory.csv` file and [base::append()]s it to the
#' supplied HMIS Extract.
#'
#' @param .data The HMIS extract list.
#'
#' @return As a list, the HMIS Extract with the Inventory.csv file included.
#' @export
append_inventory_file <- function(.data)
{
  requireNamespace("readr", quietly = TRUE)
  
  if ("exportInterval" %in% names(.data))
  {
    requireNamespace("lubridate", quietly = TRUE)
    
    export_date <- as.Date(lubridate::int_end(.data$exportInterval))
  }
  else
  {
    export_date <- .data$extractDate
  }
  
  cli::cli_alert_info("Loading the inventory file from the full HMIS export. This will likely take a minute, and you may be prompted to enter a passowrd.")
  
  .inventory <<- load_hmis(as.character(export_date), .use_full_extract = TRUE)$Inventory.csv
  
  inventory <- .inventory
  
  rm(.inventory, pos = 1)
  
  .data <- .data |> append(list(inventory), after = 9)
  
  names(.data)[10] <- "inventory"
  
  .data <- lapply(.data, applyHmisClass)
  
  class(.data) <- c("HMIS Extract", class(.data))
  
  return(.data)
}

# append_inventory_file_original <- function(hmis_extract = NULL)
# {
#   requireNamespace("readr", quietly = TRUE)
#   
#   if ("exportInterval" %in% names(hmis_extract))
#   {
#     requireNamespace("lubridate", quietly = TRUE)
#     
#     export_date <- as.Date(lubridate::int_end(hmis_extract$exportInterval))
#   }
#   else
#   {
#     export_date <- hmis_extract$extractDate
#   }
#   
#   # inventory <- suppressWarnings(readr::read_csv(shortcut("sdr data",
#   #                                                        paste0("CSVExtract",
#   #                                                               gsub("-", "", hmis_extract$extractDate)),
#   #                                                        "Inventory.csv"),
#   #                                               show_col_types = FALSE))
#   
#   cli::cli_alert_info("Loading the inventory file from the full HMIS export. This will likely take a minute, and you may be prompted to enter a passowrd.")
#   
#   # inventory <- load_hmis(as.character(export_date), .use_full_extract = TRUE)$Inventory.csv
#   # 
#   # hmis_extract <- hmis_extract |> append(list(inventory), after = 9)
#   # 
#   # names(hmis_extract)[10] <- "inventory"
#   
#   .inventory <<- load_hmis(as.character(export_date), .use_full_extract = TRUE)$Inventory.csv
#   
#   inventory <- .inventory
#   
#   return(inventory)
#   
#   # rm(.inventory, pos = 1)
#   
#   hmis_extract <- hmis_extract |> append(list(inventory), after = 9)
#   
#   names(hmis_extract)[10] <- "inventory"
#   
#   return(hmis_extract)
# }
