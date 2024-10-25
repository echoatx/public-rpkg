#' @export
tryagain <- function(which_extract, ..., .use_full_extract = FALSE)
{
  rlang::check_dots_empty()
  
  # Determine hmis_rda
  if (which_extract == "newest")
  {
    hmis_rda <- ifelse(.use_full_extract,
                       paste0(shortcut("extracts"),
                              "/",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 22, nchar(shortcut("newest extract")) - 14),
                              "FULL_",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 13, nchar(shortcut("newest extract")))),
                       paste0(shortcut("newest extract"), ".encryptr.bin"))
  }
  else if (!file.exists(which_extract))
  {
    extract_date <- as.Date(which_extract)
    
    hmis_rda <- ifelse(.use_full_extract,
                       shortcut("extracts", paste0("hmisData_FULL_", extract_date, ".rda.encryptr.bin")),
                       shortcut("extracts", paste0("hmisData_", extract_date, ".rda.encryptr.bin")))
  }
  else
  {
    hmis_rda <- which_extract
  }
  
  user_key_folder <- paste0(paste(strsplit(here::here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  ghost_folder <- tempdir()
  
  filename_no_path <- strsplit(hmis_rda, "/")[[1]][length(strsplit(hmis_rda, "/")[[1]])]
  
  temp_extract_file <- file.path(ghost_folder, substr(filename_no_path, 1, nchar(filename_no_path) - 13))
  
  options(warn = - 1)
  
  message(paste0("Removing ", temp_extract_file, "."))
  
  file.remove(temp_extract_file)
  
  cli::cli_alert_success("You may now retry entering your password.")
}
