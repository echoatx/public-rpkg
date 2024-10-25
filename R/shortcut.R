#' Quickly Navigate to Certain Folders in the Research-and-Evaluation-Team
#' Filesystem
#'
#' *Shortcut* is a quicker way to use the [here] function, customized to
#' provide quick access to the ECHO Dropbox, RE Team Dropbox, Small Data
#' Requests (SDR) data folder, HMIS extracts (in Datasets and in SDR/data), CE
#' BNL (in Datasets and in SDR/data), and Dashboard Input/Output folders.
#'
#' @param bookmarkedDirectory as a `string`, the abbreviated name of the folder
#'   you want or which your file is in: `"echo dropbox"`, `"re dropbox"`,
#'   `"extracts"`, `"sdr data"`, `"bnl"`, `"bnl excel"`, `"db inputs"`, `"db
#'   outputs"`. You can also type `"newest extract"` to get the direct link to
#'   the most recent HMIS extract or `"newest bnl"` to get the most recent BNL.
#' @param ... the rest of the path to you file as you would type it in the
#'   [here] function.
#'
#' @return Returns the [here] function with the first part filled in up to the
#'   parent destination folder (`"extracts"`/`"bnl"`/etc) with the rest appended
#'   to reach your destination file, so for example you could type
#'   `shortcut("bnl", "XXXXXXXX CA BNL.xlsx")` from the Dashboard .rproject
#'   instead of `here("../", "Project Requests", "Small Data Requests",. "data"
#'   "XXXXXXXX CA BNL.xlsx")`.
#' @export
shortcut <- function(bookmarkedDirectory, ..., dropbox_username = NULL)
{
  requireNamespace("here",      quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  if (Sys.getenv("OS") != "Windows_NT")
  {
    cli::cli_warn("If you are not running on a Windows OS this function probably will not work!")
  }
  
  where_is_here <- strsplit(here::here(), "/")
  
  windowsUsername <- unlist(strsplit(here::here(), "/"))[3]
  
  if (dir.exists(paste0("C:/Users/", windowsUsername, "/", "ECHO Dropbox")))
  {
    if (length(where_is_here[[1]]) >= 5 & unlist(where_is_here)[4] == "ECHO Dropbox")
    {
      dropboxUsername <- unlist(strsplit(here::here(), "/"))[5]
      
      echoDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername), "/")
      
      reDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
    }
    else
    {
      requireNamespace("fs", quietly = TRUE)
      requireNamespace("purrr", quietly = TRUE)
      
      dropbox_folders <- fs::dir_ls(paste0("C:/Users/", windowsUsername, "/",  "ECHO Dropbox"), type = "directory")
      
      dropbox_user_folder_path <- as.character(purrr::discard(dropbox_folders, \(x) grepl("Timesheets|ECHO Team Folder", x)))
      
      dropboxUsername <- sub("/", "", strsplit(dropbox_user_folder_path, "ECHO Dropbox")[[1]][2])
      
      echoDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername), "/")
      
      reDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
    }
  }
  else
  {
    reDropbox0 <- strsplit(paste0("C:/Users/", strsplit(here::here(), "/")[[1]][3], "/Dropbox (ECHO)/Research-and-Evaluation-Team"), "/")
    
    echoDropbox0 <- strsplit(paste0("C:/Users/", strsplit(here::here(), "/")[[1]][3], "/Dropbox (ECHO)"), "/")
  }
  
  reDropbox <- list()
  
  for (i in 1:length(reDropbox0[[1]]))
  {
    reDropbox[[length(reDropbox) + 1]] <- reDropbox0[[1]][i]
    rm(i)
  }
  
  rm(reDropbox0)
  
  echoDropbox <- list()
  
  for (i in 1:length(echoDropbox0[[1]]))
  {
    echoDropbox[[length(echoDropbox) + 1]] <- echoDropbox0[[1]][i]
    rm(i)
  }
  
  rm(echoDropbox0)
  
  if (file.exists(paste0(paste(unlist(echoDropbox), collapse = "/"), "/Housing for Health")))
  {
    saPath <- "Housing for Health"
  }
  else if (file.exists(paste0(paste(unlist(echoDropbox), collapse = "/"), "/Systems Advancement")))
  {
    saPath <- "Systems Advancement"
  }
  else
  {
    if (grepl("sa ", bookmarkedDirectory))
    {
      cli::cli_abort(c("x" = "Cannot detect {.file Systems Advancement} or {.file Housing for Health} folder."))
    }
  }
  
  bookmarks <- c("bnl",
                 # "bnl excel",
                 "db inputs",
                 "db outputs",
                 "downloads",
                 "echo dropbox",
                 "extracts",
                 "newest bnl",
                 "newest extract",
                 "re dropbox",
                 "sdr data",
                 "sa dropbox",
                 "sa data",
                 "sa capds",
                 "sa aging",
                 "sa connxus",
                 "sa cuc",
                 "sa hiv",
                 "sa mortality")
  
  ls_message <- function()
  {
    make_cyan <- cli::make_ansi_style("cyan")
    
    cli::cli_div(theme = list(rule = list(color = "cyan",
                                          "line-type" = "double")))
    cli::cli_rule("{.strong Input Options for {.color_orange shortcut()}}")
    cli::cli_text(c(make_cyan("{cli::symbol$info}"), " The bookmark options you can use with {.strong {.fn shortcut}} are as follows:"))
    for (i in 1:length(bookmarks))
    {
      cli::cli_bullets(c("*" = bookmarks[i]))
      rm(i)
    }
    cli::cli_end()
  }
  
  destination <- switch(bookmarkedDirectory,
                        "echo dropbox"   = echoDropbox,
                        "re dropbox"     = reDropbox,
                        "newest extract" = c(reDropbox, list("Datasets", "HMIS_Extracts")),
                        "extracts"       = c(reDropbox, list("Datasets", "HMIS_Extracts")),
                        "sdr data"       = c(reDropbox, list("Project Requests", "Small Data Requests", "data")),
                        "bnl"            = c(reDropbox, list("Datasets", "CE_BNLs")),
                        "newest bnl"     = c(reDropbox, list("Datasets", "CE_BNLs")), 
                        # "bnl excel"      = c(reDropbox, list("Project Requests", "Small Data Requests", "data", "CA BNL")),
                        "db inputs"      = c(reDropbox, list("Dashboards", "data", "ART_Inputs")),
                        "db outputs"     = c(reDropbox, list("Dashboards", "data", "generate_tables_output")),
                        "downloads"      = c(strsplit(here::here(), "/")[[1]][1:3], "Downloads"),
                        "sa dropbox"     = c(echoDropbox, list(saPath)),
                        "sa data"        = c(echoDropbox, list(saPath, "Data")),
                        "sa capds"       = c(echoDropbox, list(saPath, "Data", "CAPDS")),
                        "sa aging"       = c(echoDropbox, list(saPath, "Data", "Commission on Aging")),
                        "sa connxus"     = c(echoDropbox, list(saPath, "Data", "Connxus")),
                        "sa cuc"         = c(echoDropbox, list(saPath, "Data", "CuC - Medical Complexity and CA")),
                        "sa hiv"         = c(echoDropbox, list(saPath, "Data", "HIV-AIDS Pop")),
                        "sa mortality"   = c(echoDropbox, list(saPath, "Data", "Mortality")),
                        "ls"             = {return(ls_message())},
                        cli::cli_abort(c("x" = "Cannot determine destination from user-supplied input: {.strong {.var {bookmarkedDirectory}}}.",
                                         "!" = "{.emph To see the list of valid inputs for this function type {.strong {.field shortcut(\"ls\")}}.}",
                                         "i" = "If you are trying to reach a different destination folder than those in the list mentioned above, please type the path into the {.strong {.fn here}} function instead of using {.strong {.fn shortcut}}.")))
  
  if(bookmarkedDirectory == "newest extract")
  {
    if(lubridate::wday(lubridate::floor_date(as.Date(Sys.Date()), "month")) %in% 1:4)
    {
      pathToFile <- paste0("hmisData_", as.character((lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") - 7) + 4), ".rda")
    }
    else
    {
      pathToFile <- paste0("hmisData_", as.character(lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") + 4), ".rda")
    }
    
    return(paste0(c(destination, pathToFile), collapse = "/"))
  }
  else if (bookmarkedDirectory == "newest bnl")
  {
    pathToFile <- paste0("bnl_", lubridate::floor_date(Sys.Date(), unit = "month"), ".rda")
    
    return(paste0(c(destination, pathToFile), collapse = "/"))
  }
  else
  {
    CALL <- match.call()
    
    ifelse((length(CALL) - 2) > 0,
           return(paste0(c(destination, list(...)), collapse = "/")),
           return(paste0(c(destination), collapse = "/")))
  }
}
# shortcut0 <- function(bookmarkedDirectory, ..., dropbox_username = NULL)
# {
#   requireNamespace("here",      quietly = TRUE)
#   requireNamespace("lubridate", quietly = TRUE)
#   # requireNamespace("stringr",   quietly = TRUE)
#   
#   # where_is_here <- stringr::str_split(here::here(), "/")
#   where_is_here <- strsplit(here::here(), "/")
#   
#   # windowsUsername <- unlist(stringr::str_split(here::here(), "/"))[3]
#   windowsUsername <- unlist(strsplit(here::here(), "/"))[3]
#   
#   if (dir.exists(paste0("C:/Users/", windowsUsername, "/", "ECHO Dropbox")))
#   {
#     if (length(where_is_here[[1]]) >= 5 & unlist(where_is_here)[4] == "ECHO Dropbox")
#     {
#       # dropboxUsername <- unlist(stringr::str_split(here::here(), "/"))[5]
#       dropboxUsername <- unlist(strsplit(here::here(), "/"))[5]
#       
#       # echoDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", windowsUsername,
#       #                                                   "/ECHO Dropbox/", dropboxUsername), "/")
#       echoDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername), "/")
#       
#       # reDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", windowsUsername,
#       #                                                 "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
#       reDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
#     }
#     else
#     {
#       if (is.null(dropbox_username))
#       {
#         dropboxUsername <- windowsUsername
#       }
#       else
#       {
#         dropboxUsername <- dropbox_username
#       }
#       
#       # echoDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", windowsUsername,
#       #                                                   "/ECHO Dropbox/", dropboxUsername), "/")
#       echoDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername), "/")
#       
#       # reDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", windowsUsername,
#       #                                                 "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
#       reDropbox0 <- strsplit(paste0("C:/Users/", windowsUsername, "/ECHO Dropbox/", dropboxUsername, "/Research-and-Evaluation-Team"), "/")
#     }
#   }
#   else
#   {
#     # reDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", stringr::str_split(here::here(), "/")[[1]][3],
#     #                                                 "/Dropbox (ECHO)/Research-and-Evaluation-Team"), "/")
#     reDropbox0 <- strsplit(paste0("C:/Users/", strsplit(here::here(), "/")[[1]][3], "/Dropbox (ECHO)/Research-and-Evaluation-Team"), "/")
#     
#     # echoDropbox0 <- stringr::str_split(stringr::str_c("C:/Users/", stringr::str_split(here::here(), "/")[[1]][3],
#     #                                                   "/Dropbox (ECHO)"), "/")
#     echoDropbox0 <- strsplit(paste0("C:/Users/", strsplit(here::here(), "/")[[1]][3], "/Dropbox (ECHO)"), "/")
#   }
#   
#   reDropbox <- list()
#   
#   for (i in 1:length(reDropbox0[[1]]))
#   {
#     reDropbox[[length(reDropbox) + 1]] <- reDropbox0[[1]][i]
#     rm(i)
#   }
#   
#   rm(reDropbox0)
#   
#   echoDropbox <- list()
#   
#   for (i in 1:length(echoDropbox0[[1]]))
#   {
#     echoDropbox[[length(echoDropbox) + 1]] <- echoDropbox0[[1]][i]
#     rm(i)
#   }
#   
#   rm(echoDropbox0)
#   
#   bookmarks <- c("bnl",
#                  "bnl excel",
#                  "db inputs",
#                  "db outputs",
#                  "downloads",
#                  "echo dropbox",
#                  "extracts",
#                  "newest bnl",
#                  "newest extract",
#                  "re dropbox",
#                  "sdr data")
#   
#   if(bookmarkedDirectory %in% bookmarks)
#   {
#     destination <- switch(bookmarkedDirectory,
#                           "echo dropbox"   = echoDropbox,
#                           "re dropbox"     = reDropbox,
#                           "newest extract" = c(reDropbox, list("Datasets", "HMIS_Extracts")),
#                           "extracts"       = c(reDropbox, list("Datasets", "HMIS_Extracts")),
#                           "sdr data"       = c(reDropbox, list("Project Requests", "Small Data Requests", "data")),
#                           "bnl"            = c(reDropbox, list("Datasets", "CE_BNLs")),
#                           "newest bnl"     = c(reDropbox, list("Datasets", "CE_BNLs")), 
#                           "bnl excel"      = c(reDropbox, list("Project Requests", "Small Data Requests", "data", "CA BNL")),
#                           "db inputs"      = c(reDropbox, list("Dashboards", "data", "ART_Inputs")),
#                           "db outputs"     = c(reDropbox, list("Dashboards", "data", "generate_tables_output")),
#                           "downloads"      = c(strsplit(here::here(), "/")[[1]][1:3], "Dowloads"))
#   }
#   else if (bookmarkedDirectory == "ls")
#   {
#     ls_message <- function()
#     {
#       make_cyan <- cli::make_ansi_style("cyan")
#       
#       cli::cli_div(theme = list(rule = list(color = "cyan",
#                                             "line-type" = "double")))
#       cli::cli_rule("{.strong Input Options for {.color_orange shortcut()}}")
#       cli::cli_text(c(make_cyan("{cli::symbol$info}"), " The bookmark options you can use with {.strong {.fn shortcut}} are as follows:"))
#       for (i in 1:length(bookmarks))
#       {
#         cli::cli_bullets(c("*" = bookmarks[i]))
#         rm(i)
#       }
#       cli::cli_end()
#     }
#     
#     return(ls_message())
#   }
#   else
#   {
#     cli::cli_abort(c("x" = "Cannot determine destination from user-supplied input: {.strong {.var {bookmarkedDirectory}}}.",
#                      "!" = "{.emph To see the list of valid inputs for this function type {.strong shortcut(\"ls\")}.}",
#                      "i" = "If you are trying to reach a different destination folder than those in the list mentioned above, please type the path into the {.strong {.fn here}} function instead of using {.strong {.fn shortcut}}."))
#     
#     # stop("\n\nERROR: Cannot determine destination. The options are:\n\n
#     #      \"echo dropbox\"\n
#     #      \"re dropbox\"\n
#     #      \"sdr data\"\n
#     #      \"extracts\"\n
#     #      \"bnl\"\n
#     #      \"bnl excel\"\n
#     #      \"db inputs\"\n
#     #      \"db outputs\"\n
#     #      \"newest extract\"\n
#     #      -or-\n
#     #      \"newest bnl\"\n\n
#     #      If you are trying to reach a different destination folder please type the path into the here() function instead of shortcut().\n\n")
#   }
#   
#   if(bookmarkedDirectory == "newest extract")
#   {
#     if(lubridate::wday(lubridate::floor_date(as.Date(Sys.Date()), "month")) %in% 1:4)
#     {
#       # pathToFile <- stringr::str_c("hmisData_", as.character((lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") - 7) + 4), ".rda")
#       pathToFile <- paste0("hmisData_", as.character((lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") - 7) + 4), ".rda")
#     }
#     else
#     {
#       # pathToFile <- stringr::str_c("hmisData_", as.character(lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") + 4), ".rda")
#       pathToFile <- paste0("hmisData_", as.character(lubridate::floor_date(lubridate::floor_date(as.Date(Sys.Date()), "month"), "week") + 4), ".rda")
#     }
#     
#     return(paste0(c(destination, pathToFile), collapse = "/"))
#   }
#   else if (bookmarkedDirectory == "newest bnl")
#   {
#     # pathToFile <- stringr::str_c("bnl_",
#     #                              lubridate::floor_date(Sys.Date(), unit = "month"),
#     #                              ".rda")
#     pathToFile <- paste0("bnl_", lubridate::floor_date(Sys.Date(), unit = "month"), ".rda")
#     
#     return(paste0(c(destination, pathToFile), collapse = "/"))
#   }
#   else
#   {
#     CALL <- match.call()
#     
#     ifelse((length(CALL) - 2) > 0,
#            return(paste0(c(destination, list(...)), collapse = "/")),
#            return(paste0(c(destination), collapse = "/")))
#   }
# }
