#' Generate the CoC Quarterly Scorecards
#'
#' This function uses both the standard HMIS Export `.rda` and the  *full* HMIS
#' Export `.rda` to generate the CoC performance scorecards for the specified
#' timeframe.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract The standard HMIS CSV Export You may specify the `.rda`
#'   filepath to read, or the export variable (`list`) in your environment to
#'   use. You may also pipe the export into this function. If no export is
#'   explicitly supplied, this function will automatically use a standard export
#'   called `hmis` if there is one in the Global Environment. Otherwise it will
#'   search the Global Environment for any standard HMIS Export and use the
#'   first one it finds, warning/informing you that it did so, and telling you
#'   which one it used.
#' @param .year The year for which the scorecards are being run. This defaults
#'   to the current year. It can be set to a different year. (This input should
#'   be a number, not a string.)
#' @param .quarter The quarter (`1`, `2`, `3`, or `4`) for which the scorecards
#'   are being run. (This input should be a number, not a string.)
#' @param .hmis_extract_full The _**FULL**_ HMIS CSV Export You may specify the
#'   `.rda` filepath to read, or the export variable (`list`) in your
#'   environment to use. If no export is explicitly supplied, this function will
#'   automatically use an export called `hmis_full` if there is one in the
#'   Global Environment. Otherwise it will search the Global Environment for any
#'   full HMIS Export and use the first one it finds, warning/informing you that
#'   it did so and which one it used.
#' @param .nonHmisData The Excel spreadsheet containing any non-HMIS data used
#'   for the scorecard analysis (such as client feedback tracking & info). This
#'   defaults to the filepath to where this spreadsheet is stored in the ECHO
#'   Dropbox.
#' @param .referrals_report The path to the `CE Referrals.xlsx` file to be used
#'   for the CE Referrals analysis. This defaults to pulling the current version
#'   of the `CE Referrals.xlsx` file in the "Dashboards/data" folder in the ECHO
#'   Dropbox.
#' @param .exemptions OPTIONAL: Any project IDs that should be excluded from the
#'   scorecard analysis. (This input should be a number or vector of numbers,
#'   not a string or vector of strings.)
#' @param .straight_to_excel OPTIONAL: If set to `TRUE`, the function will open
#'   the resulting output in an Excel spreadsheet instead of just in R. Defaults
#'   to `FALSE`.
#' @param .save_result_tables OPTIONAL: If set to `TRUE`, the function will save
#'   the results tables for all metrics by project to your Downloads folder.
#'   Defaults to `FALSE`. (This will also save the full scorecard data frame
#'   that `.straight_to_excel` would generate, so it is not necessary to set
#'   both `.save_result_tables` and `.straight_to_excel` to `TRUE`.)
#' @param .show_diagnostics OPTIONAL: If set to `TRUE`, the function will print
#'   additional information to the console when runing metrics. This can help
#'   with troubleshooting errors and finding bugs. Defaults to `FALSE`.
#'
#' @return As a tibble, the CoC Scorecards table (optionally, an Excel
#'   spreadsheet if `.straight_to_excel` is set to `TRUE`) (optionally,
#'   downloading the results by metric by project if `.save_result_tables` is
#'   set to `TRUE`) for the specified `.year`/`.quarter` combination.
#' @export
run_coc_scorecards <- function(hmis_extract = NULL,
                               ...,
                               .year = as.double(format(Sys.Date(), "%Y")),
                               .quarter,
                               .hmis_extract_full = NULL,
                               .project_list = NULL,
                               .nonHmisData = NULL,
                               .referrals_report = NULL,
                               .exemptions = NULL,
                               .straight_to_excel = FALSE,
                               .save_result_tables = TRUE,
                               .show_diagnostics = FALSE#,
                               # .full_cf = NULL
)
{
  rlang::check_dots_empty()
  
  # Start Timer if Showing Diagnostics
  if(.show_diagnostics)
  {
    requireNamespace("tictoc", quietly = TRUE)
    
    tictoc::tic()
  }
  
  #### QUALITY ASSURANCE & DEPENDENCIES ################################################################################
  requireNamespace("dplyr",      quietly = TRUE)
  requireNamespace("fs",         quietly = TRUE)
  requireNamespace("glue",       quietly = TRUE)
  requireNamespace("here",       quietly = TRUE)
  requireNamespace("janitor",    quietly = TRUE)
  requireNamespace("lubridate",  quietly = TRUE)
  requireNamespace("magrittr",   quietly = TRUE)
  requireNamespace("purrr",      quietly = TRUE)
  requireNamespace("readr",      quietly = TRUE)
  requireNamespace("readxl",     quietly = TRUE)
  requireNamespace("tibble",     quietly = TRUE)
  requireNamespace("tidyr",      quietly = TRUE)
  requireNamespace("tidyselect", quietly = TRUE)
  
  if (.year > as.double(format(Sys.Date(), "%Y")))
  {
    cli::cli_abort(c("!" = "An invalid input was entered for the {.var .year} argument.",
                     "x" = "The year for which you are trying to run this function ({.val {(.year)}}) is greater than the current year ({.val {as.double(format(Sys.Date(), '%Y'))}}).",
                     "i" = "{.emph This function is not designed to predict the future...}"))
  }
  
  if (!.quarter %in% c(1:4))
  {
    cli::cli_abort(c("!" = "An invalid input was entered for the {.var .quarter} argument.",
                     "i" = "{.emph The specified quarter must be either: {.or {.val {c(1:4)}}}.}",
                     "x" = "You entered: {.val {(.quarter)}}"))
  }
  
  #### STAND BY MESSAGE ################################################################################################
  stand_by_message <- function()
  {
    cli::cli_h2("{.strong Generating CoC Scorecard for Quarter {(.quarter)} of {(.year)}}")
    cli::cli_inform(c("Please stand by...",
                      "\n "))
  }
  
  #### READ IN DATA & SETUP VARIABLES ##################################################################################
  
  # Setup Interval Time Variables ----
  operatingQuarter <- switch(.quarter,
                             "1" = lubridate::interval(lubridate::ymd(paste0(.year, "0101")),
                                                       lubridate::ymd(paste0(.year, "0331"))),
                             
                             "2" = lubridate::interval(lubridate::ymd(paste0(.year, "0401")),
                                                       lubridate::ymd(paste0(.year, "0630"))),
                             
                             "3" = lubridate::interval(lubridate::ymd(paste0(.year, "0701")),
                                                       lubridate::ymd(paste0(.year, "0930"))),
                             
                             "4" = lubridate::interval(lubridate::ymd(paste0(.year, "1001")),
                                                       lubridate::ymd(paste0(.year, "1231"))))
  
  reportingPeriod <- switch(.quarter,
                            "1" = lubridate::interval(lubridate::ymd(paste0(.year - 1, "0401")),
                                                      lubridate::ymd(paste0(.year, "0331"))),
                            
                            "2" = lubridate::interval(lubridate::ymd(paste0(.year - 1, "0701")),
                                                      lubridate::ymd(paste0(.year,     "0630"))),
                            
                            "3" = lubridate::interval(lubridate::ymd(paste0(.year - 1, "1001")),
                                                      lubridate::ymd(paste0(.year,     "0930"))),
                            
                            "4" = lubridate::interval(lubridate::ymd(paste0(.year, "0101")),
                                                      lubridate::ymd(paste0(.year, "1231"))))
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    cli::cli_h3("{.strong Timeframe Intervals: }")
    cli::cli_inform(c("i" = "{.var operatingQuarter} = {.val {lubridate::int_start(operatingQuarter)}} to {.val {lubridate::int_end(operatingQuarter)}}.",
                      "i" = "{.var reportingPeriod} = {.val {lubridate::int_start(reportingPeriod)}} to {.val {lubridate::int_end(reportingPeriod)}}.",
                      " " = "\n"))
  }
  
  # Read the Standard HMIS CSV Extract ----
  if (is.null(hmis_extract)) # This happens if no HMIS Extract is explicitly supplied.
  {
    # Check the environment for an HMIS Extract named "hmis" and use that if it's there.
    if ("hmis" %in% ls(envir = .GlobalEnv))
    {
      hmisExtract <- get("hmis", envir = .GlobalEnv)
      
      if ("exportInterval" %in% names(hmisExtract))
      {
        cli::cli_warn(c("!" = "No {.arg hmis_extract} argument was supplied to {.fn generate_coc_scorecards}.",
                        "i" = "{.emph The function found the HMIS extract called {.strong {.envvar {.val {as.name('hmis')}}}} dated {.strong {.val {lubridate::int_end(hmisExtract$exportInterval)}}} in your environment, and used that one.}"))
      }
      else
      {
        cli::cli_warn(c("!" = "No {.arg hmis_extract} argument was supplied to {.fn generate_coc_scorecards}.",
                        "i" = "{.emph The function found the HMIS extract called {.strong {.envvar {.val {as.name('hmis')}}}} dated {.strong {.val {hmisExtract$extractDate}}} in your environment, and used that one.}"))
      }
    }
    else
    {
      # Otherwise Check the Environment to see if there's another HMIS Extract, then use that and inform the user.
      is.hmis <- function(obj)
      {
        "HMIS Extract" %in% class(obj)
      }
      
      gl_env_list <- as.list(globalenv()) |>
        purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE)) |> 
        purrr::keep(\(x) ifelse(length(names(x)) == 12, TRUE, FALSE))
      
      if (!purrr::is_empty(gl_env_list))
      {
        other_hmisExtract <- names(gl_env_list)[1]
        
        hmisExtract <- get(paste0(other_hmisExtract), envir = .GlobalEnv)
        
        if ("exportInterval" %in% names(hmisExtract))
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph The function found an HMIS extract called {.strong {.envvar {.val {other_hmisExtract}}}} dated {.strong {.val {lubridate::int_end(hmisExtract$exportInterval)}}} in your environment, and used that one.}"))
        }
        else
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph The function found an HMIS extract called {.strong {.envvar {.val {other_hmisExtract}}}} dated {.strong {.val {hmisExtract$extractDate}}} in your environment, and used that one.}"))
        }
      }
      else # If there are no extracts, call `load_hmis("newest")` from the ECHO package
      {
        cli::cli_h3("{.strong HMIS Export Password Needed: }")
        cli::cli_inform(c("i" = "You will be prompted to enter a password for {.strong `load_hmis({.val {'newest'}})`} when the scorecards function loads the {.emph standard} HMIS Export.",
                          " " = "\n "))
        
        invisible(system("rundll32 user32.dll,MessageBeep -1"))
        hmisExtract <- ECHO::load_hmis("newest")
        
        if ("exportInterval" %in% names(hmisExtract))
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph  The function could not find another HMIS extract in your environment, so it called {.strong `load_hmis(\"newest\")`} and used that extract, dated {.strong {.val {lubridate::int_end(hmisExtract$exportInterval)}}}.}"))
        }
        else
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph  The function could not find another HMIS extract in your environment, so it called {.strong `load_hmis(\"newest\")`} and used that extract, dated {.strong {.val {hmisExtract$extractDate}}}.}"))
        }
      }
    }
  }
  else # This happen if an HMIS Extract 'IS' explicitly supplied.
  {
    if ("HMIS Extract" %in% class(hmis_extract)) # This happens if the input supplied to hmis_extract is an "HMIS Extract" R object.
    {
      hmisExtract <- hmis_extract
    }
    else # This happens if the input supplied to hmis_extract is a filepath.
    {
      if (grepl(".rda", hmis_extract))  # If the filepath is to an HMIS extract saved in .rda format.
      {
        hmisExtract <- ECHO::load_hmis(hmis_extract)
      }
      else # If the filepath is to a directory (folder) where the HMIS extract is saved as individual .csv files. (DEFUNCT AS OF MARCH 2024)
      {
        # The substr() function must use the CSVExtract naming convention in Small Data Requests/data
        extractDirectory <- ifelse(substr(hmis_extract, nchar(hmis_extract), nchar(hmis_extract)) == "/",
                                   hmis_extract,
                                   paste0(hmis_extract, "/"))
        
        extractDate <- lubridate::ymd(substr(hmis_extract, nchar(hmis_extract) - 7, nchar(hmis_extract)))
        
        hmisExtract <- ifelse(extractDate >= "2023-10-01",
                              ECHO::import_hmis(extractDirectory, extractDate, include_disabilities = TRUE),
                              ECHO::import_hmis(extractDirectory, extractDate, include_disabilities = TRUE, .FY = 22))
        
        rm(extractDirectory, extractDate)
      }
    }
  }
  
  if (Sys.Date() < lubridate::int_end(operatingQuarter))
  {
    if ("exportInterval" %in% names(hmisExtract))
    {
      cli::cli_warn(c("!" = "The end of the operating quarter for which you are running this function occurs in the future. Today is {.val {as.Date(Sys.Date())}}, but Quarter {.val {as.integer(.quarter)}} of {.val {as.integer(.year)}} does not end until {.val {lubridate::int_end(operatingQuarter)}}.",
                      "i" = "Also, the effective date of the HMIS data export you are using is {.val {lubridate::int_end(hmisExtract$exportInterval)}}."))
    }
    else
    {
      cli::cli_warn(c("!" = "The end of the operating quarter for which you are running this function occurs in the future. Today is {.val {as.Date(Sys.Date())}}, but Quarter {.val {as.integer(.quarter)}} of {.val {as.integer(.year)}} does not end until {.val {lubridate::int_end(operatingQuarter)}}.",
                      "i" = "Also, the effective date of the HMIS data export you are using is {.val {hmisExtract$extractDate}}."))
    }
  }
  if ("exportInterval" %in% names(hmisExtract))
  {
    if (lubridate::int_end(hmisExtract$exportInterval) < lubridate::int_end(operatingQuarter))
    {
      cli::cli_warn(c("!" = "The effective date of the {.emph Standard} HMIS data export you are using ({.val {lubridate::int_end(hmisExtract$exportInterval)}}) is before the end of the operating quarter ({.val {lubridate::int_end(operatingQuarter)}}).",
                      "i" = "In order to have all the data needed to accurately run the scorecards, you should use an HMIS data export run {.emph after} the end date of the quarter."))
    }
  }
  else
  {
    if (hmisExtract$extractDate < lubridate::int_end(operatingQuarter))
    {
      cli::cli_warn(c("!" = "The effective date of the {.emph Standard} HMIS data export you are using ({.val {hmisExtract$extractDate}}) is before the end of the operating quarter ({.val {lubridate::int_end(operatingQuarter)}}).",
                      "i" = "In order to have all the data needed to accurately run the scorecards, you should use an HMIS data export run {.emph after} the end date of the quarter."))
    }
  }
  
  # Read the "FULL" HMIS CSV Extract ----
  if (is.null(.hmis_extract_full)) # This happens if no "FULL" HMIS Extract is explicitly supplied.
  {
    # Check the environment for an HMIS Extract named "hmis_full" and use that if it's there.
    if ("hmis_full" %in% ls(envir = .GlobalEnv))
    {
      message("hmis_full in GlobalEnv, getting it...")
      hmisExtract_full <- get("hmis_full", envir = .GlobalEnv)
      
      cli::cli_warn(c("!" = "No {.arg .hmis_extract_full} argument was supplied to {.fn generate_coc_scorecards}.",
                      "i" = "{.emph The function used the Full HMIS extract called {.strong {.envvar {.val {as.name('hmis_full')}}}} dated {.strong {.val {hmisExtract_full$ExportDate}}} in your environment, and used that one.}"))
    }
    else
    {
      # Otherwise Check the Environment to see if there's another full HMIS Extract, then use that and inform the user.
      is.hmis <- function(obj)
      {
        "HMIS Extract" %in% class(obj)
      }
      
      gl_env_list <- as.list(globalenv()) |>
        purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE)) |> 
        purrr::keep(\(x) ifelse(length(names(x)) == 26, TRUE, FALSE))
      
      if (!purrr::is_empty(gl_env_list))
      {
        other_hmis_extract <- names(gl_env_list)[1]
        message(paste0("envlist not empty, getting: ", other_hmis_extract))
        
        hmisExtract_full <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
        
        cli::cli_warn(c("!" = "{.strong {.fn run_coc_scorecards} could not find an object named {.envvar hmis_full}. (If a {.emph FULL} HMIS extract is loaded in your environment as {.envvar hmis_full} then this issue will autoresolve.)}",
                        "i" = "{.emph {.fn run_coc_scorecards} found a {.strong full} HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      }
      else
      {
        cli::cli_h3("{.strong HMIS Export Password Needed: }")
        cli::cli_inform(c("i" = "You will be prompted to enter a password for {.strong `load_hmis({.val {'newest'}}, .use_full_extract = {.val {TRUE}})`} when the scorecards function loads the {.emph full} HMIS Export.",
                          " " = "{.strong NOTE:} {.emph (Decrypting the full HMIS Export may take a little longer than decypting the normal HMIS Export.)}",
                          " " = "\n "))
        
        invisible(system("rundll32 user32.dll,MessageBeep -1"))
        hmisExtract_full <- load_hmis("newest", .use_full_extract = TRUE)
        
        if ("ExportInterval" %in% names(hmisExtract_full))
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract_full} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph  The function could not find another {.strong full} HMIS extract in your environment, so it called {.strong `load_hmis(\"newest\", .use_full_extract = {.val {TRUE}})`} and used that extract, dated {.strong {.val {lubridate::int_end(hmisExtract_full$ExportInterval)}}}.}"))
        }
        else
        {
          cli::cli_warn(c("!" = "{.strong NOTE:} No {.arg hmis_extract_full} argument was supplied to {.strong {.fn generate_coc_scorecards}}.",
                          "i" = "{.emph  The function could not find another {.strong full} HMIS extract in your environment, so it called {.strong `load_hmis(\"newest\", .use_full_extract = {.val {TRUE}})`} and used that extract, dated {.strong {.val {hmisExtract_full$ExportDate}}}.}"))
        }
      }
    }
  }
  else
  {
    hmisExtract_full <- .hmis_extract_full
  }
  
  if ("ExportInterval" %in% names(hmisExtract_full))
  {
    if (lubridate::int_end(hmisExtract_full$ExportInterval) < lubridate::int_end(operatingQuarter))
    {
      cli::cli_warn(c("!" = "The effective date of the {.emph Full} HMIS data export you are using ({.val {lubridate::int_end(hmisExtract_full$ExportInterval)}}) is before the end of the operating quarter ({.val {lubridate::int_end(operatingQuarter)}}).",
                      "i" = "In order to have all the data needed to accurately run the scorecards, you should use an HMIS data export run {.emph after} the end date of the quarter."))
    }
  }
  else
  {
    if (hmisExtract_full$ExportDate < lubridate::int_end(operatingQuarter))
    {
      cli::cli_warn(c("!" = "The effective date of the {.emph Full} HMIS data export you are using ({.val {hmisExtract_full$ExportDate}}) is before the end of the operating quarter ({.val {lubridate::int_end(operatingQuarter)}}).",
                      "i" = "In order to have all the data needed to accurately run the scorecards, you should use an HMIS data export run {.emph after} the end date of the quarter."))
    }
  }
  
  # Create a Vector With All CoC Project IDs, Minus Any in `.exemptions` ----
  if (is.null(.project_list))
  {
    cocProjects <- suppressWarnings(c(which(!these_project("groups", c("coc", "yhdp"), hmis_extract = hmisExtract, active_only = TRUE) %in% .exemptions)) |>
                                      vapply(\(.) these_project("groups", c("coc", "yhdp"), hmis_extract = hmisExtract, active_only = TRUE)[.], FUN.VALUE = double(1)))
  }
  else
  {
    cocProjects <- c(which(!.project_list %in% .exemptions)) |>
      vapply(\(.) .project_list[.], FUN.VALUE = double(1))
  }
  
  # If the Lifeworks RRH Collab. `ProjectID` is detected, ensure that the Caritas and SAFE RRH Collab. `Project ID`s are included as well
  if (9499 %in% cocProjects)
  {
    if (!9504 %in% cocProjects)
    {
      cocProjects <- c(cocProjects, 9504)
      
      # Diagnostic message
      if (.show_diagnostics)
      {
        cli::cli_h3("{.strong CoC Projects List: }")
        cli::cli_inform(c("i" = "The {.field ProjectID} {.val {9499}} ({.val {this_project('name', 9499, hmis_extract = hmisExtract)}}) was detected in the function variable {.var cocProjects}, but the {.field ProjectID} {.val {9504}} ({.val {this_project('name', 9504, hmis_extract = hmisExtract)}}) was not, and has been added to {.var cocProjects}.",
                          " " = "\n"))
      }
    }
    
    if (!9516 %in% cocProjects)
    {
      cocProjects <- c(cocProjects, 9516)
      
      # Diagnostic message
      if (.show_diagnostics)
      {
        cli::cli_h3("{.strong CoC Projects List: }")
        cli::cli_inform(c("i" = "The {.field ProjectID} {.val {9499}} ({.val {this_project('name', 9499, hmis_extract = hmisExtract)}}) was detected in the function variable {.var cocProjects}, but the {.field ProjectID} {.val {9516}} ({.val {this_project('name', 9516, hmis_extract = hmisExtract)}}) was not, and has been added to {.var cocProjects}.",
                          " " = "\n"))
      }
    }
  }
  
  # Create "Project Score" & "PIP" Variables for Each Project in the Upcoming Data Frame ----
  `Project Score` <- rep(0, times = length(cocProjects))
  PIP <- rep(FALSE, times = length(cocProjects))
  VSP <- rep(FALSE, times = length(cocProjects))
  
  # Create Variables for Each Scorecard Measure as Well as a Way to Access the Metric Names ----
  scorecard_measures <- function(flag = NULL)
  {
    valid_flags <- c("create", "access")
    
    if (is.null(flag))
    {
      flag <- "access"
    }
    
    error_msg <- function()
    {
      cli::cli_abort(c("!" = "The supplied {.arg flag} argument is invalid.",
                       "i" = "It must be: {.val {valid_flags[1]}} or {.cls {typeof(NULL)}}.",
                       "x" = "You entered: {.val {flag}}."))
    }
    
    if (!flag %in% valid_flags)
    {
      error_msg()
    }
    
    parent_frame <- parent.frame()
    
    create_measures <- function()
    {
      # Performance Monitoring
      for (n in 1:5)
      { 
        assign(paste0("Metric PM-", n), rep(0, times = length(cocProjects)), pos = parent_frame)
        rm(n)
      }
      
      # Racial Equity
      for (n in 1:7) 
      { 
        assign(paste0("Metric RE-", n), rep(0, times = length(cocProjects)), pos = parent_frame)
        rm(n)
      }
      
      # Client Feedback
      for (n in 1:2) 
      { 
        assign(paste0("Metric CF-", n), rep(0, times = length(cocProjects)), pos = parent_frame)
        rm(n)
      }
      
      # Data Quality
      for (n in 1:4) 
      { 
        assign(paste0("Metric DQ-", n), rep(0, times = length(cocProjects)), pos = parent_frame)
        rm(n)
      }
    }
    
    access_measures <- function()
    {
      measures <- NULL
      
      # Performance Monitoring
      for (n in 1:5)
      { 
        measures <- c(measures, paste0("Metric PM-", n))
        rm(n)
      }
      
      # Racial Equity
      for (n in 1:7) 
      { 
        measures <- c(measures, paste0("Metric RE-", n))
        rm(n)
      }
      
      # Client Feedback
      for (n in 1:2) 
      { 
        measures <- c(measures, paste0("Metric CF-", n))
        rm(n)
      }
      
      # Data Quality
      for (n in 1:4) 
      { 
        measures <- c(measures, paste0("Metric DQ-", n))
        rm(n)
      }
      
      # DQ 2, 3, and 4 should be 2a, 2b, and 3
      measures[16] <- "Metric DQ-2a"
      measures[17] <- "Metric DQ-2b"
      measures[18] <- "Metric DQ-3"
      
      return(measures)
    }
    
    return(switch(flag,
                  "create" = create_measures(),
                  "access" = access_measures(),
                  error_msg()))
  }
  
  scorecard_measures("create")
  
  # Compile All Variables for the Scorecard Data Frame Into a List & Discard the Other Variables (i.e. time intervals) From That List ----
  scorecardItems <- as.list(environment()) |>
    purrr::discard(\(x) ifelse(is.list(x) | is.interval(x) | is.function(x) | is.null(x), TRUE, FALSE))
  
  # Set Up the Scorecard Data Frame ----
  # (NOTE: Do not ungroup!!! Continued rowwise operations are necessary through the end of the function up until return.)
  scorecards <- tibble::as_tibble(rev(scorecardItems)) |>
    dplyr::rowwise() |>
    dplyr::mutate(`Project Name` = ECHO::this_project("name", cocProjects, hmis_extract = hmisExtract),
                  VSP = ECHO::is_vsp(cocProjects, hmis_extract = hmisExtract)) |>
    dplyr::rename(`Project ID`   = cocProjects,
                  `Metric DQ-2a` = "Metric DQ-2",
                  `Metric DQ-2b` = "Metric DQ-3",
                  `Metric DQ-3`  = "Metric DQ-4",) |>
    dplyr::relocate(`Project Name`, .before = `Project ID`) |>
    dplyr::relocate(`Project Score`:PIP, .after = `Metric DQ-4`)
  
  # Get Rid of Unneeded Items (Not Strictly Necessary)
  rm(`Project Score`, PIP, scorecardItems)
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    diagnostic_initial_scorecards_template <<- scorecards
    
    cli::cli_h3("{.strong Scorecard Data Frame: }")
    cli::cli_inform(c("i" = "The blank scorecards template {.var scorecards} has been created within {.fn run_coc_scorecards} and has been exported in its initial form to your global environment as {.envvar diagnostic_initial_scorecards_template}.",
                      " " = "\n"))
  }
  
  # Setup Non-Interval Time Variables ----
  # (Must do this, and following variable setup, AFTER generating the scorecard data frame or they will be included with the
  # scorecard metrics in the scorecard data frame...)
  cocLookbackStopDate <- as.Date("2012-10-01")
  
  # Create a Vector of PH Destination Type CSV Codes, Post 10/1/23  ----
  phDestinationCodes <- HUD_LivingSituations_Destinations_SubsidyTypes_FY24 |>
    dplyr::filter(Classification == "Permanent Housing Situations") |>
    dplyr::pull(Value)
  
  # Create a Vector of Excluded Destination CSV Codes, Post 10/1/23  ----
  excludedDestinationCodes <- c(24, 206, 215, 225)
  
  #### SETUP SAVE DIRECTORIES, IF SELECTED #############################################################################
  if (.save_result_tables)
  {
    results_directory <- shortcut("downloads", paste0("coc_scorecards_", .year, "_q", .quarter))
    
    make_subdirs <- function()
    {
      # Performance Monitoring
      for (n in 1:5)
      { 
        dir.create(paste0(results_directory, "/0", n, "_Metric-PM", n))
        rm(n)
      }
      
      # Racial Equity
      for (n in 1:7) 
      { 
        ifelse(n + 5 < 10,
               dir.create(paste0(results_directory, "/0", n + 5, "_Metric-RE", n)),
               dir.create(paste0(results_directory, "/", n + 5, "_Metric-RE", n)))
        rm(n)
      }
      
      # Client Feedback
      for (n in 1:2) 
      { 
        dir.create(paste0(results_directory, "/", n + 12, "_Metric-CF", n))
        rm(n)
      }
      
      # Data Quality
      for (n in 1:4) 
      { 
        switch (as.character(n),
                "1" = dir.create(paste0(results_directory, "/", n + 14, "_Metric-DQ", n)),
                "2" = dir.create(paste0(results_directory, "/", n + 14, "_Metric-DQ2a")),
                "3" = dir.create(paste0(results_directory, "/", n + 14, "_Metric-DQ2b")),
                "4" = dir.create(paste0(results_directory, "/", n + 14, "_Metric-DQ3")))
        
        rm(n)
      }
    }
    
    download_results <- function(save_data)
    {
      proj_id <- get("project_id", pos = parent.frame())
      
      metric_sub_dir <- get("metric_subdirectory", pos = parent.frame())
      
      save_dir <- fs::dir_ls(results_directory) |> 
        purrr::discard(\(x) !grepl(metric_sub_dir, x))
      
      readr::write_csv(save_data,
                       paste0(save_dir, "/", this_project("name", proj_id, hmis_extract = hmisExtract), " (", proj_id, ").csv"))
    }
    
    if (!dir.exists(results_directory))
    {
      dir.create(results_directory)
      make_subdirs()
    }
    else
    {
      get_key_msg <- function()
      {
        cli::cli_div()
        cli::cli_h2("\nUSER INPUT REQUIRED")
        cli::cli_alert_warning("The directory {.path {results_directory}} already exists. If you wish to overwrite it, type {.key Y} and press {.key ENTER}. Otherwise, type anything else and press {.key ENTER}.")
        cli::cli_text("\n")
        cli::cli_end()
        readline(prompt = "INPUT> ")
      }
      
      user_keypress <- get_key_msg()
      
      if (user_keypress == "Y")
      {
        unlink(results_directory, recursive = TRUE)
        
        dir.create(results_directory)
        
        make_subdirs()
      }
      else
      {
        cli::cli_text("\n ")
        cli::cli_abort(c("!" = "Aborting.",
                         "i" = "Either move/rename the original folder, or run this function without {.arg .save_result_tables} set to {.val {TRUE}}.",
                         "x" = "The directory {.path results_directory} already exists and you have declined to overwwrite it."))
      }
    }
  }
  
  stand_by_message()
  
  #### Read in Non-HMIS Data from the Manual Excel Sheet Used for That #################################################
  # (Make sure the current year is sheet 1, and that previous years are sequential after that!)
  if (is.null(.nonHmisData))
  {
    .nonHmisData <- readxl::read_excel(shortcut("echo dropbox",
                                                "Quarterly Performance Scorecards",
                                                "Submission Tracking",
                                                "CoC_Scorecard_Submission_Tracking_Worksheet.xlsx"),
                                       sheet = ifelse(as.double(format(Sys.Date(), "%Y")) == .year,
                                                      1,
                                                      1 + as.double(format(Sys.Date(), "%Y")) - .year)) |>
      dplyr::filter(Quarter == .quarter) |>
      dplyr::select(-ProjectName, -Quarter, -`SurveyIncorporation(CF-2)`)
    
    # Diagnostic message
    if (.show_diagnostics)
    {
      cli::cli_h3("{.strong Non-HMIS Data Excel Sheet: }")
      cli::cli_inform(c("i" = "Using {.path {shortcut('echo dropbox', 'Quarterly Performance Scorecards', 'Submission Tracking', 'CoC_Scorecard_Submission_Tracking_Worksheet.xlsx')}} sheet {.val {ifelse(as.double(format(Sys.Date(), '%Y')) == .year, 1, 1 + as.double(format(Sys.Date(), '%Y')) - .year)}} for {.arg .nonHmisData}.",
                        " " = "\n"))
    }
  }
  
  if (is.null(.referrals_report))
  {
    accepted_referrals <- readxl::read_excel(shortcut("db inputs", "../", "CAReferrals", "CE Referrals.xlsx"))
  }
  
  #### CREATE SCORECARD UNIVERSE AND OTHER DATA FRAMES #################################################################
  # Scorecard Universe ----
  scorecard_universe <- hmisExtract$entry |>
    dplyr::left_join(hmisExtract$exit,    by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(hmisExtract$client,  by = c("PersonalID")) |>
    dplyr::left_join(hmisExtract$project, by = c("ProjectID")) |>
    dplyr::filter(ProjectID %in% cocProjects,
                  EntryDate <= lubridate::int_end(reportingPeriod),
                  is.na(ExitDate) | ExitDate >= lubridate::int_start(reportingPeriod)) |>
    dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < EntryDate, NA, MoveInDate))
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    diagnostic_scorecard_universe <<- scorecard_universe
    
    cli::cli_h3("{.strong Scorecard Universe: }")
    cli::cli_inform(c("i" = "Exporting function variable {.var scorecard_universe} to your global environment as {.envvar diagnostic_scorecard_universe}.",
                      " " = "\n"))
  }
  
  # Accepted Referrals ----
  accepted_referrals <- accepted_referrals |>
    dplyr::rename("ProjectType" = `Need Code Description`,
                  "ProjectName" = `Service Referto Provider`,
                  "PersonalID"  = `Client Uid`) |>
    dplyr::mutate(dplyr::across(dplyr::contains("Date"), as.Date),
                  ProjectType = dplyr::case_match(ProjectType,
                                                  "Homeless Permanent Supportive Housing" ~ suppressWarnings(this_project("type", "psh", hmis_extract = hmisExtract)),
                                                  "Residential Housing Options" ~ suppressWarnings(this_project("type", "rrh", hmis_extract = hmisExtract)),
                                                  .default = NA)) |>
    split_project_name_id(ProjectName) |>
    dplyr::filter(ProjectID %in% cocProjects) |>
    dplyr::filter(lubridate::`%within%`(`Service Refer Date`, reportingPeriod),
                  `Service Refer Outcome` == "Accepted") |>
    dplyr::left_join(scorecard_universe, by = c("PersonalID", "ProjectType", "ProjectName", "ProjectID"))
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    diagnostic_accepted_referrals <<- accepted_referrals
    
    if (is.null(.referrals_report))
    {
      cli::cli_h3("{.strong Accepted Referrals Data Frame: }")
      cli::cli_inform(c("i" = "Using {.path {shortcut('db inputs', '../', 'CAReferrals', 'CE Referrals.xlsx')}} for function variable {.var accepted_referrals}. The data frame has been exported to you global environment as {.envvar diagnostic_accepted_referrals}.",
                        " " = "\n"))
    }
    else
    {
      cli::cli_h3("{.strong Accepted Referrals Data Frame: }")
      cli::cli_inform(c("i" = "Using {.path {(.referrals_report)}} for function variable {.var accepted_referrals}. The data frame has been exported to you global environment as {.envvar diagnostic_accepted_referrals}.",
                        " " = "\n"))
    }
  }
  
  # Full Income Data ----
  # Read Full Income Benefits File (hmisExtract$incomebenefits does not have everything we need...)
  income_data_full <- hmisExtract_full$IncomeBenefits.csv
  
  # Full Race & Ethnicty Data ----
  # Race/Ethnicity Data by SPID
  race_ethnicity_full <- hmisExtract$client |>
    dplyr::select(PersonalID, Race_Ethnicity) |>
    dplyr::mutate(Race_Ethnicity = dplyr::if_else(Race_Ethnicity == "Data not collected", NA_character_, Race_Ethnicity))
  # dplyr::mutate(Race_Ethnicity = dplyr::if_else(Race_Ethnicity == "Data not collected", NA, Race_Ethnicity)) # BC this is `dplyr::if_else` using `NA` instead of `NA_character` will keep `Race_Ethnicity` as a factor
  
  # All CE Project Entries ----
  # CE Entries for Cross Reference when Missing HOH Variable in Referrals Report
  ce_entries_full <- hmisExtract$entry |>
    dplyr::filter(ProjectID %in% hmisExtract$project$ProjectID[which(hmisExtract$project[["ProjectType"]] == suppressWarnings(this_project("type", "ce", hmis_extract = hmisExtract)))])
  
  # Create Returns Window ----
  # Timeframe and Data for Calculating Returns Per LC Approved Language
  returns_lookback <- lubridate::interval((lubridate::int_start(operatingQuarter) - months(6, abbreviate = FALSE)) - lubridate::years(1),
                                          lubridate::int_start(operatingQuarter) - months(6, abbreviate = FALSE))
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    cli::cli_h3("{.strong Returns Lookback Interval: }")
    cli::cli_inform(c("i" = "{.var returns_lookback} = {.val {lubridate::int_start(returns_lookback)}} to {.val {lubridate::int_end(returns_lookback)}}.",
                      " " = "\n"))
  }
  
  # Create Returns Data ----
  coc_exits <- hmisExtract$entry |>
    dplyr::left_join(hmisExtract$exit,    by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(hmisExtract$client,  by = c("PersonalID")) |>
    dplyr::left_join(hmisExtract$project, by = c("ProjectID")) |>
    dplyr::filter(ProjectID %in% cocProjects,
                  lubridate::`%within%`(ExitDate, returns_lookback),
                  HOH == 1,
                  Destination %in% phDestinationCodes)
  
  # Diagnostic message
  if (.show_diagnostics)
  {
    diagnostic_coc_exits <<- coc_exits
    
    cli::cli_h3("{.strong CoC Returns Data Frame: }")
    cli::cli_inform(c("i" = "The function variable {.var coc_exits} (used for calculating returns) has been created and exported to your global environmnt as {.envvar diagnostic_coc_exits}.",
                      " " = "\n"))
  }
  
  # Create Disabilities Data ----
  coc_disabilities <- hmisExtract_full$Disabilities.csv |>
    dplyr::filter(InformationDate <= lubridate::int_end(operatingQuarter)) |> 
    dplyr::mutate(DataCollectionStage = ifelse(DataCollectionStage == 3, 666, DataCollectionStage)) |> 
    dplyr::arrange(PersonalID, EnrollmentID, dplyr::desc(DataCollectionStage), dplyr::desc(DateCreated)) |> 
    dplyr::filter(DataCollectionStage == max(DataCollectionStage), .by = c("PersonalID", "EnrollmentID")) |>
    dplyr::mutate(DataCollectionStage = ifelse(DataCollectionStage == 666, 3, DataCollectionStage)) |> 
    dplyr::mutate(Disability_Label = dplyr::case_match(DisabilityType,
                                                       5 ~ "Physical",
                                                       6 ~ "Developmental",
                                                       7 ~ "Chronic Health",
                                                       8 ~ "HIV/AIDS",
                                                       9 ~ "Mental Health",
                                                       10 ~ "Substance Use")) |> 
    dplyr::select(PersonalID, EnrollmentID, Disability_Label, DisabilityResponse, DataCollectionStage, DateCreated, DateUpdated) |> 
    dplyr::arrange(PersonalID, EnrollmentID, Disability_Label, dplyr::desc(DateCreated)) |> 
    dplyr::distinct(PersonalID, EnrollmentID, Disability_Label, .keep_all = TRUE) |> 
    tidyr::pivot_wider(names_from = Disability_Label,
                       values_from = DisabilityResponse) |>
    dplyr::left_join(hmisExtract_full$Disabilities.csv |>
                       dplyr::filter(InformationDate <= lubridate::int_end(operatingQuarter)) |> 
                       dplyr::mutate(DataCollectionStage = ifelse(DataCollectionStage == 3, 666, DataCollectionStage)) |> 
                       dplyr::arrange(PersonalID, EnrollmentID, dplyr::desc(DataCollectionStage), dplyr::desc(DateCreated)) |> 
                       dplyr::filter(DataCollectionStage == max(DataCollectionStage), .by = c("PersonalID", "EnrollmentID")) |>
                       dplyr::mutate(DataCollectionStage = ifelse(DataCollectionStage == 666, 3, DataCollectionStage)) |> 
                       dplyr::mutate(Impairment_Label = dplyr::case_match(DisabilityType,
                                                                          5 ~ "Physical Indefinite Impairment",
                                                                          6 ~ "Developmental Indefinite Impairment",
                                                                          7 ~ "Chronic Health Indefinite Impairment",
                                                                          8 ~ "HIV/AIDS Indefinite Impairment",
                                                                          9 ~ "Mental Health Indefinite Impairment",
                                                                          10 ~ "Substance Use Indefinite Impairment")) |> 
                       dplyr::select(PersonalID, EnrollmentID, Impairment_Label, IndefiniteAndImpairs, DataCollectionStage, DateCreated, DateUpdated) |> 
                       dplyr::arrange(PersonalID, EnrollmentID, Impairment_Label, dplyr::desc(DateCreated)) |> 
                       dplyr::distinct(PersonalID, EnrollmentID, Impairment_Label, .keep_all = TRUE) |> 
                       tidyr::pivot_wider(names_from = Impairment_Label,
                                          values_from = IndefiniteAndImpairs),
                     by = c("PersonalID", "EnrollmentID", "DataCollectionStage", "DateCreated", "DateUpdated")) |> 
    dplyr::relocate(DataCollectionStage:DateUpdated, .after = dplyr::last_col()) |> 
    dplyr::rename_with(\(x) paste0(x, "_Disabilities.csv"), .cols = tidyselect::contains("Date")) |> 
    dplyr::mutate(dplyr::across(tidyselect::contains("Date"), as.Date))
  
  
  # Decrypt Data in Client.csv ----
  # if (is.null(.full_cf))
  # {
  cli::cli_h3("{.strong Passwords Needed for Encrypted Data Columns: }")
  cli::cli_inform(c("i" = "Three neccesary data columns in {.val Client.csv} in the {.emph full} HMIS Export are encrypted. You will be prompted to enter a password shortly.",
                    "!" = "You will then need to enter the password {.emph twice more} in order to decrypt all {.strong {.val {3}}} total encrypted columns of data!",
                    " " = "{.strong NOTE:} {.emph (Decrypting data in the client file may take some time, potentially a minute or so each.)}",
                    " " = "\n ",
                    " " = "{.emph Please stand by...}",
                    " " = "\n "))
  
  invisible(system("rundll32 user32.dll,MessageBeep -1"))
  
  unlocked_cl_file <- hmisExtract_full$Client.csv |> 
    encryptr::decrypt(SSN,
                      private_key_path = paste0(paste(strsplit(here(), "/")[[1]][1:3], collapse = "/"), "/files/id_rsa_d"))
  
  invisible(system("rundll32 user32.dll,MessageBeep -1"))
  
  unlocked_cl_file <- hmisExtract_full$Client.csv |> 
    encryptr::decrypt(FirstName,
                      private_key_path = paste0(paste(strsplit(here(), "/")[[1]][1:3], collapse = "/"), "/files/id_rsa_d"))
  
  invisible(system("rundll32 user32.dll,MessageBeep -1"))
  
  unlocked_cl_file <- hmisExtract_full$Client.csv |> 
    encryptr::decrypt(LastName,
                      private_key_path = paste0(paste(strsplit(here(), "/")[[1]][1:3], collapse = "/"), "/files/id_rsa_d"))
  # }
  # else 
  # {
  #   unlocked_cl_file <- .full_cf
  # }
  
  # Create Data Universe ----
  data_universe_complete <- dplyr::rename(hmisExtract_full$Enrollment.csv,
                                          DateCreated_Enrollment.csv = DateCreated,
                                          DateUpdated_Enrollment.csv = DateUpdated) |>
    dplyr::left_join(dplyr::rename(hmisExtract_full$Exit.csv,
                                   DateCreated_Exit.csv = DateCreated,
                                   DateUpdated_Exit.csv = DateUpdated), by = c("EnrollmentID", "PersonalID")) |> 
    dplyr::left_join(dplyr::left_join(dplyr::rename(unlocked_cl_file,
                                                    DateCreated_Client.csv = DateCreated,
                                                    DateUpdated_Client.csv = DateUpdated), dplyr::select(hmisExtract$client, PersonalID, Age, Gender, Race_Ethnicity), by = "PersonalID"), by = "PersonalID") |>
    dplyr::mutate(SSN = dplyr::case_when(SSNDataQuality == 99 ~ FALSE,
                                         is.na(SSN) ~ FALSE,
                                         .default = TRUE)) |>
    dplyr::left_join(coc_disabilities, by = c("PersonalID", "EnrollmentID")) |> 
    dplyr::filter(ProjectID %in% cocProjects) |> 
    dplyr::filter(lubridate::`%within%`(EntryDate, operatingQuarter)
                  | lubridate::`%within%`(DateCreated_Enrollment.csv, operatingQuarter)
                  | lubridate::`%within%`(DateUpdated_Enrollment.csv, operatingQuarter)
                  | lubridate::`%within%`(DateCreated_Exit.csv, operatingQuarter)
                  | lubridate::`%within%`(DateUpdated_Exit.csv, operatingQuarter)
                  | lubridate::`%within%`(DateCreated_Client.csv, operatingQuarter)
                  | lubridate::`%within%`(DateUpdated_Client.csv, operatingQuarter)
                  | lubridate::`%within%`(ExitDate, operatingQuarter),
                  any(lubridate::`%within%`(c(EntryDate, ExitDate), operatingQuarter)))
  # Add this last part of the filter for Client is currently enrolled during operating quarter to avoid catching updates for clients who do not
  # belong in this data frame
  
  #### UTILITY FUNCTIONS ###############################################################################################
  set_scores <- function(..., tiers, thresholds, points)
  {
    rlang::check_dots_empty()
    
    if (length(thresholds) != tiers)
    {
      cli::cli_abort(c("!" = "{.strong An invalid input was entered for the {.arg thresholds} argument.}",
                       "i" = "{.emph The number of items in {.arg thresholds} must match the number of {.arg tiers}.}",
                       "x" = "You entered {.strong {.val {length(thresholds)}}} item{?s} in {.arg thresholds}, for {.strong {.val {tiers}}} tier{?s}."))
    }
    
    if (length(points) != tiers)
    {
      cli::cli_abort(c("!" = "{.strong An invalid input was entered for the {.arg points} argument.}",
                       "i" = "{.emph The number of items in {.arg points} must match the number of {.arg tiers}.}",
                       "x" = "You entered {.strong {.val {length(points)}}} item{?s} in {.arg points}, for {.strong {.val {tiers}}} tier{?s}."))
    }
    
    return(data.frame("Tier"      = 1:tiers,
                      "Threshold" = thresholds,
                      "Points"    = points))
  }
  
  percent <- function(.percent)
  {
    return(.percent / 100)
  }
  
  round_as_percent <- function(.decimal, as_decimal = FALSE)
  {
    ifelse(!as_decimal,
           round(.decimal * 100),
           percent(round(.decimal * 100)))
  }
  
  one_percent_above <- function(x)
  {
    x + 0.01
  }
  
  one_percent_below <- function(x)
  {
    x - 0.01
  }
  
  fill_in_race_ethnicity <- function(spid)
  {
    vapply(spid, \(x) race_ethnicity_full$Race_Ethnicity[which(race_ethnicity_full$PersonalID == x)], FUN.VALUE = character(1))
  }
  
  fill_in_hoh <- function(.data)
  {
    project_ce_entries <- dplyr::filter(ce_entries_full, PersonalID %in% .data$PersonalID)
    
    get_hoh_info <- function(spid)
    {
      referral_date <- .data$`Service Refer Date`[which(.data$PersonalID == spid)]
      
      ca_pre_referral <- project_ce_entries |>
        dplyr::filter(PersonalID == spid) |>
        dplyr::mutate(ca_to_referral_time = referral_date - EntryDate) |>
        dplyr::filter(ca_to_referral_time >= 0) |>
        dplyr::arrange(-dplyr::desc(ca_to_referral_time)) |>
        dplyr::distinct(PersonalID, .keep_all = TRUE)
      
      if (dplyr::tally(tibble::as_tibble(ca_pre_referral)) > 0)
      {
        return(ca_pre_referral$HOH)
      }
      else
      {
        return(NA_real_)
      }
    }
    
    return(.data |> dplyr::mutate(HOH = dplyr::if_else(is.na(HOH),
                                                       vapply(.data$PersonalID, get_hoh_info, FUN.VALUE = double(1)),
                                                       HOH)))
  }
  
  add_latest_annual_assessment <- function(.data)
  {
    dplyr::mutate(.data, Latest.Annual.Assessment.Due = as.Date(paste0(lubridate::year(lubridate::int_end(operatingQuarter)),
                                                                       "-",
                                                                       lubridate::month(EntryDate),
                                                                       "-",
                                                                       lubridate::mday(EntryDate)),
                                                                format = "%Y-%m-%d")) |>
      dplyr::mutate(Latest.Annual.Assessment.Due = dplyr::case_when(Latest.Annual.Assessment.Due == EntryDate ~ NA_Date_,
                                                                    .default = Latest.Annual.Assessment.Due))
  }
  
  return_na <- function(msg = get("na_message", pos = parent.frame()), fn_mode = get("function_mode", pos = parent.frame()))
  {
    # When this function is called, it will return the most recently defined `na_message` variable within the scope where the function is being
    # used. In general, you should probably define `na_message` right before each time this function is used.
    
    switch(fn_mode,
           "Text Results" = msg,
           "Numeric Scores" = NA_real_)
  }
  
  na_type <- function(fn_mode = get("function_mode", pos = parent.frame()))
  {
    switch(fn_mode,
           "Text Results" = NA_character_,
           "Numeric Scores" = NA_real_)
  }
  
  type_to_return <- function(fn_mode = get("function_mode", pos = parent.frame()))
  {
    switch(fn_mode,
           "Text Results" = character(1),
           "Numeric Scores" = double(1))
  }
  
  vsp_check <- function(p_id)
  {
    ifelse(p_id %in% c(9605, 9271, 9516), TRUE, FALSE)
  }
  
  # Define these outside the function below so they are only created once and can be used as needed
  sheet_3 <- c("Metric-PM2", "Metric-RE3", "Metric-RE4")
  sheet_4 <- c("Metric-PM3", "Metric-RE5")
  sheet_5 <- c("Metric-PM5", "Metric-RE7")
  sheet_6 <- c("Metric-DQ1")
  sheet_7 <- c("Metric-DQ2a")
  sheet_8 <- c("Metric-DQ2b")
  
  read_safe_data <- function(project, ..., supplemental = FALSE, current_metric = get("metric_subdirectory", pos = parent.frame()))
  {
    rlang::check_dots_empty()
    
    if (!supplemental)
    {
      sheet_number <- 1
    }
    else
    {
      sheet_number <- dplyr::case_when(current_metric %in% sheet_3 ~ 3,
                                       current_metric %in% sheet_4 ~ 4,
                                       current_metric %in% sheet_5 ~ 5,
                                       current_metric == sheet_6 ~ 6,
                                       current_metric == sheet_7 ~ 7,
                                       current_metric == sheet_8 ~ 8,
                                       .default = cli::cli_abort(c("!" = "Error in {.fn read_safe_data} with the argument {.arg supplemental} set to {.val {TRUE}}.",
                                                                   "i" = "Supplemental data is only available for: {.val {c(sheet_3, sheet_4, sheet_5, sheet_6, sheet_7, sheet_8)}}",
                                                                   "x" = "You are running this function for: {.val {current_metric}}.")))
    }
    
    excel_path_folder <- fs::dir_ls(shortcut("echo dropbox", "Quarterly Performance Scorecards", "Submission Tracking", "SAFE Data Submissions")) |> 
      purrr::discard(\(x) !grepl(project, x))
    
    excel_file <- paste0(excel_path_folder, "/", .year, "_q", .quarter, "_", project, ".xlsx")
    
    return(readxl::read_excel(excel_file, sheet = sheet_number))
  }
  
  short_metric_label <- function(current_metric = get("metric_subdirectory", pos = parent.frame()))
  {
    gsub("^(.{2})(.*)$", "\\1-\\2", strsplit(current_metric, "-")[[1]][2])
  }
  
  #### PROGRESS BAR START ##############################################################################################
  scorecard_function_env <- environment()
  
  cli::cli_progress_bar(name = "Progress:", total = 36)
  
  #### Write report parameters file ####################################################################################
  if (.save_result_tables)
  {
    write(paste0("CoC Quarterly Performance Scorecards Report: Quarter ", .quarter, " of ", .year,
                 "\n\nReporting Period: ", format(as.Date(lubridate::int_start(reportingPeriod)), "%B %d, %Y"), " - ", format(as.Date(lubridate::int_end(reportingPeriod)), "%B %d, %Y"),
                 "\n\nQuarter Dates: ", format(as.Date(lubridate::int_start(operatingQuarter)), "%B %d, %Y"), " - ", format(as.Date(lubridate::int_end(operatingQuarter)), "%B %d, %Y"),
                 "\n\nReturn Lookback Window: ", format(as.Date(lubridate::int_start(returns_lookback)), "%B %d, %Y"), " - ", format(as.Date(lubridate::int_end(returns_lookback)), "%B %d, %Y")),
          file = paste0(results_directory, "/report-parameters.txt"))
  }
  
  #### FUNCTIONS FOR THE SCORECARD METRICS #############################################################################
  
  # PERFORMANCE MONITORING METRIC-1 ====================================================================================
  # PSH: "Referral to Move-In Time & Rate" ----
  # RRH: "Referral to Enrollment Time & Rate" ----
  metric_pm_1 <- function(.data = NULL, ..., return_max_points = NULL, return_median = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-PM1"
    
    psh_scores <- set_scores(tiers = 4,
                             thresholds = c(90, 180, 270, 271),
                             points = c(10, 6, 2, 0))
    
    rrh_scores <- set_scores(tiers = 4,
                             thresholds = c(14, 20, 28, 29),
                             points = c(12, 8, 4, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input_days, project_type)
    {
      input_days <- round(input_days)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "psh" = dplyr::case_when(dplyr::between(input_days, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input_days, scores$Threshold[which(scores$Tier == 1)] + 1, scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input_days, scores$Threshold[which(scores$Tier == 2)] + 1, scores$Threshold[which(scores$Tier == 3)]) ~ 3,
                                      input_days >= scores$Threshold[which(scores$Tier == 4)] ~ 4,
                                      .default = NA),
             "rrh" = dplyr::case_when(dplyr::between(input_days, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input_days, scores$Threshold[which(scores$Tier == 1)] + 1, scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input_days, scores$Threshold[which(scores$Tier == 2)] + 1, scores$Threshold[which(scores$Tier == 3)]) ~ 3,
                                      input_days >= scores$Threshold[which(scores$Tier == 4)] ~ 4,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Referral.to.Movein  = MoveInDate - `Service Refer Date`,
                        Referral.to.Movein.No.Negs = dplyr::if_else(Referral.to.Movein < 0, NA, Referral.to.Movein),
                        Average.Movein.Time = mean(Referral.to.Movein.No.Negs, na.rm = TRUE),
                        Movein.Tier         = calculate_tier(as.double(Average.Movein.Time), "psh"))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          `Service Refer Date`,
                          `Service Refer Outcome`,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Referral.to.Movein,
                          Average.Movein.Time,
                          Movein.Tier) |> 
            download_results()
        }
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        avg_movein_days <- as.numeric(round(project_data$Average.Movein.Time[1]))
        
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Movein.Tier))),
                                                psh_scores$Points[which(psh_scores$Tier == dplyr::pull(dplyr::distinct(project_data, Movein.Tier)))],
                                                NA_real_),
                      "Text Results" = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Movein.Tier))),
                                              cli::pluralize("{avg_movein_days} average day{?s} from referral to move-in"),
                                              cli::pluralize("None of the {nrow(project_data)} accepted referral{?s} moved-in during the reporting period")),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        project_data <- suppressWarnings(read_safe_data(project_id) |> 
                                           dplyr::filter(Metric == short_metric_label()) |> 
                                           dplyr::mutate(`Data for Reporting Period (Proportion or Number)` = as.numeric(`Data for Reporting Period (Proportion or Number)`)))
        
        result <- dplyr::pull(project_data, `Data for Reporting Period (Proportion or Number)`)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (is.na(result))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(result > 0,
                                                psh_scores$Points[which(psh_scores$Tier == calculate_tier(result, "psh"))],
                                                NA_real_),
                      "Text Results" = ifelse(result > 0,
                                              cli::pluralize("{result} average day{?s} from referral to move-in"),
                                              paste0("No accepted referrals moved in during the reporting period")),
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    median_for_psh <- function()
    {
      psh_projects <- which(these_project("types", cocProjects, hmis_extract = hmisExtract) == "psh") |>
        vapply(\(.) cocProjects[.], FUN.VALUE = double(1))
      
      project_data <- accepted_referrals |>
        dplyr::filter(ProjectID == psh_projects[1]) |>
        dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
        dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
        fill_in_hoh()
      
      if (length(psh_projects) > 1)
      {
        for (n in 2:length(psh_projects))
        {
          project_data <- project_data |>
            dplyr::bind_rows(accepted_referrals |>
                               dplyr::filter(ProjectID == psh_projects[n]) |>
                               dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
                               dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
                               fill_in_hoh())
          rm(n)
        }
      }
      
      project_data <- project_data |>
        dplyr::filter(HOH == 1) |>
        dplyr::mutate(Referral.to.Movein = MoveInDate - `Service Refer Date`,
                      Referral.to.Movein.No.Negs = dplyr::if_else(Referral.to.Movein < 0, NA, Referral.to.Movein),
                      Median.Movein = median(Referral.to.Movein.No.Negs, na.rm = TRUE)) |>
        dplyr::distinct(Median.Movein)
      
      return(project_data$Median.Movein)
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Referral.to.Entry = EntryDate - `Service Refer Date`,
                        Referral.to.Entry.No.Negs = dplyr::if_else(Referral.to.Entry < 0, NA, Referral.to.Entry),
                        Average.Entry.Time = mean(Referral.to.Entry.No.Negs, na.rm = TRUE),
                        Entry.Tier = calculate_tier(as.double(Average.Entry.Time), "rrh"))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          `Service Refer Date`,
                          `Service Refer Outcome`,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Referral.to.Entry,
                          Average.Entry.Time,
                          Entry.Tier) |> 
            download_results()
        }
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          return(return_na())
        }
        
        avg_entry_days <- as.numeric(round(project_data$Average.Entry.Time[1]))
        
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Entry.Tier))),
                                                rrh_scores$Points[which(rrh_scores$Tier == dplyr::pull(dplyr::distinct(project_data, Entry.Tier)))],
                                                NA_real_),
                      "Text Results" = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Entry.Tier))),
                                              cli::pluralize("{avg_entry_days} average day{?s} from referral to enrollment"),
                                              cli::pluralize("None of the {nrow(project_data)} accepted referral{?s} were enrolled during the reporting period")),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        project_data <- suppressWarnings(read_safe_data(project_id) |> 
                                           dplyr::filter(Metric == short_metric_label()) |> 
                                           dplyr::mutate(`Data for Reporting Period (Proportion or Number)` = as.numeric(`Data for Reporting Period (Proportion or Number)`)))
        
        result <- dplyr::pull(project_data, `Data for Reporting Period (Proportion or Number)`)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (is.na(result))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(result > 0,
                                                rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(result, "rrh"))],
                                                NA_real_),
                      "Text Results" = ifelse(result > 0,
                                              cli::pluralize("{result} average day{?s} from referral to enrollment"),
                                              cli::pluralize("No accepted referrals were enrolled during the reporting period")),
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    median_for_rrh <- function()
    {
      rrh_projects <- which(these_project("types", cocProjects, hmis_extract = hmisExtract) == "rrh") |>
        vapply(\(.) cocProjects[.], FUN.VALUE = double(1))
      
      project_data <- accepted_referrals |>
        dplyr::filter(ProjectID == rrh_projects[1]) |>
        dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
        dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
        fill_in_hoh()
      
      if (length(rrh_projects) > 1)
      {
        for (n in 2:length(rrh_projects))
        {
          project_data <- project_data |>
            dplyr::bind_rows(accepted_referrals |>
                               dplyr::filter(ProjectID == rrh_projects[n]) |>
                               dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
                               dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
                               fill_in_hoh())
          rm(n)
        }
      }
      
      project_data <- project_data |>
        dplyr::filter(HOH == 1) |>
        dplyr::mutate(Referral.to.Entry = EntryDate - `Service Refer Date`, # Referral.to.Entry = MoveInDate - `Service Refer Date`, # ERROR?
                      Referral.to.Entry.No.Negs = dplyr::if_else(Referral.to.Entry < 0, NA, Referral.to.Entry),
                      Median.Entry = median(Referral.to.Entry.No.Negs, na.rm = TRUE)) |>
        dplyr::distinct(Median.Entry)
      
      return(project_data$Median.Entry)
    }
    
    if (is.null(return_median))
    {
      run_metric_pm1 <- function(project)
      {
        project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
        
        switch(project_type,
               "psh" = calculate_for_psh(project),
               "rrh" = calculate_for_rrh(project),
               na_type())
      }
      
      return(dplyr::mutate(.data, `Metric PM-1` = vapply(.data$`Project ID`, run_metric_pm1, FUN.VALUE = type_to_return())))
    }
    else
    {
      pm1_median <- switch(return_median,
                           "psh" = median_for_psh(),
                           "rrh" = median_for_rrh(),
                           NA_real_)
      
      return(pm1_median)
    }
  }
  
  # PERFORMANCE MONITORING METRIC-2 ====================================================================================
  # PSH: "Successful Retention/Exit" ----
  # RRH: "Enrollment to Move-In Time & Rate" ----
  metric_pm_2 <- function(.data = NULL, ..., return_max_points = NULL, return_median = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-PM2"
    
    psh_scores <- set_scores(tiers = 4,
                             thresholds = c(percent(96), percent(93), percent(90), percent(89)),
                             points = c(15, 10, 5, 0))
    
    rrh_scores <- set_scores(tiers = 4,
                             thresholds = c(60, 90, 120, 121),
                             points = c(6, 4, 2, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- switch(project_type,
                      "rrh" = round(input),
                      "psh" = round_as_percent(input, as_decimal = TRUE))
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)] + 1, scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)] + 1, scores$Threshold[which(scores$Tier == 3)]) ~ 3,
                                      input >= scores$Threshold[which(scores$Tier == 4)] ~ 4,
                                      .default = NA),
             
             "psh" = dplyr::case_when(dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                                      dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 4)]) ~ 4,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        enrolled_households <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1), "PersonalID")
        
        excluded_exits <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, Destination %in% excludedDestinationCodes), "PersonalID")
        
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        is.na(ExitDate) | Destination %in% phDestinationCodes)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          EnrollmentID,
                          PersonalID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Destination) |>
            download_results()
        }
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          return(na_type())
        }
        
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(nrow(project_data) / (enrolled_households - excluded_exits), "psh"))],
                      "Text Results"   = cli::pluralize("{nrow(project_data)} retention{?s} and or PH exit{?s} / ({enrolled_households} project client{?s} - {excluded_exits} excluded exit{?s}) = {round_as_percent(nrow(project_data) / (enrolled_households - excluded_exits))}%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1) |>
          dplyr::mutate(Enrollment.to.Movein = MoveInDate - EntryDate,
                        Enrollment.to.Movein.No.Negs = dplyr::if_else(Enrollment.to.Movein < 0, NA, Enrollment.to.Movein),
                        Average.Movein.Time = mean(Enrollment.to.Movein.No.Negs, na.rm = TRUE),
                        Movein.Tier = calculate_tier(as.double(Average.Movein.Time), "rrh"))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(na_type())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Enrollment.to.Movein,
                          Average.Movein.Time,
                          Movein.Tier) |>
            download_results()
        }
        
        # Otherwise, continue analysis
        avg_movein_days <- as.numeric(round(project_data$Average.Movein.Time[1]))
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Movein.Tier))),
                                                rrh_scores$Points[which(rrh_scores$Tier == dplyr::pull(dplyr::distinct(project_data, Movein.Tier)))],
                                                NA_real_),
                      "Text Results"   = ifelse(!is.na(dplyr::pull(dplyr::distinct(project_data, Movein.Tier))),
                                                cli::pluralize("{avg_movein_days} average day{?s} from enrollment to move-in"),
                                                paste0("No enrolled clients in the reporting period have move-in dates")),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    median_for_rrh <- function()
    {
      rrh_projects <- which(these_project("types", cocProjects, hmis_extract = hmisExtract) == "rrh") |>
        vapply(\(.) cocProjects[.], FUN.VALUE = double(1))
      
      project_data <- scorecard_universe |> 
        dplyr::filter(ProjectID == rrh_projects[1])
      
      if (length(rrh_projects) > 1)
      {
        for (n in 2:length(rrh_projects))
        {
          project_data <- project_data |>
            dplyr::bind_rows(scorecard_universe |> 
                               dplyr::filter(ProjectID == rrh_projects[n]))
          
          rm(n)
        }
      }
      
      project_data <- project_data |>
        dplyr::filter(HOH == 1,
                      MoveInDate > EntryDate) |> # This is different from Line 1447 ... why/neccessary? ----
      dplyr::mutate(Enrollment.to.Movein = MoveInDate - EntryDate,
                    Enrollment.to.Movein.No.Negs = dplyr::if_else(Enrollment.to.Movein < 0, NA, Enrollment.to.Movein),
                    Median.Movein = median(Enrollment.to.Movein.No.Negs, na.rm = TRUE)) |> 
        dplyr::distinct(Median.Movein)
      
      return(project_data$Median.Movein)
    }
    
    if (is.null(return_median))
    {
      run_metric_pm2 <- function(project)
      {
        project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
        
        switch(project_type,
               "psh" = calculate_for_psh(project),
               "rrh" = calculate_for_rrh(project),
               na_type())
      }
      
      return(.data |> dplyr::mutate(`Metric PM-2` = vapply(.data$`Project ID`, run_metric_pm2, FUN.VALUE = type_to_return())))
    }
    else
    {
      pm2_median <- switch(return_median,
                           "rrh" = median_for_rrh(),
                           cli::cli_abort(c("!" = "{.strong Invalid input for the {.arg return_median} argument:}",
                                            "x" = "The {.strong {.fn metric_pm_2}} function can only return a median for the {.strong {.val rrh}} project type.",
                                            "i" = "{.emph For the above result, you will need to enter} {.strong {.val rrh}} {.emph for the} {.strong {.arg return_median}} {.emph argument.}")))
      
      return(pm2_median)
    }
  }
  
  # PERFORMANCE MONITORING METRIC-3 ====================================================================================
  # PSH: "Income Growth" ----
  # RRH: "Successful Exits" ----
  metric_pm_3 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-PM3"
    
    psh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(60), percent(35), percent(34)),
                             points = c(10, 5, 0))
    
    rrh_scores <- set_scores(tiers = 4,
                             thresholds = c(percent(85), percent(71), percent(60), percent(59)),
                             points = c(15, 10, 5, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             # Manually assign tiers based on results in function below for PSH!!!
             "rrh" = dplyr::case_when(dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                                      dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                                      dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                                      dplyr::between(rounded_input, 0, scores$Threshold[which(scores$Tier == 4)]) ~ 4,
                                      .default = NA))
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        assessment_data <- income_data_full |>
          dplyr::mutate(across(contains("Date"), as.Date)) |> 
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage == 5,
                        lubridate::`%within%`(InformationDate, reportingPeriod)) |>
          dplyr::arrange(PersonalID, desc(InformationDate)) |> 
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          dplyr::select(PersonalID,
                        EnrollmentID,
                        Latest_Annual_Assessment_Date = InformationDate)
        
        exit_data <- project_data |> 
          dplyr::filter(lubridate::`%within%`(ExitDate, reportingPeriod)) |> 
          dplyr::select(PersonalID, EnrollmentID, ExitDate)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        if(nrow(assessment_data) == 0 & nrow(exit_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        project_data <- project_data |> 
          dplyr::filter(PersonalID %in% unique(c(assessment_data$PersonalID, exit_data$PersonalID)))
        
        income_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID)
        
        income_end_amounts <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          dplyr::ungroup() |>
          dplyr::rename(End.Income = TotalMonthlyIncome) |>
          dplyr::filter(DataCollectionStage != 1) |> # NEW 2024-06-26, making sure people with only entries don't have it counted for start & end ----
          dplyr::select(PersonalID, End.Income)
        
        income_start_amounts0 <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::mutate(Income_Observations = dplyr::n(),
                        Income_Row = dplyr::row_number())
        
        income_start_amounts_one_observation <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations == 1) |> 
          dplyr::ungroup()
        
        income_start_amounts_multiple_observations <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations > 1) |> 
          dplyr::slice(2) |> 
          dplyr::ungroup()
        
        income_start_amounts <- income_start_amounts_one_observation |> 
          dplyr::bind_rows(income_start_amounts_multiple_observations) |> 
          dplyr::rename(Start.Income = TotalMonthlyIncome) |>
          dplyr::select(PersonalID, Start.Income)
        
        project_data <- project_data |>
          dplyr::left_join(income_start_amounts, by = c("PersonalID")) |>
          dplyr::left_join(income_end_amounts, by = c("PersonalID")) |>
          dplyr::mutate(Increased.or.Maintained.Income = dplyr::case_when(Start.Income == 0 & End.Income == 0 ~ "Zero",
                                                                          is.na(Start.Income) | is.na(End.Income) ~ "Missing", # NOTE: Adjust name or add category for only missing end? ---
                                                                          End.Income == Start.Income ~ "Sustained",
                                                                          End.Income > Start.Income ~ "Increased",
                                                                          .default = "Decreased"))
        
        # Continue analysis if there 'are' applicable clients for this metric
        # decrease_zero <- round((sum(project_data$Increased.or.Maintained.Income == "Decreased", project_data$Increased.or.Maintained.Income == "Zero")  * 100) / nrow(project_data))
        decrease_zero <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Decreased", project_data$Increased.or.Maintained.Income == "Zero") / nrow(project_data))
        
        # sustained <- round((sum(project_data$Increased.or.Maintained.Income == "Sustained") * 100) / nrow(project_data))
        sustained <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Sustained") / nrow(project_data))
        
        # increased <- round((sum(project_data$Increased.or.Maintained.Income == "Increased") * 100) / nrow(project_data))
        increased <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Increased") / nrow(project_data))
        
        # increased_or_sustained <- round((sum(project_data$Increased.or.Maintained.Income == "Increased",
        #                                      project_data$Increased.or.Maintained.Income == "Sustained") * 100) / nrow(project_data))
        increased_or_sustained <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Increased",
                                                       project_data$Increased.or.Maintained.Income == "Sustained") / nrow(project_data))
        
        # majority <- max(c(decrease_zero, sustained, increased))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = dplyr::case_when(increased > 50 ~ psh_scores$Points[which(psh_scores$Tier == 1)],
                                                          increased_or_sustained > 50 ~ psh_scores$Points[which(psh_scores$Tier == 2)],
                                                          increased_or_sustained < 50 ~ psh_scores$Points[which(psh_scores$Tier == 3)], # !!! Should this be <= ??? ----
                                                          .default = NA_real_),
                      "Text Results" = dplyr::case_when(increased > 50 ~ cli::pluralize("{increased}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased')} of {nrow(project_data)} client{?s}) increased their income"),
                                                        increased_or_sustained > 50 ~ cli::pluralize("{increased_or_sustained}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased', project_data$Increased.or.Maintained.Income == 'Sustained')} of {nrow(project_data)} client{?s}) increased OR maintained their income, but less than 50% increased their income"),
                                                        increased_or_sustained < 50 ~ cli::pluralize("Only {increased_or_sustained}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased', project_data$Increased.or.Maintained.Income == 'Sustained')} of {nrow(project_data)} client{?s}) increased OR maintained their income"), # !!! Should this be <= ??? ----
                                                        .default = NA_character_),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        exited_households <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, !is.na(ExitDate)), "PersonalID")
        
        excluded_exits <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, Destination %in% excludedDestinationCodes), "PersonalID")
        
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        Destination %in% phDestinationCodes)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Destination) |>
            download_results()
        }
        
        # Return NA if there are no applicable clients for this metric
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(nrow(project_data) / (exited_households - excluded_exits), "rrh"))],
                      # "Text Results"   = cli::pluralize("{nrow(project_data)} positive exit{?s} / ({exited_households} total exit{?s} - {excluded_exits} excluded exit{?s}) = {round((nrow(project_data) * 100) / (exited_households - excluded_exits))}%"),
                      "Text Results"   = cli::pluralize("{nrow(project_data)} positive exit{?s} / ({exited_households} total exit{?s} - {excluded_exits} excluded exit{?s}) = {round_as_percent(nrow(project_data) / (exited_households - excluded_exits))}%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_pm3 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric PM-3` = vapply(.data$`Project ID`, run_metric_pm3, FUN.VALUE = type_to_return())))
  }
  
  # PERFORMANCE MONITORING METRIC-4 ====================================================================================
  # PSH & RRH: "Returns to Homelessness" ----
  metric_pm_4 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-PM4"
    
    psh_scores <- set_scores(tiers = 2,
                             thresholds = c(0, 1),
                             points = c(15, 0))
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(0), percent(10), percent(11)),
                             points = c(12, 8, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      if (project_type == "rrh")
      {
        input <- round_as_percent(input, as_decimal = TRUE)
      }
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      return(switch(project_type,
                    "psh" = dplyr::case_when(input == scores$Threshold[which(scores$Tier == 1)] ~ 1,
                                             input < 0 ~ NA,
                                             .default = 2),
                    "rrh" = dplyr::case_when(input == scores$Threshold[which(scores$Tier == 1)] ~ 1,
                                             dplyr::between(input, one_percent_above(scores$Threshold[which(scores$Tier == 1)]), scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                             dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                             .default = NA),
                    NA_real_))
    }
    
    calculate_for_ph <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        intervention <- ECHO::this_project("type", project_id, hmis_extract = hmisExtract)
        
        assign("scores", get(paste0(intervention, "_scores")))
        
        project_data <- coc_exits |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::mutate(Excluded_Range = lubridate::interval(EntryDate + lubridate::days(1),
                                                             pmin(ExitDate + lubridate::days(14),
                                                                  lubridate::int_start(operatingQuarter) - lubridate::days(1))))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No possible returns in the reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        returns_universe <- hmisExtract$entry |>
          dplyr::rename(PossibleReturnDate = EntryDate) |>
          dplyr::filter(PersonalID %in% project_data$PersonalID,
                        !EnrollmentID %in% project_data$EnrollmentID) |>
          dplyr::left_join(dplyr::select(hmisExtract$project, ProjectID, ProjectType), by = c("ProjectID")) |>
          dplyr::filter(ProjectType %in% c(0:4, 8:10, 13)) |>
          dplyr::select(PersonalID, PossibleReturnDate) |>
          dplyr::mutate(Entered.On = paste0("Entered.On ", PossibleReturnDate, " ", (\(.) dplyr::row_number(.))())) |>
          tidyr::pivot_wider(names_from = Entered.On, values_from = PossibleReturnDate)
        
        # Return NA if there are no possible returns at all for this metric.
        na_message <- "No possible returns in the reporting period"
        
        if (dplyr::tally(returns_universe) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        returnStartColumn <- length(project_data) + 1
        returnEndColumn <- length(project_data) + length(returns_universe) - 1
        
        is_within_six_months <- function(Exit_Date, ReEntry_Date, excluded_window)
        {
          returnWindow <- lubridate::interval(Exit_Date, Exit_Date + lubridate::dmonths(6))
          
          return(ifelse(!is.na(ReEntry_Date) & ReEntry_Date != (lubridate::int_start(excluded_window) - lubridate::days(1)),
                        ifelse(!lubridate::`%within%`(ReEntry_Date, excluded_window),
                               lubridate::`%within%`(ReEntry_Date, returnWindow),
                               FALSE),
                        FALSE))
        }
        
        is_within_twelve_months <- function(Exit_Date, ReEntry_Date)
        {
          returnWindow <- lubridate::interval(Exit_Date + lubridate::dmonths(6) + lubridate::ddays(1), Exit_Date + lubridate::dmonths(12))
          
          return(ifelse(!is.na(ReEntry_Date),
                        lubridate::`%within%`(ReEntry_Date, returnWindow),
                        FALSE))
        }
        
        is_within_twentyfour_months <- function(Exit_Date, ReEntry_Date)
        {
          returnWindow <- lubridate::interval(Exit_Date + lubridate::dmonths(12) + lubridate::ddays(1), Exit_Date + lubridate::dmonths(24))
          
          return(ifelse(!is.na(ReEntry_Date),
                        lubridate::`%within%`(ReEntry_Date, returnWindow),
                        FALSE))
        }
        
        project_data <- dplyr::`%>%`(dplyr::`%>%`(dplyr::`%>%`(project_data, dplyr::left_join(returns_universe, by = c("PersonalID"))), dplyr::rowwise()), dplyr::mutate(Returned.to.Homelessness.Within.6.Months = dplyr::if_else(any(is_within_six_months(ExitDate, dplyr::c_across(names(.[returnStartColumn]):names(.[returnEndColumn])), Excluded_Range), na.rm = TRUE), TRUE, FALSE), Returned.to.Homelessness.Within.12.Months = dplyr::if_else(any(is_within_twelve_months(ExitDate, dplyr::c_across(names(.[returnStartColumn]):names(.[returnEndColumn]))), na.rm = TRUE), TRUE, FALSE), Returned.to.Homelessness.Within.24.Months = dplyr::if_else(any(is_within_twentyfour_months(ExitDate, dplyr::c_across(names(.[returnStartColumn]):names(.[returnEndColumn]))), na.rm = TRUE), TRUE, FALSE)))
        
        r.6 <- sum(project_data$Returned.to.Homelessness.Within.6.Months)
        
        r.12 <- sum(project_data$Returned.to.Homelessness.Within.12.Months)
        
        r.24 <- sum(project_data$Returned.to.Homelessness.Within.24.Months)
        
        return_counts <- tibble::as_tibble(project_data |>
                                             dplyr::rowwise() |>
                                             mutate(six_to_twelve = sum(Returned.to.Homelessness.Within.6.Months,
                                                                        Returned.to.Homelessness.Within.12.Months),
                                                    six_to_twentyfour = sum(Returned.to.Homelessness.Within.6.Months,
                                                                            Returned.to.Homelessness.Within.12.Months,
                                                                            Returned.to.Homelessness.Within.24.Months)))
        
        project_data <- project_data |> mutate(Total.6.Month.Returns = r.6,
                                               Total.12.Month.Returns = r.12 - as.double(dplyr::tally(return_counts,
                                                                                                      six_to_twelve > 1)),
                                               Total.24.Month.Returns = r.24 - as.double(dplyr::tally(return_counts,
                                                                                                      six_to_twentyfour > 1)))
        
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          tidyselect::contains("Entered.On"),
                          Returned.to.Homelessness.Within.6.Months,
                          Returned.to.Homelessness.Within.12.Months,
                          Returned.to.Homelessness.Within.24.Months,
                          Total.6.Month.Returns,
                          Total.12.Month.Returns,
                          Total.24.Month.Returns) |>
            download_results()
        }
        
        return(switch(function_mode,
                      "Numeric Scores" = switch(intervention,
                                                "rrh" = scores$Points[which(scores$Tier == calculate_tier(sum(project_data$Returned.to.Homelessness.Within.6.Months) / nrow(project_data), intervention))],
                                                "psh" = scores$Points[which(scores$Tier == calculate_tier(sum(project_data$Returned.to.Homelessness.Within.6.Months), intervention))]),
                      # "Text Results"   = cli::pluralize("{sum(project_data$Returned.to.Homelessness.Within.6.Months)} client{?s} returned within 6 months / {nrow(project_data)} exit{?s} to PH = {round((sum(project_data$Returned.to.Homelessness.Within.6.Months) * 100) / nrow(project_data))}%"),
                      "Text Results"   = cli::pluralize("{sum(project_data$Returned.to.Homelessness.Within.6.Months)} client{?s} returned within 6 months / {nrow(project_data)} exit{?s} to PH = {round_as_percent(sum(project_data$Returned.to.Homelessness.Within.6.Months) / nrow(project_data))}%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_pm4 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      ifelse(project_type %in% c("psh", "rrh"),
             calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric PM-4` = vapply(.data$`Project ID`, run_metric_pm4, FUN.VALUE = type_to_return())))
  }
  
  # PERFORMANCE MONITORING METRIC-5 ====================================================================================
  # PSH: "Healthcare Benefits Access" ----
  # RRH: "Income Growth" ----
  metric_pm_5 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-PM5"
    
    psh_scores <- set_scores(tiers = 4,
                             thresholds = c(percent(91), percent(81), percent(71), percent(70)),
                             points = c(5, 3, 1, 0))
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(51), percent(50), 0),
                             points = c(15, 10, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             # Manually assign tiers based on results in function below for RRH!!!
             "psh" = dplyr::case_when(dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                                      dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                                      dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                                      dplyr::between(rounded_input, 0, scores$Threshold[which(scores$Tier == 4)]) ~ 4,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        !is.na(MoveInDate))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        benefits_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          benefits_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE) / nrow(project_data), "psh"))],
                      # "Text Results"   = cli::pluralize("{sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE)} / {nrow(project_data)} moved-in head{?s} of household {cli::qty(sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE))}ha{?s/ve} medical insurance ({round((sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE) * 100) / nrow(project_data))}%)"),
                      "Text Results"   = cli::pluralize("{sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE)} / {nrow(project_data)} moved-in head{?s} of household {cli::qty(sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE))}ha{?s/ve} medical insurance ({round_as_percent(sum(benefits_data$InsuranceFromAnySource ==  1, na.rm = TRUE) / nrow(project_data))}%)"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        assessment_data <- income_data_full |>
          dplyr::mutate(across(contains("Date"), as.Date)) |> 
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage == 5,
                        lubridate::`%within%`(InformationDate, reportingPeriod)) |>
          dplyr::arrange(PersonalID, desc(InformationDate)) |> 
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          dplyr::select(PersonalID,
                        EnrollmentID,
                        Latest_Annual_Assessment_Date = InformationDate)
        
        exit_data <- project_data |> 
          dplyr::filter(lubridate::`%within%`(ExitDate, reportingPeriod)) |> 
          dplyr::select(PersonalID, EnrollmentID, ExitDate)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        if(nrow(assessment_data) == 0 & nrow(exit_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        project_data <- project_data |> 
          dplyr::filter(PersonalID %in% unique(c(assessment_data$PersonalID, exit_data$PersonalID)))
        
        income_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID)
        
        income_end_amounts <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          dplyr::ungroup() |>
          dplyr::rename(End.Income = TotalMonthlyIncome) |>
          dplyr::filter(DataCollectionStage != 1) |>  # NEW 2024-07-03, making sure people with only entries don't have it counted for start & end ----
          dplyr::select(PersonalID, End.Income)
        
        income_start_amounts0 <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::mutate(Income_Observations = dplyr::n(),
                        Income_Row = dplyr::row_number())
        
        income_start_amounts_one_observation <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations == 1) |> 
          dplyr::ungroup()
        
        income_start_amounts_multiple_observations <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations > 1) |> 
          dplyr::slice(2) |> 
          dplyr::ungroup()
        
        income_start_amounts <- income_start_amounts_one_observation |> 
          dplyr::bind_rows(income_start_amounts_multiple_observations) |> 
          dplyr::rename(Start.Income = TotalMonthlyIncome) |>
          dplyr::select(PersonalID, Start.Income)
        
        project_data <- project_data |>
          dplyr::left_join(income_start_amounts, by = c("PersonalID")) |>
          dplyr::left_join(income_end_amounts, by = c("PersonalID")) |>
          dplyr::mutate(Increased.or.Maintained.Income = dplyr::case_when(Start.Income == 0 & End.Income == 0 ~ "Zero",
                                                                          is.na(Start.Income) | is.na(End.Income) ~ "Missing",
                                                                          End.Income == Start.Income ~ "Sustained",
                                                                          End.Income > Start.Income ~ "Increased",
                                                                          .default = "Decreased"))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |>  download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        # decrease_zero <- round((sum(project_data$Increased.or.Maintained.Income == "Decreased", project_data$Increased.or.Maintained.Income == "Zero")  * 100) / nrow(project_data))
        decrease_zero <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Decreased", project_data$Increased.or.Maintained.Income == "Zero") / nrow(project_data))
        
        # sustained <- round((sum(project_data$Increased.or.Maintained.Income == "Sustained") * 100) / nrow(project_data))
        sustained <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Sustained") / nrow(project_data))
        
        # increased <- round((sum(project_data$Increased.or.Maintained.Income == "Increased") * 100) / nrow(project_data))
        increased <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Increased") / nrow(project_data))
        
        # increased_or_sustained <- round((sum(project_data$Increased.or.Maintained.Income == "Increased",
        #                                      project_data$Increased.or.Maintained.Income == "Sustained") * 100) / nrow(project_data))
        increased_or_sustained <- round_as_percent(sum(project_data$Increased.or.Maintained.Income == "Increased",
                                                       project_data$Increased.or.Maintained.Income == "Sustained") / nrow(project_data))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = dplyr::case_when(increased > 50 ~ rrh_scores$Points[which(rrh_scores$Tier == 1)],
                                                          increased_or_sustained > 50 ~ rrh_scores$Points[which(rrh_scores$Tier == 2)],
                                                          increased_or_sustained < 50 ~ rrh_scores$Points[which(rrh_scores$Tier == 3)], # !!! Should this be <= ??? ----
                                                          .default = NA_real_),
                      "Text Results" = dplyr::case_when(increased > 50 ~ cli::pluralize("{increased}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased')} of {nrow(project_data)} client{?s}) increased their income"),
                                                        increased_or_sustained > 50 ~ cli::pluralize("{increased_or_sustained}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased', project_data$Increased.or.Maintained.Income == 'Sustained')} of {nrow(project_data)} client{?s}) increased OR maintained their income, but less than 50% increased their income"),
                                                        increased_or_sustained < 50 ~ cli::pluralize("Only {increased_or_sustained}% ({sum(project_data$Increased.or.Maintained.Income == 'Increased', project_data$Increased.or.Maintained.Income == 'Sustained')} of {nrow(project_data)} client{?s}) increased OR maintained their income"), # !!! Should this be <= ??? ----
                                                        .default = NA_character_),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_pm5 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric PM-5` = vapply(.data$`Project ID`, run_metric_pm5, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-1 =============================================================================================
  # PSH: "White/BIPOC Referral to Move-in Rate" (a.k.a. EM-1a) ----
  # RRH: "White/BIPOC Referral to Enrollment Rate" (a.k.a. EM-1a) ----
  metric_re_1 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE1"
    
    psh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(3, 0))
    
    rrh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(1, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Relevant_MoveInDate = dplyr::if_else(!lubridate::`%within%`(MoveInDate, reportingPeriod),
                                                             NA,
                                                             MoveInDate),
                        Client.Race.Ethnicity = dplyr::if_else(is.na(Race_Ethnicity),
                                                               fill_in_race_ethnicity(PersonalID),
                                                               Race_Ethnicity),
                        is.BIPOC = dplyr::if_else(Client.Race.Ethnicity == "White",
                                                  FALSE,
                                                  TRUE)) |>
          dplyr::filter(!is.na(Client.Race.Ethnicity))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          Relevant_MoveInDate,
                          ExitDate,
                          Client.Race.Ethnicity,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Finish calculations
        bipoc_result <- as.double(dplyr::tally(project_data, !is.na(Relevant_MoveInDate) & is.BIPOC) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, !is.na(Relevant_MoveInDate) & !is.BIPOC) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        # If both are zero, return this
        if (bipoc_result == 0 & non_bipoc_result == 0)
        {
          return(switch(function_mode,
                        "Numeric Scores" = NA_real_,
                        "Text Results"   = cli::pluralize("None of the {nrow(project_data)} accepted referral{?s} moved-in during the reporting period"),
                        na_type()))
        }
        
        # Return the score for this metric normally.
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.na(Relevant_MoveInDate) & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, !is.na(Relevant_MoveInDate) & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Relevant_EntryDate = dplyr::if_else(!lubridate::`%within%`(EntryDate, reportingPeriod),
                                                            NA,
                                                            EntryDate),
                        Client.Race.Ethnicity = dplyr::if_else(is.na(Race_Ethnicity),
                                                               fill_in_race_ethnicity(PersonalID),
                                                               Race_Ethnicity),
                        is.BIPOC = dplyr::if_else(Client.Race.Ethnicity == "White",
                                                  FALSE,
                                                  TRUE)) |>
          dplyr::filter(!is.na(Client.Race.Ethnicity))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          Relevant_EntryDate,
                          MoveInDate,
                          ExitDate,
                          Client.Race.Ethnicity,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Finish calculations
        bipoc_result <- as.double(dplyr::tally(project_data, !is.na(Relevant_EntryDate) & is.BIPOC) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, !is.na(Relevant_EntryDate) & !is.BIPOC) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        # If both are zero, return this
        if (bipoc_result == 0 & non_bipoc_result == 0)
        {
          return(switch(function_mode,
                        "Numeric Scores" = NA_real_,
                        "Text Results"   = cli::pluralize("None of the {nrow(project_data)} accepted referral{?s} moved-in during the reporting period"),
                        na_type()))
        }
        
        # Return the score for this metric normally.
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.na(Relevant_EntryDate) & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, !is.na(Relevant_EntryDate) & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re1 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-1` = vapply(.data$`Project ID`, run_metric_re1, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-2 =============================================================================================
  # PSH: "White/BIPOC Referral to Move-in Time" (a.k.a. EM-1b) ----
  # RRH: "White/BIPOC Referral to Enrollment Time" (a.k.a. EM-1b) ----
  metric_re_2 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE2"
    
    psh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(2, 0))
    
    rrh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(2, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             NA)
    }
    
    psh_median <- round(metric_pm_1(return_median = "psh"))
    
    rrh_median <- round(metric_pm_1(return_median = "rrh"))
    
    if (.save_result_tables)
    {
      write(paste0("RRH Median Metric-1 = ", rrh_median, " days\n"),
            file = paste0(results_directory, "/medians.txt"),
            append = TRUE)
      
      write(paste0("PSH Median Metric-1 = ", psh_median, " days\n"),
            file = paste0(results_directory, "/medians.txt"))
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Client.Race.Ethnicity = dplyr::if_else(is.na(Race_Ethnicity),
                                                               fill_in_race_ethnicity(PersonalID),
                                                               Race_Ethnicity),
                        is.BIPOC = dplyr::if_else(Client.Race.Ethnicity == "White",
                                                  FALSE,
                                                  TRUE),
                        Referral.to.Movein = MoveInDate - `Service Refer Date`,
                        Referral.to.Movein.No.Negs = dplyr::if_else(Referral.to.Movein < 0, NA, Referral.to.Movein),
                        Average.Movein.Time = mean(Referral.to.Movein.No.Negs, na.rm = TRUE)) |>
          dplyr::filter(!is.na(Client.Race.Ethnicity))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Client.Race.Ethnicity,
                          is.BIPOC,
                          Referral.to.Movein,
                          Referral.to.Movein.No.Negs) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Finish calculations
        avg_movein_time <- round(as.double(project_data$Average.Movein.Time[1]))
        
        # bipoc_result <- as.double(tally(project_data, is.BIPOC & Referral.to.Movein.No.Negs <= psh_median) / nrow(dplyr::filter(project_data, is.BIPOC)))
        bipoc_result <- as.double(tally(project_data, is.BIPOC & Referral.to.Movein.No.Negs <= avg_movein_time) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        # non_bipoc_result <- as.double(tally(project_data, !is.BIPOC & Referral.to.Movein.No.Negs <= psh_median) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        non_bipoc_result <- as.double(tally(project_data, !is.BIPOC & Referral.to.Movein.No.Negs <= avg_movein_time) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        # If both are zero, return this
        if (bipoc_result == 0 & non_bipoc_result == 0)
        {
          return(switch(function_mode,
                        "Numeric Scores" = NA_real_,
                        "Text Results"   = cli::pluralize("0 / {nrow(project_data)} accepted referral{?s} moved-in during the reporting period"),
                        na_type()))
        }
        
        # Return the score for this metric normally
        # return(switch(function_mode,
        #               "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
        #               "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.BIPOC & Referral.to.Movein.No.Negs <= psh_median)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
        #                                         "BIPOC Result (", as.double(tally(project_data, is.BIPOC & Referral.to.Movein.No.Negs <= psh_median)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
        #                                         round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
        #               na_type()))
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.BIPOC & Referral.to.Movein.No.Negs <= avg_movein_time)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, is.BIPOC & Referral.to.Movein.No.Negs <= avg_movein_time)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- accepted_referrals |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::arrange(PersonalID, dplyr::desc(EntryDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          fill_in_hoh() |>
          dplyr::filter(HOH == 1) |>
          dplyr::mutate(MoveInDate = dplyr::if_else(MoveInDate < `Service Refer Date`,
                                                    NA,
                                                    MoveInDate),
                        Client.Race.Ethnicity = dplyr::if_else(is.na(Race_Ethnicity),
                                                               fill_in_race_ethnicity(PersonalID),
                                                               Race_Ethnicity),
                        is.BIPOC = dplyr::if_else(Client.Race.Ethnicity == "White",
                                                  FALSE,
                                                  TRUE),
                        Referral.to.Entry = EntryDate - `Service Refer Date`,
                        Referral.to.Entry.No.Negs = dplyr::if_else(Referral.to.Entry < 0, NA, Referral.to.Entry),
                        Average.Entry.Time = mean(Referral.to.Entry.No.Negs, na.rm = TRUE)) |>
          dplyr::filter(!is.na(Client.Race.Ethnicity))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No accepted referrals in reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Client.Race.Ethnicity,
                          is.BIPOC,
                          Referral.to.Entry,
                          Referral.to.Entry.No.Negs) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Finish calculations
        avg_entry_time <- round(as.double(project_data$Average.Entry.Time[1]))
        
        # bipoc_result <- as.double(tally(project_data, is.BIPOC & Referral.to.Entry.No.Negs <= psh_median) / nrow(dplyr::filter(project_data, is.BIPOC)))
        bipoc_result <- as.double(tally(project_data, is.BIPOC & Referral.to.Entry.No.Negs <= avg_entry_time) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        # non_bipoc_result <- as.double(tally(project_data, !is.BIPOC & Referral.to.Entry.No.Negs <= psh_median) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        non_bipoc_result <- as.double(tally(project_data, !is.BIPOC & Referral.to.Entry.No.Negs <= avg_entry_time) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        # If both are zero, return this
        if (bipoc_result == 0 & non_bipoc_result == 0)
        {
          return(switch(function_mode,
                        "Numeric Scores" = NA_real_,
                        "Text Results"   = cli::pluralize("0 / {nrow(project_data)} accepted referral{?s} moved-in during the reporting period"),
                        na_type()))
        }
        
        # Return the score for this metric normally
        # return(switch(function_mode,
        #               "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
        #               "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.BIPOC & Referral.to.Entry.No.Negs <= rrh_median)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
        #                                         "BIPOC Result (", as.double(tally(project_data, is.BIPOC & Referral.to.Entry.No.Negs <= rrh_median)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
        #                                         round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
        #               na_type()))
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.BIPOC & Referral.to.Entry.No.Negs <= avg_entry_time)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, is.BIPOC & Referral.to.Entry.No.Negs <= avg_entry_time)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re2 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-2` = vapply(.data$`Project ID`, run_metric_re2, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-3 =============================================================================================
  # PSH: "White/BIPOC Retentions & Positive Exits" (a.k.a. EM-2) ----
  # RRH: "White/BIPOC Enrollment to Move-In Rate" (a.k.a. EM-2a) ----
  metric_re_3 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE3"
    
    psh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(8, 4, 0))
    
    rrh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(3, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        enrolled_households <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1), "PersonalID")
        
        excluded_exits <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, Destination %in% excludedDestinationCodes), "PersonalID")
        
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        is.na(ExitDate) | Destination %in% phDestinationCodes) |>
          dplyr::mutate(is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if appplicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Race_Ethnicity,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally.
        bipoc_result <- as.double(tally(project_data, is.BIPOC) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        non_bipoc_result <- as.double(tally(project_data, !is.BIPOC) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.na(MoveInDate) & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, !is.na(MoveInDate) & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1) |>
          dplyr::mutate(Relevant_MoveInDate = dplyr::if_else(!lubridate::`%within%`(MoveInDate, reportingPeriod),
                                                             NA,
                                                             MoveInDate),
                        is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if appplicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Race_Ethnicity,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally.
        #Edited 10/8/24 to add in !is.na(Relevant_MoveInDate) for both groups
        bipoc_result <- as.double(tally(project_data, !is.na(Relevant_MoveInDate) & is.BIPOC) / nrow(dplyr::filter(project_data, is.BIPOC)))
        
        non_bipoc_result <- as.double(tally(project_data, !is.na(Relevant_MoveInDate) & !is.BIPOC) / nrow(dplyr::filter(project_data, !is.BIPOC)))
        
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.na(MoveInDate) & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, !is.na(MoveInDate) & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re3 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-3` = vapply(.data$`Project ID`, run_metric_re3, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-4 =============================================================================================
  # PSH: "White/BIPOC Income Growth" (a.k.a. EM-3) ----
  # RRH: "White/BIPOC Enrollment to Move-In Time" (a.k.a. RRH EM-2b) ----
  metric_re_4 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE4"
    
    psh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(5, 3, 0))
    
    rrh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(2, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA),
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA),
             NA)
    }
    
    rrh_median <- round(metric_pm_2(return_median = "rrh"))
    
    if (.save_result_tables)
    {
      write(paste0("RRH Median Metric-2 = ", rrh_median, " days\n"),
            file = paste0(results_directory, "/medians.txt"),
            append = TRUE)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        assessment_data <- income_data_full |>
          dplyr::mutate(across(contains("Date"), as.Date)) |> 
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage == 5,
                        lubridate::`%within%`(InformationDate, reportingPeriod)) |>
          dplyr::arrange(PersonalID, desc(InformationDate)) |> 
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          dplyr::select(PersonalID,
                        EnrollmentID,
                        Latest_Annual_Assessment_Date = InformationDate)
        
        exit_data <- project_data |> 
          dplyr::filter(lubridate::`%within%`(ExitDate, reportingPeriod)) |> 
          dplyr::select(PersonalID, EnrollmentID, ExitDate)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        if(nrow(assessment_data) == 0 & nrow(exit_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        project_data <- project_data |> 
          dplyr::filter(PersonalID %in% unique(c(assessment_data$PersonalID, exit_data$PersonalID)))
        
        income_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID)
        
        income_end_amounts <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          dplyr::ungroup() |>
          dplyr::rename(End.Income = TotalMonthlyIncome) |>
          dplyr::filter(DataCollectionStage != 1) |> # NEW 2024-07-03, making sure people with only entries don't have it counted for start & end ----
          dplyr::select(PersonalID, End.Income)
        
        income_start_amounts0 <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::mutate(Income_Observations = dplyr::n(),
                        Income_Row = dplyr::row_number())
        
        income_start_amounts_one_observation <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations == 1) |> 
          dplyr::ungroup()
        
        income_start_amounts_multiple_observations <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations > 1) |> 
          dplyr::slice(2) |> 
          dplyr::ungroup()
        
        income_start_amounts <- income_start_amounts_one_observation |> 
          dplyr::bind_rows(income_start_amounts_multiple_observations) |> 
          dplyr::rename(Start.Income = TotalMonthlyIncome) |>
          dplyr::select(PersonalID, Start.Income)
        
        project_data <- project_data |>
          dplyr::left_join(income_start_amounts, by = c("PersonalID")) |>
          dplyr::left_join(income_end_amounts, by = c("PersonalID")) |>
          dplyr::mutate(Increased.or.Maintained.Income = dplyr::case_when(Start.Income == 0 & End.Income == 0 ~ "Zero",
                                                                          is.na(Start.Income) | is.na(End.Income) ~ "Missing",
                                                                          End.Income == Start.Income ~ "Sustained",
                                                                          End.Income > Start.Income ~ "Increased",
                                                                          .default = "Decreased"),
                        is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        bipoc_result <- as.double(dplyr::tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & is.BIPOC)) / nrow(project_data)
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & !is.BIPOC)) / nrow(project_data)
        
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & !is.BIPOC)), "/",  nrow(project_data), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") &  is.BIPOC)), "/",  nrow(project_data), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1) |>
          dplyr::mutate(Enrollment.to.Movein = MoveInDate - EntryDate,
                        Enrollment.to.Movein.No.Negs = dplyr::if_else(Enrollment.to.Movein < 0, NA, Enrollment.to.Movein),
                        Average.Movein.Time = mean(Enrollment.to.Movein.No.Negs, na.rm = TRUE),
                        is.BIPOC = dplyr::if_else(Race_Ethnicity == "White", FALSE, TRUE))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Race_Ethnicity,
                          Enrollment.to.Movein,
                          Enrollment.to.Movein.No.Negs,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        avg_movein_time <- round(as.double(project_data$Average.Movein.Time[1]))
        
        # bipoc_result <- as.double(tally(project_data, Enrollment.to.Movein.No.Negs <= rrh_median & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        bipoc_result <- as.double(dplyr::tally(project_data, Enrollment.to.Movein.No.Negs <= avg_movein_time & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        
        # non_bipoc_result <- as.double(tally(project_data, Enrollment.to.Movein.No.Negs <= rrh_median & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        non_bipoc_result <- as.double(dplyr::tally(project_data, Enrollment.to.Movein.No.Negs <= avg_movein_time & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        
        # return(switch(function_mode,
        #               "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
        #               "Text Results"   = paste0("White Result (", as.double(tally(project_data, Enrollment.to.Movein.No.Negs <= rrh_median & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
        #                                         "BIPOC Result (", as.double(tally(project_data, Enrollment.to.Movein.No.Negs <= rrh_median & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
        #                                         round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
        #               na_type()))
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(dplyr::tally(project_data, Enrollment.to.Movein.No.Negs <= avg_movein_time & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(dplyr::tally(project_data, Enrollment.to.Movein.No.Negs <= avg_movein_time & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re4 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-4` = vapply(.data$`Project ID`, run_metric_re4, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-5 =============================================================================================
  # PSH: "White/BIPOC Returns" (a.k.a. EM-4) ----
  # RRH: "Successful Exits" (a.k.a. EM-3) ----
  metric_re_5 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE5"
    
    psh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(8, 4, 0))
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(8, 4, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA),
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- coc_exits |>
          dplyr::filter(ProjectID == project_id) |> 
          dplyr::mutate(is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE),
                        Excluded_Range = lubridate::interval(EntryDate + lubridate::days(1),
                                                             pmin(ExitDate + lubridate::days(14),
                                                                  lubridate::int_start(operatingQuarter) - lubridate::days(1))))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No possible returns in the reporting period"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        returns_universe <- hmisExtract$entry |>
          dplyr::rename(PossibleReturnDate = EntryDate) |>
          dplyr::filter(PersonalID %in% project_data$PersonalID,
                        !EnrollmentID %in% project_data$EnrollmentID) |>
          dplyr::left_join(dplyr::select(hmisExtract$project, ProjectID, ProjectType), by = c("ProjectID")) |>
          dplyr::filter(ProjectType %in% c(0:4, 8:10, 13)) |>
          dplyr::select(PersonalID, PossibleReturnDate) |>
          dplyr::mutate(Entered.On = paste0("Entered.On ", PossibleReturnDate, " ", (\(.) dplyr::row_number(.))())) |>
          tidyr::pivot_wider(names_from = Entered.On, values_from = PossibleReturnDate)
        
        returnStartColumn <- length(project_data) + 1
        returnEndColumn <- length(project_data) + length(returns_universe) - 1
        
        is_within_six_months <- function(Exit_Date, ReEntry_Date, excluded_window)
        {
          returnWindow <- lubridate::interval(Exit_Date, Exit_Date + lubridate::dmonths(6))
          
          return(ifelse(!is.na(ReEntry_Date) & ReEntry_Date != (lubridate::int_start(excluded_window) - lubridate::days(1)),
                        ifelse(!lubridate::`%within%`(ReEntry_Date, excluded_window),
                               lubridate::`%within%`(ReEntry_Date, returnWindow),
                               FALSE),
                        FALSE))
        }
        
        # MUST convert rowwise df back to tibble so tally() will work...
        project_data <- tibble::as_tibble(dplyr::`%>%`(dplyr::`%>%`(dplyr::`%>%`(project_data, dplyr::left_join(returns_universe, by = c("PersonalID"))), dplyr::rowwise()), dplyr::mutate(Returned.to.Homelessness.Within.6.Months = dplyr::if_else(any(is_within_six_months(ExitDate, dplyr::c_across(names(.[returnStartColumn]):names(.[returnEndColumn])), Excluded_Range), na.rm = TRUE), TRUE, FALSE))))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          tidyselect::contains("Entered.On"),
                          Race_Ethnicity,
                          is.BIPOC,
                          Returned.to.Homelessness.Within.6.Months) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        bipoc_result <- as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        exited_households <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, !is.na(ExitDate)), "PersonalID")
        
        excluded_exits <- dplyr::n_distinct(dplyr::filter(scorecard_universe, ProjectID == project_id, HOH == 1, Destination %in% excludedDestinationCodes), "PersonalID")
        
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        Destination %in% phDestinationCodes) |>
          dplyr::mutate(is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectName,
                          ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          Destination,
                          Race_Ethnicity,
                          is.BIPOC) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        #Edited 10/8/24 to add "!is.na(MoveInDate) & " so result divides the number without housing by BIPOC
        bipoc_result <- as.double(dplyr::tally(project_data, !is.na(MoveInDate) & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, !is.na(MoveInDate) & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, !is.na(MoveInDate) & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, !is.na(MoveInDate) & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re5 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-5` = vapply(.data$`Project ID`, run_metric_re5, FUN.VALUE = type_to_return())))
  }
  
  # RACIAL EQUITY METRIC-6 =============================================================================================
  # PSH: "White/BIPOC Access to Medical Benefits" (a.k.a. EM-5) ----
  # RRH: "White/BIPOC Returns" (a.k.a. EM-4) ----
  metric_re_6 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE6"
    
    psh_scores <- set_scores(tiers = 2,
                             thresholds = c(percent(9), percent(10)),
                             points = c(2, 0))
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(6, 3, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = assign("scores", get(paste0(project_type, "_scores"))),
             "rrh" = assign("scores", get(paste0(project_type, "_scores"))))
      
      switch(project_type,
             "psh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 2)], percent(100)) ~ 2,
                                      .default = NA_real_),
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, scores$Threshold[which(scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 1)], scores$Threshold[which(scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, scores$Threshold[which(scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA_real_),
             NA)
    }
    
    calculate_for_psh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        !is.na(MoveInDate)) |>
          dplyr::mutate(is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        benefits_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID) |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          dplyr::ungroup() |>
          dplyr::left_join(project_data, by = c("EnrollmentID", "PersonalID"))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          benefits_data |> download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(benefits_data$is.BIPOC) | all(!benefits_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        bipoc_result <- as.double(dplyr::tally(benefits_data, InsuranceFromAnySource == 1 & is.BIPOC)) / nrow(dplyr::filter(benefits_data, is.BIPOC))
        
        non_bipoc_result <- as.double(dplyr::tally(benefits_data, InsuranceFromAnySource == 1 & !is.BIPOC)) / nrow(dplyr::filter(benefits_data, !is.BIPOC))
        
        return(switch(function_mode,
                      "Numeric Scores" = psh_scores$Points[which(psh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "psh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(benefits_data, InsuranceFromAnySource == 1 & !is.BIPOC)), "/",  nrow(dplyr::filter(benefits_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(benefits_data, InsuranceFromAnySource == 1 & is.BIPOC)), "/",  nrow(dplyr::filter(benefits_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- coc_exits |>
          dplyr::filter(ProjectID == project_id) |> 
          dplyr::mutate(is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE),
                        Excluded_Range = lubridate::interval(EntryDate + lubridate::days(1),
                                                             pmin(ExitDate + lubridate::days(14),
                                                                  lubridate::int_start(operatingQuarter) - lubridate::days(1))))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No possible returns"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        returns_universe <- hmisExtract$entry |>
          dplyr::rename(PossibleReturnDate = EntryDate) |>
          dplyr::filter(PersonalID %in% project_data$PersonalID,
                        !EnrollmentID %in% project_data$EnrollmentID) |>
          dplyr::left_join(dplyr::select(hmisExtract$project, ProjectID, ProjectType), by = c("ProjectID")) |>
          dplyr::filter(ProjectType %in% c(0:4, 8:10, 13)) |>
          dplyr::select(PersonalID, PossibleReturnDate) |>
          dplyr::mutate(Entered.On = paste0("Entered.On ", PossibleReturnDate, " ", (\(.) dplyr::row_number(.))())) |>
          tidyr::pivot_wider(names_from = Entered.On, values_from = PossibleReturnDate)
        
        returnStartColumn <- length(project_data) + 1
        returnEndColumn <- length(project_data) + length(returns_universe) - 1
        
        is_within_six_months <- function(Exit_Date, ReEntry_Date, excluded_window)
        {
          returnWindow <- lubridate::interval(Exit_Date, Exit_Date + lubridate::dmonths(6))
          
          return(ifelse(!is.na(ReEntry_Date) & ReEntry_Date != (lubridate::int_start(excluded_window) - lubridate::days(1)),
                        ifelse(!lubridate::`%within%`(ReEntry_Date, excluded_window),
                               lubridate::`%within%`(ReEntry_Date, returnWindow),
                               FALSE),
                        FALSE))
        }
        
        # Must convert rowwise df back to tibble so tally() will work...
        project_data <- tibble::as_tibble(dplyr::`%>%`(dplyr::`%>%`(dplyr::`%>%`(project_data, dplyr::left_join(returns_universe, by = c("PersonalID"))), dplyr::rowwise()), dplyr::mutate(Returned.to.Homelessness.Within.6.Months = dplyr::if_else(any(is_within_six_months(ExitDate, dplyr::c_across(names(.[returnStartColumn]):names(.[returnEndColumn])), Excluded_Range), na.rm = TRUE), TRUE, FALSE))))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> 
            dplyr::select(ProjectID,
                          PersonalID,
                          EnrollmentID,
                          EntryDate,
                          MoveInDate,
                          ExitDate,
                          tidyselect::contains("Entered.On"),
                          Race_Ethnicity,
                          is.BIPOC,
                          Returned.to.Homelessness.Within.6.Months) |>
            download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        bipoc_result <- as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(dplyr::tally(project_data, Returned.to.Homelessness.Within.6.Months & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re6 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_psh(project),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-6` = vapply(.data$`Project ID`, run_metric_re6, FUN.VALUE = type_to_return())))
  }
  
  # Racial Equity Metric-7 =============================================================================================
  # RRH: "White/BIPOC Income Growth" (a.k.a. EM-5) ----
  metric_re_7 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-RE7"
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(9), percent(19), percent(20)),
                             points = c(8, 4, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = 0))
    }
    
    calculate_tier <- function(input, project_type)
    {
      input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "rrh" = dplyr::case_when(input < 0 | dplyr::between(input, 0, rrh_scores$Threshold[which(rrh_scores$Tier == 1)]) ~ 1,
                                      dplyr::between(input, one_percent_above(rrh_scores$Threshold[which(rrh_scores$Tier == 1)]), rrh_scores$Threshold[which(rrh_scores$Tier == 2)]) ~ 2,
                                      dplyr::between(input, rrh_scores$Threshold[which(rrh_scores$Tier == 3)], percent(100)) ~ 3,
                                      .default = NA_real_),
             NA)
    }
    
    calculate_for_rrh <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric.
        assessment_data <- income_data_full |>
          dplyr::mutate(across(contains("Date"), as.Date)) |> 
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage == 5,
                        lubridate::`%within%`(InformationDate, reportingPeriod)) |>
          dplyr::arrange(PersonalID, desc(InformationDate)) |> 
          dplyr::distinct(PersonalID, .keep_all = TRUE) |> 
          dplyr::select(PersonalID,
                        EnrollmentID,
                        Latest_Annual_Assessment_Date = InformationDate)
        
        exit_data <- project_data |> 
          dplyr::filter(lubridate::`%within%`(ExitDate, reportingPeriod)) |> 
          dplyr::select(PersonalID, EnrollmentID, ExitDate)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        if(nrow(assessment_data) == 0 & nrow(exit_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        project_data <- project_data |> 
          dplyr::filter(PersonalID %in% unique(c(assessment_data$PersonalID, exit_data$PersonalID)))
        
        income_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID)
        
        income_end_amounts <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::distinct(PersonalID, .keep_all = TRUE) |>
          dplyr::ungroup() |>
          dplyr::rename(End.Income = TotalMonthlyIncome) |>
          dplyr::filter(DataCollectionStage != 1) |> # NEW 2024-07-03, making sure people with only entries don't have it counted for start & end ----
          dplyr::select(PersonalID, End.Income)
        
        income_start_amounts0 <- income_data |>
          dplyr::group_by(PersonalID) |>
          dplyr::arrange(-dplyr::desc(PersonalID), dplyr::desc(InformationDate)) |>
          dplyr::mutate(Income_Observations = dplyr::n(),
                        Income_Row = dplyr::row_number())
        
        income_start_amounts_one_observation <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations == 1) |> 
          dplyr::ungroup()
        
        income_start_amounts_multiple_observations <- income_start_amounts0 |> 
          dplyr::filter(Income_Observations > 1) |> 
          dplyr::slice(2) |> 
          dplyr::ungroup()
        
        income_start_amounts <- income_start_amounts_one_observation |> 
          dplyr::bind_rows(income_start_amounts_multiple_observations) |> 
          dplyr::rename(Start.Income = TotalMonthlyIncome) |>
          dplyr::select(PersonalID, Start.Income)
        
        project_data <- project_data |>
          dplyr::left_join(income_start_amounts, by = c("PersonalID")) |>
          dplyr::left_join(income_end_amounts, by = c("PersonalID")) |>
          dplyr::mutate(Increased.or.Maintained.Income = dplyr::case_when(Start.Income == 0 & End.Income == 0 ~ "Zero",
                                                                          is.na(Start.Income) | is.na(End.Income) ~ "Missing",
                                                                          End.Income == Start.Income ~ "Sustained",
                                                                          End.Income > Start.Income ~ "Increased",
                                                                          .default = "Decreased"),
                        is.BIPOC = dplyr::if_else(Race_Ethnicity == "White",
                                                  FALSE,
                                                  TRUE))
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return NA if all clients are White or all clients are BIPOC.
        na_message <- "All clients are either White or BIPOC"
        
        if(all(project_data$is.BIPOC) | all(!project_data$is.BIPOC))
        {
          return(return_na())
        }
        
        # Return the score for this metric normally
        bipoc_result <- as.double(dplyr::tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & is.BIPOC)) / nrow(dplyr::filter(project_data, is.BIPOC))
        
        non_bipoc_result <- as.double(dplyr::tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & !is.BIPOC)) / nrow(dplyr::filter(project_data, !is.BIPOC))
        
        return(switch(function_mode,
                      "Numeric Scores" = rrh_scores$Points[which(rrh_scores$Tier == calculate_tier(non_bipoc_result - bipoc_result, "rrh"))],
                      "Text Results"   = paste0("White Result (", as.double(tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & !is.BIPOC)), "/",  nrow(dplyr::filter(project_data, !is.BIPOC)), ") - ",
                                                "BIPOC Result (", as.double(tally(project_data, Increased.or.Maintained.Income %in% c("Increased", "Sustained") & is.BIPOC)), "/",  nrow(dplyr::filter(project_data, is.BIPOC)), ") = ",
                                                round(non_bipoc_result - bipoc_result, 4) * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_re7 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = na_type(),
             "rrh" = calculate_for_rrh(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric RE-7` = vapply(.data$`Project ID`, run_metric_re7, FUN.VALUE = type_to_return())))
  }
  
  # CLIENT FEEDBACK METRIC-1 ===========================================================================================
  # PSH & RRH: "Response Rate to Client Feedback Survey" ----
  metric_cf_1 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-CF1"
    
    scores <- set_scores(tiers = 6,
                         thresholds = c(percent(50), percent(40), percent(30), percent(20), percent(10), percent(9)),
                         points = c(5, 4, 3, 2, 1, 0))
    
    calculate_tier <- function(input)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      dplyr::case_when(dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 4)], one_percent_below(scores$Threshold[which(scores$Tier == 3)])) ~ 4,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 5)], one_percent_below(scores$Threshold[which(scores$Tier == 4)])) ~ 5,
                       dplyr::between(rounded_input, 0, scores$Threshold[which(scores$Tier == 6)]) ~ 6,
                       .default = NA_real_)
    }
    
    if (!is.null(return_max_points))
    {
      # These are bonus points, so maximum is zero
      return(0)
    }
    
    calculate_for_ph <- function(project_id)
    {
      project_surveys_completed <- .nonHmisData |>
        dplyr::filter(ProjectID == project_id) |> 
        dplyr::pull(`SurveysReceived(CF-1)`)
      
      project_data <- scorecard_universe |>
        dplyr::filter(ProjectID == project_id,
                      is.na(ExitDate) | ExitDate >= lubridate::int_start(operatingQuarter),
                      Age >= 18) |>
        dplyr::mutate(TotalCompletedSurveys = project_surveys_completed)
      
      # Return NA if there are no applicable clients for this metric
      na_message <- "No applicable clients"
      
      if (nrow(project_data) == 0)
      {
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        return(return_na())
      }
      
      # Save the results to 'Downloads' folder, if applicable
      if (.save_result_tables)
      {
        project_data |> download_results()
      }
      
      # Continue calculation
      survey_response_rate <- project_surveys_completed / nrow(project_data)
      
      # Diagnostic message
      if (.show_diagnostics)
      {
        switch(function_mode,
               "Numeric Scores" = cli::cli_inform(c("*" = "{this_project('name', project_id, hmis_extract = hmisExtract)} ({project_id}) Score: {.val {scores$Points[which(scores$Tier == calculate_tier(survey_response_rate))]}}.")),
               "Text Results"   = cli::cli_inform(c("*" = "{this_project('name', project_id, hmis_extract = hmisExtract)} ({project_id}): {.val {project_surveys_completed}} / {.val {nrow(project_data)}} client{?s} completed a survey.")),
               na_type())
      }
      
      # Return the score for this metric normally
      return(switch(function_mode,
                    "Numeric Scores" = scores$Points[which(scores$Tier == calculate_tier(survey_response_rate))],
                    "Text Results"   = cli::pluralize("{project_surveys_completed} / {nrow(project_data)} client{?s} completed a survey"),
                    na_type()))
    }
    
    run_metric_cf1 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_ph(project),
             "rrh" = calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric CF-1` = vapply(.data$`Project ID`, run_metric_cf1, FUN.VALUE = type_to_return())))
  }
  
  # CLIENT FEEDBACK METRIC-2 ===========================================================================================
  # PSH & RRH: "Incorporation of Client Feedback Into Programming" ----
  metric_cf_2 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-CF2"
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = 0,
                    "psh" = 0,
                    NA_real_))
    }
    
    run_metric_cf2 <- function(project)
    {
      return(na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric CF-2` = vapply(.data$`Project ID`, run_metric_cf2, FUN.VALUE = type_to_return())))
  }
  
  # DATA QUALITY METRIC-1 ==============================================================================================
  # PSH & RRH: "Quarterly Data Completeness of UDEs" ----
  metric_dq_1 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    # See pg. 26: https://files.hudexchange.info/resources/documents/HMIS-Standard-Reporting-Terminology-Glossary-2024.pdf
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-DQ1"
    
    psh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(98), percent(95), percent(94)),
                             points = c(2, 1, 0))
    
    rrh_scores <- set_scores(tiers = 3,
                             thresholds = c(percent(98), percent(95), percent(94)),
                             points = c(2, 1, 0))
    
    if (!is.null(return_max_points))
    {
      return(switch(return_max_points,
                    "rrh" = rrh_scores$Points[which(rrh_scores$Tier == 1)],
                    "psh" = psh_scores$Points[which(psh_scores$Tier == 1)]))
    }
    
    calculate_tier <- function(input, project_type)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      switch(project_type,
             "psh" = dplyr::case_when(dplyr::between(rounded_input, psh_scores$Threshold[which(psh_scores$Tier == 1)], percent(100)) ~ 1,
                                      dplyr::between(rounded_input, psh_scores$Threshold[which(psh_scores$Tier == 2)], one_percent_below(psh_scores$Threshold[which(psh_scores$Tier == 1)])) ~ 2,
                                      dplyr::between(rounded_input, 0, psh_scores$Threshold[which(psh_scores$Tier == 3)]) ~ 3,
                                      .default = NA_real_),
             "rrh" = dplyr::case_when(dplyr::between(rounded_input, rrh_scores$Threshold[which(rrh_scores$Tier == 1)], percent(100)) ~ 1,
                                      dplyr::between(rounded_input, rrh_scores$Threshold[which(rrh_scores$Tier == 2)], one_percent_below(rrh_scores$Threshold[which(rrh_scores$Tier == 1)])) ~ 2,
                                      dplyr::between(rounded_input, 0, rrh_scores$Threshold[which(rrh_scores$Tier == 3)]) ~ 3,
                                      .default = NA_real_),
             NA)
    }
    
    # These are the columns that are what HUD collectively defines as: "[income and sources]"
    Income_and_Sources <- c(names(income_data_full[1:37]))
    
    # These are the actual yes/no possible income sources within [income and sources]
    IncomeSources <- c(names(income_data_full[seq(7, 35, by = 2)]))
    
    calculate_data_completeness <- function(project_data_entry)
    {
      intervention <- this_project("type", project_data_entry$ProjectID[1], hmis_extract = hmisExtract)
      
      project_income_file <- income_data_full |>
        dplyr::filter(EnrollmentID %in% project_data_entry$EnrollmentID) |>
        dplyr::left_join(dplyr::select(project_data_entry,
                                       PersonalID,
                                       RelationshipToHoH,
                                       Age,
                                       EnrollmentID,
                                       EntryDate,
                                       ExitDate),
                         by = c("EnrollmentID", "PersonalID")) |>
        add_latest_annual_assessment() |>
        dplyr::mutate(AnnualAssessmentWindow = dplyr::if_else(!is.na(Latest.Annual.Assessment.Due),
                                                              lubridate::interval(Latest.Annual.Assessment.Due - lubridate::days(30),
                                                                                  Latest.Annual.Assessment.Due + lubridate::days(30)),
                                                              NA)) |>
        dplyr::arrange(PersonalID, desc(EnrollmentID), desc(DataCollectionStage), desc(InformationDate)) |>
        dplyr::group_by(PersonalID, EnrollmentID) |>
        dplyr::distinct(DataCollectionStage == 5, .keep_all = TRUE) |>
        dplyr::ungroup() |>
        dplyr::rowwise() |>
        dplyr::mutate(psde_IncomeAndSources_4.02_Start = dplyr::if_else(DataCollectionStage == 1 & any(RelationshipToHoH == 1, Age >= 18) & intervention != "sso-ce", # !!! NEED TO CODE A WAY TO DETERMINE SSO vs SSO-CE !!!
                                                                        dplyr::case_when(IncomeFromAnySource == 99 ~ 1,
                                                                                         all(is.na(Income_and_Sources)) ~ 1,
                                                                                         .default = 0),
                                                                        0),
                      
                      psde_IncomeAndSources_4.02B_AnnualAssessment = dplyr::if_else(DataCollectionStage == 5 & any(RelationshipToHoH == 1, Age >= 18) & intervention != "sso-ce", # !!! NEED TO CODE A WAY TO DETERMINE SSO vs SSO-CE !!!
                                                                                    dplyr::if_else(is.na(IncomeFromAnySource) | IncomeFromAnySource == 99, 1, 0),
                                                                                    0),
                      
                      psde_IncomeAndSources_4.02_Exit = dplyr::if_else(DataCollectionStage == 3 & any(RelationshipToHoH == 1, Age >= 18) & intervention != "sso-ce", # !!! NEED TO CODE A WAY TO DETERMINE SSO vs SSO-CE !!!
                                                                       dplyr::case_when(IncomeFromAnySource == 99 ~ 1,
                                                                                        all(is.na(Income_and_Sources)) ~ 1,
                                                                                        .default = 0),
                                                                       0)) |>
        dplyr::ungroup() |>
        select(PersonalID,
               EnrollmentID,
               psde_IncomeAndSources_4.02_Start,
               psde_IncomeAndSources_4.02B_AnnualAssessment,
               psde_IncomeAndSources_4.02_Exit)
      
      project_data_entry <- project_data_entry |>
        dplyr::rowwise() |>
        dplyr::mutate(ude_Name_3.01 = dplyr::case_when(NameDataQuality == 99 ~ 1,
                                                       any(c(is.na(FirstName), is.na(LastName))) ~ 1,
                                                       .default = 0),
                      
                      ude_SSN_3.02 = dplyr::if_else(!SSN | is.na(SSN), 1, 0),
                      
                      ude_DOB_3.03 = dplyr::case_when(is.na(DOB) ~ 1,
                                                      DOBDataQuality == 99 ~ 1,
                                                      DOBDataQuality == 2 ~ 1,
                                                      .default = 0),
                      
                      ude_RaceEthnicity_3.04 = dplyr::if_else(RaceNone == 99, 1, 0),
                      
                      ude_Gender_3.06 = dplyr::if_else(GenderNone == 99, 1, 0),
                      
                      ude_VeteranStatus_3.07 = dplyr::if_else(is.na(VeteranStatus) | VeteranStatus == 99, 1, 0),
                      
                      ude_ProjectStartDate_3.10 = dplyr::if_else(is.na(EntryDate), 1, 0),
                      
                      ude_DisablingCondition_3.08 = dplyr::if_else(is.na(DisablingCondition) | DisablingCondition == 99, 1, 0),
                      
                      ude_Destination_3.12 = dplyr::case_when(Destination == 99 ~ 1,
                                                              !is.na(ExitDate) & is.na(Destination) ~ 1,
                                                              .default = 0),
                      
                      ude_RelationshipToHOH_3.15 = dplyr::if_else(is.na(RelationshipToHoH) | RelationshipToHoH == 99, 1, 0),
                      
                      ude_EnrollmentCoC_3.16 = dplyr::if_else(RelationshipToHoH == 1 & is.na(EnrollmentCoC), 1, 0)) |>
        dplyr::ungroup() |>
        dplyr::left_join(project_income_file, by = c("EnrollmentID", "PersonalID"))
      
      return(project_data_entry)
    }
    
    calculate_for_ph <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        intervention <- ECHO::this_project("type", project_id, hmis_extract = hmisExtract)
        
        assign("scores", get(paste0(intervention, "_scores")))
        
        project_data <- data_universe_complete |>
          dplyr::filter(ProjectID == project_id)
        
        # Return NA if there are no applicable clients for this metric
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        project_data <- project_data |> 
          calculate_data_completeness() |> 
          select(PersonalID,
                 tidyselect::starts_with("ude_") | tidyselect::starts_with("psde_")) |> 
          janitor::adorn_totals()
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |>  download_results()
        }
        
        # Return the score for this metric normally
        total_possible <-  project_data |> (\(.) (nrow(.) - 1) * (ncol(.) - 1))()
        
        data_not_completed <- project_data |> (\(.) sum(.[nrow(.),2:ncol(.)]))()
        
        data_completeness <- round(1 - (data_not_completed / total_possible), digits = 2)
        
        return(switch(function_mode,
                      "Numeric Scores" = scores$Points[which(scores$Tier == calculate_tier(data_completeness, intervention))],
                      "Text Results" = paste0("Data Completeness = ", data_completeness * 100, "%"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_dq1 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_ph(project),
             "rrh" = calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric DQ-1` = vapply(.data$`Project ID`, run_metric_dq1, FUN.VALUE = type_to_return())))
  }
  
  # DATA QUALITY METRIC-2a =============================================================================================
  # PSH & RRH: "Timeliness of Data Entry: Entries & Exits" ----
  metric_dq_2a <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-DQ2a"
    
    scores <- set_scores(tiers = 5,
                         thresholds = c(percent(99), percent(96), percent(91), percent(85), percent(84)),
                         points = c(4, 3, 2, 1, 0))
    
    if (!is.null(return_max_points))
    {
      return(scores$Points[which(scores$Tier == 1)])
    }
    
    calculate_tier <- function(input)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      dplyr::case_when(dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 4)], one_percent_below(scores$Threshold[which(scores$Tier == 3)])) ~ 4,
                       dplyr::between(rounded_input, 0, scores$Threshold[which(scores$Tier == 5)]) ~ 5,
                       .default = NA_real_)
    }
    
    lookbackWindow = lubridate::interval(lubridate::int_start(operatingQuarter) - lubridate::days(6),
                                         lubridate::int_end(operatingQuarter) + lubridate::days(6))
    
    entryExitWindow <- lubridate::interval(lubridate::int_start(lookbackWindow), lubridate::int_end(operatingQuarter))
    
    calculate_for_ph <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1,
                        lubridate::`%within%`(EntryDate, entryExitWindow) | lubridate::`%within%`(ExitDate, entryExitWindow)) |>
          dplyr::select(PersonalID, EnrollmentID, EntryDate, ExitDate)
        
        assessment_data <- income_data_full |>
          dplyr::left_join(project_data, by = c("PersonalID", "EnrollmentID")) |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage %in% c(1, 3),
                        lubridate::`%within%`(DateCreated, lookbackWindow)
                        | (DataCollectionStage == 1 & lubridate::`%within%`(EntryDate, entryExitWindow))
                        | (DataCollectionStage == 3 & lubridate::`%within%`(ExitDate, entryExitWindow))) |>
          dplyr::mutate(dplyr::across(tidyselect::contains("Date"), as.Date),
                        Six.Day.Grace.Period = dplyr::case_match(DataCollectionStage,
                                                                 1 ~ lubridate::interval(EntryDate - lubridate::days(6),
                                                                                         EntryDate + lubridate::days(6)),
                                                                 
                                                                 3 ~ lubridate::interval(ExitDate - lubridate::days(6),
                                                                                         ExitDate + lubridate::days(6)),
                                                                 
                                                                 .default = NA),
                        Data.Entered.On.Time = lubridate::`%within%`(DateCreated, Six.Day.Grace.Period),
                        Days.Away.From.Due.Date = dplyr::case_match(DataCollectionStage,
                                                                    1 ~ DateCreated - EntryDate,
                                                                    3 ~ DateCreated - ExitDate,
                                                                    .default = NA),
                        Pre.Cutoff.Data.On.Time = dplyr::case_when(DataCollectionStage == 1 & EntryDate < lubridate::int_start(operatingQuarter) & Data.Entered.On.Time ~ TRUE,
                                                                   DataCollectionStage == 3 & ExitDate < lubridate::int_start(operatingQuarter) & Data.Entered.On.Time ~ TRUE,
                                                                   .default = FALSE)) |> 
          filter(!Pre.Cutoff.Data.On.Time) |> 
          dplyr::select(PersonalID,
                        DataCollectionStage,
                        InformationDate,
                        DateCreated,
                        DateUpdated,
                        EntryDate,
                        ExitDate,
                        Six.Day.Grace.Period,
                        Days.Away.From.Due.Date,
                        Data.Entered.On.Time)
        
        # Return NA if there are no applicable clients for this metric
        na_message <- "No applicable entries/exits in the reporting period"
        
        if (nrow(assessment_data) == 0)
        {
          if (.save_result_tables)
          {
            assessment_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          assessment_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = scores$Points[which(scores$Tier == calculate_tier(sum(assessment_data$Data.Entered.On.Time) / sum(nrow(assessment_data))))],
                      "Text Results"   = cli::pluralize("{sum(assessment_data$Data.Entered.On.Time)} / {nrow(assessment_data)} Entry/Exit Record{?s} Entered On Time ({round((sum(assessment_data$Data.Entered.On.Time) * 100) / nrow(assessment_data))}%)"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_dq2a <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_ph(project),
             "rrh" = calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric DQ-2a` = vapply(.data$`Project ID`, run_metric_dq2a, FUN.VALUE = type_to_return())))
  }
  
  # DATA QUALITY METRIC-2b =============================================================================================
  # PSH & RRH: "Timeliness of Data Entry: Annual Assessments" ----
  metric_dq_2b <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-DQ2b"
    
    scores <- set_scores(tiers = 5,
                         thresholds = c(percent(99), percent(96), percent(91), percent(85), percent(84)),
                         points = c(4, 3, 2, 1, 0))
    
    if (!is.null(return_max_points))
    {
      return(scores$Points[which(scores$Tier == 1)])
    }
    
    calculate_tier <- function(input)
    {
      rounded_input <- round_as_percent(input, as_decimal = TRUE)
      
      dplyr::case_when(dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 1)], percent(100)) ~ 1,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 2)], one_percent_below(scores$Threshold[which(scores$Tier == 1)])) ~ 2,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 3)], one_percent_below(scores$Threshold[which(scores$Tier == 2)])) ~ 3,
                       dplyr::between(rounded_input, scores$Threshold[which(scores$Tier == 4)], one_percent_below(scores$Threshold[which(scores$Tier == 3)])) ~ 4,
                       dplyr::between(rounded_input, 0, scores$Threshold[which(scores$Tier == 5)]) ~ 5,
                       .default = NA_real_)
    }
    
    operatingQuarter_start <- lubridate::int_start(operatingQuarter)
    
    operatingQuarter_end <- lubridate::int_end(operatingQuarter)
    
    adjustedAssessment_operatingQuarter <- lubridate::interval(operatingQuarter_start - lubridate::days(30),
                                                               operatingQuarter_end - lubridate::days(30))
    
    lookbackWindow <- lubridate::interval(lubridate::int_start(adjustedAssessment_operatingQuarter) - lubridate::days(30),
                                          lubridate::int_end(operatingQuarter))
    if (.save_result_tables & function_mode == "Text Results")
    {
      write(paste0("\nAnnual Assessment Due Dates = ", format(as.Date(lubridate::int_start(adjustedAssessment_operatingQuarter)), "%B %d, %Y"), " - ", format(as.Date(lubridate::int_end(adjustedAssessment_operatingQuarter)), "%B %d, %Y"),
                   "\n\nAnnual Assessment Data Window = ", format(as.Date(lubridate::int_start(lookbackWindow)), "%B %d, %Y"), " - ", format(as.Date(lubridate::int_end(lookbackWindow)), "%B %d, %Y")),
            file = paste0(results_directory, "/report-parameters.txt"),
            append = TRUE)
    }
    
    calculate_for_ph <- function(project_id)
    {
      is_safe_project <- vsp_check(project_id)
      
      run_for_hmis_data <- function()
      {
        project_data <- scorecard_universe |>
          dplyr::filter(ProjectID == project_id,
                        HOH == 1) |>
          dplyr::select(PersonalID,
                        EnrollmentID,
                        EntryDate)
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' applicable clients for this metric
        project_data <- project_data |> 
          add_latest_annual_assessment() |>
          dplyr::mutate(Annual.Assessment.Grace.Period = lubridate::interval(Latest.Annual.Assessment.Due - lubridate::days(30),
                                                                             Latest.Annual.Assessment.Due + lubridate::days(30))) |>
          dplyr::filter(lubridate::`%within%`(Latest.Annual.Assessment.Due, adjustedAssessment_operatingQuarter))
        
        # Return NA if there are no applicable clients for this metric.
        na_message <- "No annual assessments due"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Continue analysis if there 'are' annual assessments due
        assessment_data <- income_data_full |>
          dplyr::filter(EnrollmentID %in% project_data$EnrollmentID,
                        DataCollectionStage == 5) |>
          dplyr::mutate(across(contains("Date"), as.Date)) |> 
          dplyr::arrange(PersonalID, desc(DateCreated)) |> 
          dplyr::distinct(PersonalID, .keep_all = TRUE)
        
        project_data <- project_data |> 
          left_join(assessment_data, by = c("PersonalID", "EnrollmentID")) |> 
          dplyr::mutate(Annual.Assessment.On.Time = dplyr::if_else(lubridate::`%within%`(DateCreated, Annual.Assessment.Grace.Period),
                                                                   TRUE,
                                                                   FALSE),
                        Days.Away.From.Due.Date = abs(DateCreated - Latest.Annual.Assessment.Due)) |> 
          dplyr::select(PersonalID,
                        EntryDate,
                        Latest.Annual.Assessment.Due,
                        Latest_Annual_Assessment_Date = DateCreated,
                        Annual.Assessment.Grace.Period,
                        Days.Away.From.Due.Date,
                        Annual.Assessment.On.Time)
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = scores$Points[which(scores$Tier == calculate_tier(sum(project_data$Annual.Assessment.On.Time, na.rm = TRUE) / nrow(project_data)))],
                      "Text Results"   = cli::pluralize("{sum(project_data$Annual.Assessment.On.Time, na.rm = TRUE)} / {nrow(project_data)} Annual Assesment{?s} Completed On Time ({round((sum(project_data$Annual.Assessment.On.Time, na.rm = TRUE) * 100) / nrow(project_data))}%)"),
                      na_type()))
      }
      
      run_for_safe_data <- function()
      {
        na_message <- "..."
        
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results" = NA_character_,
                      na_type()))
      }
      
      # Run the correct function above based on whether a project uses HMIS or not
      ifelse(is_safe_project,
             run_for_safe_data(),
             run_for_hmis_data())
    }
    
    run_metric_dq2b <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_ph(project),
             "rrh" = calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric DQ-2b` = vapply(.data$`Project ID`, run_metric_dq2b, FUN.VALUE = type_to_return())))
  }
  
  # DATA QUALITY METRIC-3 ==============================================================================================
  # PSH & RRH: "Internal Data Audit" ----
  metric_dq_3 <- function(.data = NULL, ..., return_max_points = NULL, function_mode)
  {
    rlang::check_dots_empty()
    
    metric_subdirectory <- "Metric-DQ3"
    
    if (!is.null(return_max_points))
    {
      # Max points = 0 because these are bonus points.
      return(0)
    }
    
    data_audit_universe <- hmisExtract$entry |>
      dplyr::left_join(hmisExtract$exit,    by = c("EnrollmentID", "PersonalID")) |>
      dplyr::left_join(hmisExtract$client,  by = c("PersonalID")) |>
      dplyr::left_join(hmisExtract$project, by = c("ProjectID")) |>
      dplyr::filter(ProjectID %in% cocProjects,
                    EntryDate <= lubridate::int_end(operatingQuarter),
                    is.na(ExitDate) | ExitDate >= int_start(operatingQuarter))
    
    calculate_for_ph <- function(project_id)
    {
      project_submission <- .nonHmisData |>
        dplyr::filter(ProjectID == project_id)
      
      project_completed_data_audit <- project_submission$`InternalDataAuditCompleted(DQ-3)`
      
      project_attested_no_bad_movins <- NA
      
      if (project_completed_data_audit)
      {
        project_attested_no_bad_movins <- project_submission$`ProjectAttestsNoReturnsBeforeEnrollment(DQ-3)`
      }
      
      double_check_entry_dates <- function()
      {
        project_data <- data_audit_universe |>
          dplyr::filter(ProjectID == project_id) |>
          dplyr::mutate(MoveInDateError = dplyr::if_else(MoveInDate < EntryDate, TRUE, FALSE))
        
        # Return NA if there are no applicable clients for this metric
        na_message <- "No applicable clients"
        
        if (nrow(project_data) == 0)
        {
          if (.save_result_tables)
          {
            project_data |> download_results()
          }
          
          return(return_na())
        }
        
        # Save the results to 'Downloads' folder, if applicable
        if (.save_result_tables)
        {
          project_data |> download_results()
        }
        
        # Return the score for this metric normally
        return(switch(function_mode,
                      "Numeric Scores" = ifelse(any(project_data$MoveInDateError, na.rm = TRUE), 0, 3),
                      "Text Results"   = ifelse(any(project_data$MoveInDateError, na.rm = TRUE), 
                                                cli::pluralize("Project had {sum(project_data$MoveInDateError, na.rm = TRUE)} move-in date{?s} before enrollment dates"),
                                                paste0("Project had no move-in dates before enrollment dates")),
                      na_type()))
      }
      
      do_not_apply_bonus <- function()
      {
        return(switch(function_mode,
                      "Numeric Scores" = NA_real_,
                      "Text Results"   = paste0("Project did not complete internal data audit"),
                      na_type()))
      }
      
      ifelse(project_completed_data_audit & project_attested_no_bad_movins,
             double_check_entry_dates(),
             do_not_apply_bonus())
    }
    
    run_metric_3 <- function(project)
    {
      project_type <- ECHO::this_project("type", project, hmis_extract = hmisExtract)
      
      switch(project_type,
             "psh" = calculate_for_ph(project),
             "rrh" = calculate_for_ph(project),
             na_type())
    }
    
    return(.data |> dplyr::mutate(`Metric DQ-3` = vapply(.data$`Project ID`, run_metric_3, FUN.VALUE = type_to_return())))
  }
  
  #### GENERATE TEXT RESULTS TABLE #####################################################################################
  run_text_results <- function(.data, metrics)
  {
    for (n in 1:length(metrics))
    {
      use_function <- get(gsub("-| ", "_", tolower(metrics[n])))
      
      .data <- use_function(.data, function_mode = "Text Results")
      
      cli::cli_progress_output(cli::cli_text("Calculating {.val {metrics[n]}}"),
                               .envir = scorecard_function_env)
      
      cli::cli_progress_update(.envir = scorecard_function_env)
      
      rm(n)
    }
    
    return(.data)
  }
  
  results <- scorecards |> 
    run_text_results(scorecard_measures()) |> 
    dplyr::select(-PIP, -`Project Score`, -VSP, -`Project Name`) |> 
    dplyr::rename_with(\(x) paste0(x, ": Result"), dplyr::contains("Metric"))
  
  #### GENERATE NUMERICAL SCORES TABLE #################################################################################
  run_numeric_scores <- function(.data, metrics)
  {
    for (n in 1:length(metrics))
    {
      use_function <- get(gsub("-| ", "_", tolower(metrics[n])))
      
      .data <- use_function(.data, function_mode = "Numeric Scores") |>
        dplyr::mutate(Max.Points = dplyr::case_when(ECHO::this_project("type", `Project ID`, hmis_extract = hmisExtract) == "psh"
                                                    ~ dplyr::case_when(is.na(.data[[metrics[n]]]) ~ Max.Points - use_function(return_max_points = "psh"),
                                                                       .default = Max.Points),
                                                    
                                                    ECHO::this_project("type", `Project ID`, hmis_extract = hmisExtract) == "rrh"
                                                    ~ dplyr::case_when(is.na(.data[[metrics[n]]]) ~ Max.Points - use_function(return_max_points = "rrh"),
                                                                       .default = Max.Points),
                                                    
                                                    .default = NA))
      
      cli::cli_progress_output(cli::cli_text("Scoring {.val {metrics[n]}}"),
                               .envir = scorecard_function_env)
      
      cli::cli_progress_update(.envir = scorecard_function_env)
      
      rm(n)
    }
    
    return(.data)
  }
  
  scores <- scorecards |>
    dplyr::mutate(Max.Points = dplyr::if_else(these_project("types", `Project ID`, hmis_extract = hmisExtract) == "rrh", 100, 93)) |>
    run_numeric_scores(scorecard_measures())|>
    dplyr::mutate(`Project Score` = rowSums(dplyr::across(`Metric PM-1`:`Metric DQ-3`, \(x) tidyr::replace_na(x, 0)))) |>
    dplyr::rename_with(\(x) paste0(x, ": Score"), dplyr::contains("Metric")) |> 
    dplyr::mutate(PIP = ifelse(((`Project Score` * 100) / Max.Points) < 60, TRUE, FALSE))
  
  #### JOIN THE SCORES & RESULTS TABLES TO CREATE THE FINAL SCORECARD SHEET ############################################
  order_columns <- function(.data)
  {
    for (n in 1:length(scorecard_measures()))
    {
      .data <- .data |> 
        dplyr::relocate(paste0(scorecard_measures()[n], ": Result"), .before = paste0(scorecard_measures()[n], ": Score"))
      
      rm(n)
    }
    
    return(.data)
  }
  
  coc_scorecard_sheet <- scores |> 
    dplyr::left_join(results, by = "Project ID") |>
    order_columns() |> 
    dplyr::rename(`Total Points Earned` = `Project Score`,
                  `Max Points` = Max.Points) |> 
    dplyr::mutate(`Project Score` = dplyr::if_else(`Max Points` > 0,
                                                   paste0(round((`Total Points Earned` * 100) / `Max Points`), "%"),
                                                   NA_character_),
                  Component = toupper(these_project("types", `Project ID`, hmis_extract = hmisExtract))) |> 
    dplyr::relocate(Component, .before = VSP) |> 
    dplyr::relocate(`Max Points`:`Project Score`, .before = PIP)
  
  if (.save_result_tables)
  {
    readr::write_csv(coc_scorecard_sheet, paste0(results_directory, "/scorecards.csv"))
  }
  
  #### OPTION 1: RETURN THE SCORECARD SHEET NORMALLY IN R ##############################################################
  if (!.straight_to_excel)
  {
    message("\nScorecard generation complete.\n")
    
    # Stop Timer if Showing Diagnostics
    if(.show_diagnostics)
    {
      tictoc::toc()
    }
    
    return(coc_scorecard_sheet)
  }
  
  #### OPTION 2: RETURN THE SCORECARD SHEET IN MS EXCEL ################################################################
  message("\nScorecard generation complete.")
  message("\nSending results to an Excel spreadsheet...\n")
  
  # Stop Timer if Showing Diagnostics
  if(.show_diagnostics)
  {
    tictoc::toc()
  }
  
  return(invisible(show_in_excel(coc_scorecard_sheet)))
}
