#' Generate the Enrollment and/or Client Universe(s)
#'
#' Create a tibble of enrollments that overlap with the reporting period.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the HMIS CSV/XML extract list of tibbles.
#' @param universe_type which type of universe: `"enrollment"`, `"client"`, or
#'   `"both"`. (If both, you can use dollar sign notation to get which universe
#'   you want from the object created: i.e., `your_universe$enrollment` /
#'   `your_universe$client`.)
#' @param timeframe a lubridate interval: the date the lookback period begins
#'   through the date the lookback period ends. (If you enter `"ytd"` instead,
#'   the function will use January first of the current year through the last
#'   day of the previous full month.)
#' @param calendar_year_start the calendar year for the analysis: this defaults
#'   to `calendarYearStart` if it is in your environment, or January first of
#'   the year of the start of the reporting period if `calendarYearStart` is not
#'   in your environment. Otherwise, you can specify it as a string:
#'   `"YYYY-01-01"`.
#' @param old_version Defaults to `FALSE.` If set to `TRUE` it will use the
#'   original code which does not auto filter out duplicate projects in the
#'   projects file.
#' @param .FY The HMIS Data Standards Fiscal Year (entered numerically with two
#'   digits, i.e., `22` for FY22). _**Defaults to FY24.**_ Can be set backwards
#'   up to FY22 for backwards compatibility with older HMIS Extracts.
#'
#' @return A tibble: the enrollment universe.
#' @export
make_universe <- function(hmis_extract = NULL, ...,
                          universe_type,
                          timeframe,
                          calendar_year_start = NULL,
                          old_version = FALSE,
                          .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr", quietly = TRUE)
  
  valid_universes <- c("enrollment", "client", "both")
  
  if (universe_type %in% valid_universes)
  {
    make_both_universes <- function(h_ext, tf, cys, ov, fyear)
    {
      enrollment <- make_enrollment_universe(h_ext,
                                             .timeframe = tf,
                                             .calendar_year_start = cys,
                                             .old_version = ov,
                                             fy = fyear)
      
      # client <- dplyr::distinct(dplyr::arrange(enrollment, dplyr::desc(EntryDate)), PersonalID, .keep_all = TRUE)
      
      client <- enrollment |> 
        dplyr::arrange(dplyr::desc(EntryDate)) |> 
        dplyr::distinct(PersonalID, .keep_all = TRUE)
      
      return(list(enrollment = enrollment,
                  client = client))
    }
    
    output <- switch(universe_type,
                     "enrollment" = make_enrollment_universe(hmis_extract,
                                                             .timeframe = timeframe,
                                                             .calendar_year_start = calendar_year_start,
                                                             .old_version = old_version,
                                                             fy = .FY),
                     "client" = dplyr::distinct(dplyr::arrange(make_enrollment_universe(hmis_extract,
                                                                                        .timeframe = timeframe,
                                                                                        .calendar_year_start = calendar_year_start,
                                                                                        .old_version = old_version,
                                                                                        fy = .FY), dplyr::desc(EntryDate)), PersonalID, .keep_all = TRUE),
                     "both" = make_both_universes(hmis_extract,
                                                  tf = timeframe,
                                                  cys = calendar_year_start,
                                                  ov = old_version,
                                                  fyear = .FY)) # list(enrollment = make_enrollment_universe(hmis_extract,
    # .timeframe = timeframe,
    # .calendar_year_start = calendar_year_start,
    # .old_version = old_version,
    # fy = .FY),
    # client = dplyr::distinct(dplyr::arrange(make_enrollment_universe(hmis_extract,
    #                                                                  .timeframe = timeframe,
    #                                                                  .calendar_year_start = calendar_year_start,
    #                                                                  .old_version = old_version,
    #                                                                  fy = .FY), dplyr::desc(EntryDate)), PersonalID, .keep_all = TRUE)
    # client = dplyr::distinct(dplyr::arrange(enrollment, dplyr::desc(EntryDate)), PersonalID, .keep_all = TRUE)))
    
    return(output)
  }
  else
  {
    cli::cli_abort(c("!" = "{.strong Invalid input for the {.arg universe_type} argument.}",
                     "i" = "{.emph This argument must be: {.or {.val {valid_universes}}}.}",
                     "x" = "You entered: {.val {universe_type}}"))
  }
}

make_enrollment_universe <- function(.hmis_extract = NULL, ...,
                                     .timeframe,
                                     .calendar_year_start,
                                     .old_version,
                                     fy)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("tibble",    quietly = TRUE)
  
  if (is.null(.hmis_extract)) # NEW! ----
  {
    ifelse("hmis" %in% ls(envir = .GlobalEnv),
           .hmis_extract <- get("hmis", envir = .GlobalEnv),
           stop("\n\nERROR: .hmis_extract is missing.\n\nIf the HMIS extract is loaded in the environment as \"hmis\" this issue will autoresolve.\n\nIf the extract is called something else you will have to manually assign the extract to the \".hmis_extract\" argument, or pipe the extract into this function.\n\n"))
  }
  
  if (is.null(.calendar_year_start))
  {
    ifelse("calendarYearStart" %in% ls(envir = .GlobalEnv),
           .calendar_year_start <- get("calendarYearStart", envir = .GlobalEnv),
           .calendar_year_start <- lubridate::floor_date(lubridate::int_start(.timeframe), "year"))
    
    if (!"calendarYearStart" %in% ls(envir = .GlobalEnv))
    {
      message("\n\nNOTICE: \".calendar_year_start\" argument was not supplied and the \"calendarYearStart\" variable it would default to is not present in your environment. The \".calendar_year_start\" argument has bee set to January 1 of the year your .timeframe starts. If you wish to use a different \".calendar_year_start\" please run this function again and supply a specific variable: i.e., \".calendar_year_start = x\".")
    }
  }
  
  if (!lubridate::is.interval(.timeframe))
  {
    if (.timeframe == "ytd")
    {
      .timeframe <- lubridate::interval(lubridate::floor_date(Sys.Date(), unit = "year"),
                                        lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1))
    }
    else
    {
      ytd <- "ytd"
      date1 <- lubridate::floor_date(Sys.Date(), unit = "year")
      date2 <- (lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1))
      
      cli::cli_abort(c("!" = "The  {.arg reporting period} argument must be a {.pkg lubridate} interval. See: {.fun lubridate::interval}. (It can use the `%--%` notation.)",
                       "x" = "You entered {.val {(.timeframe)}}, the class of which is {.cls {class(.timeframe)}} and the type of which is {.val {typeof(.timeframe)}}.",
                       "i" = "{.emph Alternatively, you can enter {.val {ytd}} for {.arg reporting period}, which will run the function from {.val {date1}} through {.val {date2}}.}"))
    }
  }
  
  client_csv   = .hmis_extract$client
  entry_csv    = .hmis_extract$entry
  exit_csv     = .hmis_extract$exit
  services_csv = .hmis_extract$services
  project_csv  = .hmis_extract$project
  # extractDate  = .hmis_extract$extractDate
  
  if ("exportInterval" %in% names(.hmis_extract))
  {
    extractDate <- as.Date(lubridate::int_end(.hmis_extract$exportInterval))
  }
  else
  {
    extractDate <- .hmis_extract$extractDate
  }
  
  # projecttype_labels <- tibble(ProjectType = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14),
  #                              ProjectTypeLabel = c("Emergency Shelter",
  #                                                   "Transitional Housing",
  #                                                   "PH - Permanent Supportive Housing",
  #                                                   "Street Outreach",
  #                                                   "Services Only",
  #                                                   "Other",
  #                                                   "Safe Haven",
  #                                                   "PH - Housing Only",
  #                                                   "PH - Housing with Services (no disability required for entry)",
  #                                                   "Day Shelter",
  #                                                   "Homelessness Prevention",
  #                                                   "PH - Rapid Re-Housing",
  #                                                   "Coordinated Entry"))
  
  conditional_left_join <- function(.piped_data)
  {
    if (.old_version)
    {
      newData <- dplyr::left_join(.piped_data, project_csv, by = c("ProjectID"))
    }
    else
    {
      newData <- dplyr::left_join(.piped_data, dplyr::distinct(project_csv, ProjectID, .keep_all = TRUE), by = c("ProjectID"))
    }
    
    return(newData)
  }
  
  # Create df of enrollments that overlap with the reporting period, given some likely exit dates based on project types
  
  data <- entry_csv %>% 
    dplyr::left_join(exit_csv, by = c("EnrollmentID", "PersonalID")) %>%
    dplyr::left_join(client_csv, by = c("PersonalID")) %>%
    dplyr::left_join(services_csv, by = c("EnrollmentID", "PersonalID")) %>%
    conditional_left_join() %>%
    dplyr::mutate(LatestDate = pmax(EntryDate, LatestServiceDate, MoveInDate, ExitDate, na.rm = TRUE),
                  # ProjectTypeLabel = these_project("types", ProjectType, full_label = TRUE, .FY = fy),
                  # ExitDateUpdated = dplyr::if_else(is.na(ExitDate),
                  #                                  dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ lubridate::ymd(extractDate),
                  #                                                   ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate < (lubridate::ymd(extractDate) - lubridate::days(395)) ~ LatestDate,
                  #                                                   ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate >= (lubridate::ymd(extractDate) - lubridate::days(395)) ~ lubridate::ymd(extractDate),
                  #                                                   ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate < (lubridate::ymd(extractDate) - lubridate::days(180)) ~ LatestDate,
                  #                                                   ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate >= (lubridate::ymd(extractDate) - lubridate::days(180)) ~ lubridate::ymd(extractDate)),
                  #                                  ExitDate),
                  ExitDateUpdated = dplyr::if_else(is.na(ExitDate),
                                                   dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ extractDate,
                                                                    ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate < (extractDate - lubridate::days(395)) ~ LatestDate,
                                                                    ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate >= (extractDate - lubridate::days(395)) ~ extractDate,
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate < (extractDate - lubridate::days(180)) ~ LatestDate,
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate >= (extractDate - lubridate::days(180)) ~ extractDate),
                                                   ExitDate),
                  EnrollmentInterval = lubridate::interval(EntryDate, ExitDateUpdated),
                  AgeAtEntry_interval = DOB %--% EntryDate,
                  AgeAtYearStart_interval = DOB %--% as.Date(.calendar_year_start),
                  Age = AgeAtEntry_interval %/% lubridate::years(1),
                  Age_CalendarYear = AgeAtYearStart_interval %/% lubridate::years(1)) %>%
    dplyr::filter(lubridate::int_overlaps(.timeframe, EnrollmentInterval)) %>% 
    dplyr::arrange(dplyr::desc(ExitDateUpdated)) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(Youth = dplyr::if_else(any(HOH == 1 & Age < 25), 1, 0),
                  HHType = dplyr::if_else(n() > 1 & any(Age < 18), "Family with Children", "Other"),
                  HOH = dplyr::case_match(HOH,
                                          1 ~ "Self",
                                          2 ~ "Child",
                                          3 ~ "Spouse",
                                          4 ~ "Other",
                                          5 ~ "Other (Not Related)",
                                          99 ~ "Unknown")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(#AgeGroup = factor(dplyr::case_when(Age < 18 ~ "Under 18",
      #                  Age %in% 18:24 ~ "18 to 24",
      #                  Age %in% 25:34 ~ "25 to 34",
      #                  Age %in% 35:44 ~ "35 to 44",
      #                  Age %in% 45:54 ~ "45 to 54",
      #                  Age %in% 55:64 ~ "55 to 64",
      #                  Age > 64 ~ "65 and Over",
      #                  is.na(Age) ~ "Unknown"),
      # levels = c("Under 18",
      #            "18 to 24",
      #            "25 to 34",
      #            "35 to 44",
      #            "45 to 54",
      #            "55 to 64",
      #            "65 and Over",
      #            "Unknown")),
      # HHType = dplyr::case_when(is.na(HHType) ~ "Unknown",
      #                           .default = HHType)
      HHType = dplyr::if_else(is.na(HHType), "Unknown", HHType))
  
  return(tibble::as_tibble(data))
}
