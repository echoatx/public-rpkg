#' HRS Returns Demographics
#'
#' This function finds the demographics (Race/Ethnicity) and Project Types for
#' people who returned to homelessness within 2 years (default - edit
#' `max_days_to_reappear` to override this).
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the HMIS CSV Extract.
#' @param effective_date the effective date for the returns report (usually the
#'   dashboard date). (The interval will be set for `effective_date - 730` days
#'   through `effective_date - 1` day.)
#' @param max_days_to_reappear The return window. Defaults to `730` days (two
#'   years).
#' @param return_function_info If set to `TRUE`, the function will return a
#'   status message about the internal variable (dates) it is using (instead of
#'   the data frame it normally returns). Defaults to `FALSE.`
#' @param return_orig_return_window If set to `TRUE`, the function will return
#'   the tibble containing the original exits being used to calculate returns,
#'   instead of the resulting returns table. Defaults to `FALSE.`
#'
#' @return a tibble: `returns_report`.
#' @export
hrs_returns_demographics <- function(hmis_extract = NULL, ..., effective_date, max_days_to_reappear = 730, return_function_info = FALSE, return_orig_return_window = FALSE)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  effective_date <- as.Date(effective_date)
  
  report_start_date <- effective_date - lubridate::years(1)
  
  report_end_date <- (report_start_date + lubridate::years(1)) - lubridate::days(1)
  
  included_project_types <- c(4, 0, 1, 2, 8, 3, 9, 10, 13)
  
  hmis_data <- hmis_extract$entry |> 
    dplyr::left_join(hmis_extract$exit, by = c("EnrollmentID", "PersonalID")) |> 
    dplyr::left_join(hmis_extract$client, by = "PersonalID") |> 
    dplyr::left_join(hmis_extract$project, by = "ProjectID") 
  
  if (return_orig_return_window)
  {
    orig_return_window <- hmis_data |> dplyr::filter(ProjectType %in% included_project_types,
                                                     !is.na(ExitDate),
                                                     lubridate::`%within%`(as.Date(ExitDate), lubridate::interval(report_start_date - lubridate::days(730), report_end_date - lubridate::days(730))),
                                                     Destination %in% dplyr::filter(HUD_LivingSituations_Destinations_SubsidyTypes_FY24, Classification == "Permanent Housing Situations")$Value) |> 
      dplyr::arrange(PersonalID, -dplyr::desc(ExitDate), -dplyr::desc(EnrollmentID)) |> 
      dplyr::distinct(PersonalID, .keep_all = TRUE) |>
      dplyr::mutate(Returns_Universe_Project_Category = dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ "Permanent Housing",
                                                                         ProjectType %in% c(0, 1) ~ "Emergency Shelter",
                                                                         ProjectType == 2 ~ "Transitional Housing",
                                                                         ProjectType == 4 ~ "Street Outreach",
                                                                         ProjectType == 8 ~ "Safe Haven")) |> 
      dplyr::mutate(Returns_Universe_Total_Exits_by_ProjectType = dplyr::n(), .by = Returns_Universe_Project_Category) |> 
      dplyr::mutate(Returns_Universe_Total_Exits_by_RaceEthnicity = dplyr::n(), .by = Race_Ethnicity) |> 
      dplyr::mutate(Returns_Universe_Total_Exits_by_AgeGroup = dplyr::n(), .by = Age_Group) |> 
      dplyr::mutate(Returns_Universe_Total_Exits_by_Gender = dplyr::n(), .by = Gender)
    
    return(orig_return_window)
  }
  
  returns_universe <- hmis_data |>
    dplyr::filter(ProjectType %in% included_project_types,
                  !is.na(ExitDate),
                  # as.Date(ExitDate) %within% lubridate::interval(report_start_date - lubridate::days(730), report_end_date - lubridate::days(730)),
                  lubridate::`%within%`(as.Date(ExitDate), lubridate::interval(report_start_date - lubridate::days(730), report_end_date - lubridate::days(730))),
                  Destination %in% dplyr::filter(HUD_LivingSituations_Destinations_SubsidyTypes_FY24, Classification == "Permanent Housing Situations")$Value) |> 
    dplyr::arrange(PersonalID, -dplyr::desc(ExitDate), -dplyr::desc(EnrollmentID)) |> 
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::mutate(Returns_Universe_Project_Category = dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ "Permanent Housing",
                                                                       ProjectType %in% c(0, 1) ~ "Emergency Shelter",
                                                                       ProjectType == 2 ~ "Transitional Housing",
                                                                       ProjectType == 4 ~ "Street Outreach",
                                                                       ProjectType == 8 ~ "Safe Haven")) |> 
    dplyr::mutate(Returns_Universe_Total_Exits_by_ProjectType = dplyr::n(), .by = Returns_Universe_Project_Category) |> 
    dplyr::mutate(Returns_Universe_Total_Exits_by_RaceEthnicity = dplyr::n(), .by = Race_Ethnicity) |> 
    dplyr::mutate(Returns_Universe_Total_Exits_by_AgeGroup = dplyr::n(), .by = Age_Group) |> 
    dplyr::mutate(Returns_Universe_Total_Exits_by_Gender = dplyr::n(), .by = Gender) |> 
    dplyr::select(PersonalID,
                  Returns_Universe_ProjectID = ProjectID, # NEW!!! ----
                  Returns_Universe_ProjectType_CSV_Code = ProjectType, # NEW!!! ----
                  Returns_Universe_Total_Exits_by_ProjectType,
                  Returns_Universe_Total_Exits_by_RaceEthnicity,
                  Returns_Universe_Total_Exits_by_AgeGroup,
                  Returns_Universe_Project_Type = ProjectType,
                  Returns_Universe_Total_Exits_by_Gender,
                  Returns_Universe_Project_Category,
                  Returns_Universe_Exit_Date = ExitDate)
  
  returns_report <- hmis_data |> 
    dplyr::left_join(returns_universe, by = "PersonalID") |> 
    dplyr::filter(ProjectType %in% included_project_types,
                  PersonalID %in% returns_universe$PersonalID,
                  # EntryDate %within% lubridate::interval(Returns_Universe_Exit_Date, report_end_date)) |> 
                  `%within%`(EntryDate, lubridate::interval(Returns_Universe_Exit_Date, report_end_date))) |> 
    dplyr::select(HouseholdID,
                  EnrollmentID,
                  PersonalID,
                  Age_Group,
                  Race_Ethnicity,
                  Project = ProjectName,
                  ProjectID,
                  NewProjectType = ProjectType,
                  HOH,
                  EntryDate,
                  ExitDate,
                  Returns_Universe_ProjectID, # NEW!!! ----
                  Returns_Universe_ProjectType_CSV_Code, # NEW!!! ----
                  Returns_Universe_Project_Type,
                  Returns_Universe_Project_Category,
                  Returns_Universe_Exit_Date,
                  Returns_Universe_Total_Exits_by_ProjectType,
                  Returns_Universe_Total_Exits_by_RaceEthnicity,
                  Returns_Universe_Total_Exits_by_Gender,
                  Returns_Universe_Total_Exits_by_AgeGroup) |>
    dplyr::mutate(Excluded_Range = lubridate::interval(EntryDate + lubridate::days(1), pmin(ExitDate + lubridate::days(14), report_end_date)),
                  Excluded_Range = dplyr::if_else(NewProjectType %in% c(3, 9, 10, 13),
                                                  dplyr::if_else(is.na(Excluded_Range),
                                                                 lubridate::interval(EntryDate + lubridate::days(1), report_end_date),
                                                                 Excluded_Range),
                                                  NA)) |>
    dplyr::mutate(AllExcludedRangesPerSPID = list(Excluded_Range), .by = PersonalID) |>
    dplyr::rowwise() |>
    dplyr::mutate(in_Excluded_Range = dplyr::if_else(NewProjectType %in% c(3, 9, 10, 13),
                                                     any(EntryDate %within% AllExcludedRangesPerSPID, na.rm = TRUE),
                                                     FALSE)) |> 
    dplyr::ungroup() |>
    dplyr::mutate(is_Return = dplyr::case_when(NewProjectType %in% c(4, 0, 1, 2, 8)
                                               # & EntryDate %within% lubridate::interval(report_start_date, report_end_date)
                                               ~ TRUE,
                                               
                                               NewProjectType %in% c(3, 9, 10, 13)
                                               # & EntryDate %within% lubridate::interval(report_start_date, report_end_date)
                                               & (EntryDate - Returns_Universe_Exit_Date > 14)
                                               & !in_Excluded_Range
                                               ~ TRUE,
                                               
                                               .default = FALSE)) |> 
    dplyr::arrange(PersonalID, dplyr::desc(as.integer(is_Return)), -dplyr::desc(as.Date(EntryDate))) |> 
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::mutate(Days_to_Return = dplyr::if_else(is_Return,
                                                  EntryDate - Returns_Universe_Exit_Date,
                                                  NA),
                  Days_to_Return = dplyr::if_else(Days_to_Return > max_days_to_reappear, NA, Days_to_Return),
                  # is_Return = dplyr::if_else(is.na(Days_to_Return), NA, is_Return), #### NEW: 04/24/2024 ----
                  is_Return = dplyr::case_when(is_Return & !is.na(Days_to_Return) ~ is_Return,
                                               is_Return & is.na(Days_to_Return) ~ FALSE,
                                               !is_Return ~ is_Return,
                                               .default = NA), #### NEW: 04/24/2024 ----
                  ProjectType = factor(Returns_Universe_Project_Category,
                                       levels = c("Permanent Housing",
                                                  "Emergency Shelter",
                                                  "Transitional Housing",
                                                  "Street Outreach",
                                                  "Safe Haven")))
  
  if (return_function_info)
  {
    original_exit_window <- lubridate::interval(report_start_date - lubridate::days(730), report_end_date - lubridate::days(730))
    
    show_function_status <- function()
    {
      cli::cli_div()
      cli::cli_rule()
      cli::cli_text("\n")
      cli::cli_inform(c("i" = "The variable {.strong {.var report_start_date}} is set to {.strong {.val {format(report_start_date, '%m/%d/%Y')}}}.",
                        " " = "\n",
                        "i" = "The variable {.strong {.var report_end_date}} is set to {.strong {.val {format(report_end_date, '%m/%d/%Y')}}}.",
                        " " = "\n",
                        "i" = "The variable {.strong {.var original_exit_window}} is set to {.strong {.val {format(lubridate::int_start(original_exit_window), '%m/%d/%Y')}} – {.val {format(lubridate::int_end(original_exit_window), '%m/%d/%Y')}}}."))
      cli::cli_text("\n")
      cli::cli_rule()
      cli::cli_end()
    }
    
    show_function_status()
  }
  
  return(returns_report)
}

#' HRS Clients Housed
#'
#' This function finds clients housed by project type and for the previous 12
#' months for the specified time period.
#'
#' @param .data the data frame to which to apply this function.
#' @param reporting_period_start the start of the reporting period.
#' @param reporting_period_end the end of the reporting period.
#'
#' @return two tibbles: `clients_housed_grouped` and `annual_housings`.
#' @export
hrs_clients_housed <- function(.data, reporting_period_start, reporting_period_end)
{
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  previousYear <- lubridate::interval(as.Date(reporting_period_start), as.Date(reporting_period_end))
  
  reportingPeriod <- lubridate::interval(as.Date("2017-01-01"), as.Date(reporting_period_end))
  
  mhaProjectTypes <- c(0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 12, 14)
  
  phDestinations <- HUD_LivingSituations_Destinations_SubsidyTypes_FY24 |>
    dplyr::filter(Classification == "Permanent Housing Situations") |>
    dplyr::pull(Value)
  
  homelessEntrySituations <- HUD_LivingSituations_Destinations_SubsidyTypes_FY24 |>
    dplyr::filter(Classification == "Homeless Situations") |>
    dplyr::pull(Value)
  
  clientshoused <- .data$entry |>
    dplyr::left_join(.data$exit, by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(dplyr::distinct(.data$project, ProjectID, .keep_all = TRUE), by = c("ProjectID"))
  
  phMoveins <- clientshoused |>
    dplyr::filter(CoCCode == "TX-503",
                  ProjectType %in% these_project("types", c("psh", "rrh")),
                  lubridate::`%within%`(MoveInDate, reportingPeriod)) |>
    dplyr::mutate("ProjectTypeLabel" = these_project("types", ProjectType, full_label = TRUE),
                  SPID.Year = paste0(PersonalID, lubridate::year(MoveInDate)))
  
  phMoveins_groupedByYear <- phMoveins |>
    dplyr::mutate(Year = lubridate::year(MoveInDate)) |>
    dplyr::arrange(dplyr::desc(PersonalID), dplyr::desc(MoveInDate)) |>
    dplyr::distinct(PersonalID, Year, .keep_all = TRUE) |>
    dplyr::group_by(Year, ProjectTypeLabel) |>
    dplyr::summarise(Count = dplyr::n())
  
  mhaExits <- clientshoused |>
    dplyr::mutate(SPID.Year = paste0(PersonalID, lubridate::year(ExitDate)),
                  ProjectTypeLabel = "Minimal Housing Assistance") |> 
    dplyr::filter(CoCCode == "TX-503",
                  ProjectType %in% mhaProjectTypes,
                  lubridate::`%within%`(ExitDate, reportingPeriod),
                  Destination %in% phDestinations & LivingSituationEntry %in% homelessEntrySituations,
                  !SPID.Year %in% phMoveins$SPID.Year)
  
  mhaExits_groupedByYear <- mhaExits |>
    dplyr::mutate(Year = lubridate::year(ExitDate)) |>
    dplyr::arrange(dplyr::desc(PersonalID), dplyr::desc(ExitDate)) |>
    dplyr::distinct(PersonalID, Year, .keep_all = TRUE) |>
    dplyr::group_by(Year, ProjectTypeLabel) |>
    dplyr::summarise(Count = dplyr::n())
  
  clientsHousedByYear <- dplyr::bind_rows(mhaExits_groupedByYear, phMoveins_groupedByYear) |>
    dplyr::arrange(-dplyr::desc(Year))
  
  housingsCombinedByYear <- dplyr::bind_rows(dplyr::filter(phMoveins, lubridate::`%within%`(MoveInDate, previousYear)),
                                             dplyr::filter(mhaExits, lubridate::`%within%`(ExitDate, previousYear))) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE)
  
  return(list(clients_housed_grouped = clientsHousedByYear,
              annual_housings = housingsCombinedByYear))
}

#' HRS Unsheltered Snapshot
#'
#' This function returns a tibble with all of the unsheltered clients from the
#' CA Reference list who have not touched the system in the past 180 days, per
#' the information available in the CA Reference List.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the HMIS CSV Extract.
#' @param dashboard_date The date for which the dashboard is being generated.
#' @param ce_bnl the file path to the CE BNL Excel file.
#' @param bnl_date The date of the CE BNL (do not supply an actual date value,
#'   just a `string`).
#' @param mean_days The lookback timeframe. Defaults to `180` days (six months).
#'
#' @return A tibble with all of the unsheltered clients from the CA Reference
#'   list that have not touched the system in the past `mean_days`.
#' @export
hrs_unsheltered_snapshot <- function(hmis_extract = NULL, ..., dashboard_date, ce_bnl, bnl_date, mean_days = 180)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("readr",     quietly = TRUE)
  requireNamespace("readxl",    quietly = TRUE)
  # requireNamespace("stringr",   quietly = TRUE)
  requireNamespace("tidyselect", quietly = TRUE)
  
  if(!file.exists(shortcut("db inputs", paste0(dashboard_date, "_update"), "CA All Assessments 21.11.08 (Dedup before using).xls")))
  {
    new_ca <- readxl::read_excel(shortcut("db inputs", paste0(dashboard_date, "_update"), "CA All Assessments 21.11.08 (Dedup before using).xlsx"), sheet = 1) |>
      dplyr::mutate(uid = as.numeric(`Client Uid`),
                    IDDate = as.Date(`ID Date`),
                    FirstIDDate = as.Date(`First ID Date`)) |>
      dplyr::filter(!is.na(uid), is.numeric(uid)) 
  }
  else
  {
    new_ca <- readxl::read_excel(shortcut("db inputs", paste0(dashboard_date, "_update"), "CA All Assessments 21.11.08 (Dedup before using).xls"), sheet = 1) |>
      dplyr::mutate(uid = as.numeric(`Client Uid`),
                    IDDate = as.Date(`ID Date`),
                    FirstIDDate = as.Date(`First ID Date`)) |>
      dplyr::filter(!is.na(uid), is.numeric(uid))
  }
  
  # calculate time between homelessness start date and assessment date
  # DH column is the number of days between ID Date and Effective Date of report and DH is
  # the number of days between "Approximate date homelessness started" and Effective date.
  new_ca <- new_ca |> 
    dplyr::mutate(DaysHomelessToLatestAssess = DH - DA,
                  DaysFirstAssessLatestAssess = as.double(difftime(IDDate, FirstIDDate, unit = "day")),
                  DaysHomelessToFirstAssess = DaysHomelessToLatestAssess - DaysFirstAssessLatestAssess)
  
  ca_dedup <- new_ca |> 
    dplyr::distinct(uid, IDDate, FirstIDDate, .keep_all = TRUE)
  
  # Different definitions of "New assessment"
  
  # Def 1: First assessment & assessment is in the most recent reporting period (last month)
  new_assessment_period <- lubridate::interval(lubridate::ymd(bnl_date) - months(1), lubridate::ymd(bnl_date))
  
  # Keeps only each client's first assessment if it is in the most recent reporting period
  
  clients_newly_assessed_def1 <- ca_dedup |> 
    dplyr::filter(FirstIDDate %within% new_assessment_period)
  
  # Def 2: Only has 1 assessment
  clients_newly_assessed_def2 <- new_ca |>
    dplyr::group_by(uid) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::filter(n == 1)
  
  select_bnl_columns <- function(.data)
  {
    .data <- .data |> 
      dplyr::select(DaysSinceAssessment = `DA`,
                    Shelter = `Shelter`,
                    PersonalID = `Client Uid`,
                    PrimaryRace = ifelse(as.Date(bnl_date) >= as.Date("2023-11-01"),
                                         "Race (Retired)(895)",
                                         "Primary Race(895)"),
                    SecondaryRace = ifelse(as.Date(bnl_date) >= as.Date("2023-11-01"),
                                           "Secondary Race (Retired)(1443)",
                                           "Secondary Race - If Applicable(1443)"),
                    Ethnicity = ifelse(as.Date(bnl_date) >= as.Date("2023-11-01"),
                                       "Ethnicity (Retired)(896)",
                                       "Ethnicity(896)"),
                    Gender = `Gender(894)`,
                    Age = `Age (Calculated)`,
                    HOH,
                    Veteran = `Vets`,
                    DV = `VSP Y/N`,
                    Chronic = `CH`,
                    ProjectType,
                    HHSize = HH,
                    MinorChildren = `Children`)
    
    return(.data)
  }
  
  # PSH prioritization
  psh_priority <- ce_bnl$pshPriority |> 
    dplyr::filter(pmin(`DA`,
                       lubridate::ymd(bnl_date) - as.Date(`ID Date`),
                       lubridate::ymd(bnl_date) - as.Date(`Date Contact Info Last Updated or Confirmed (please reconfirm and update whenever possible)(3674)`),
                       lubridate::ymd(bnl_date) - as.Date(`IC Walk-In Info`),
                       na.rm = TRUE) <= mean_days) |>
    dplyr::filter(`Client First Name` != "Test") |>
    dplyr::mutate(HOH = "Self",
                  ProjectType = "Unsheltered") |>
    select_bnl_columns()
  
  # psh_priority <- readxl::read_excel(ce_bnl, sheet = 1, skip = 1) |>
  # dplyr::filter(pmin(`DA`,
  #                    lubridate::ymd(bnl_date) - as.Date(`ID Date`),
  #                    lubridate::ymd(bnl_date) - as.Date(`Date Contact Info Last Updated or Confirmed (please reconfirm and update whenever possible)(3674)`),
  #                    lubridate::ymd(bnl_date) - as.Date(`IC Walk-In Info`),
  #                    na.rm = TRUE) <= mean_days) |>
  # dplyr::filter(`Client First Name` != "Test") |>
  # dplyr::mutate(HOH = "Self",
  #               ProjectType = "Unsheltered") |>
  #   select_bnl_columns()
  
  # RRH prioritization
  rrh_priority <- ce_bnl$rrhPriority |> 
    dplyr::filter(pmin(`DA`,
                       lubridate::ymd(bnl_date) - as.Date(`ID Date`),
                       lubridate::ymd(bnl_date) - as.Date(`Date Contact Info Last Updated or Confirmed (please reconfirm and update whenever possible)(3674)`),
                       lubridate::ymd(bnl_date) - as.Date(`IC Walk-In Info`),
                       na.rm = TRUE) <= mean_days) |>
    dplyr::filter(`Client First Name` != "Test") |>
    dplyr::mutate(HOH = "Self",
                  ProjectType = "Unsheltered") |>
    select_bnl_columns()
  
  # rrh_priority <- readxl::read_excel(ce_bnl, sheet = 2, skip = 1) |>
  #   dplyr::filter(pmin(`DA`,
  #                      lubridate::ymd(bnl_date) - as.Date(`ID Date`),
  #                      lubridate::ymd(bnl_date) - as.Date(`Date Contact Info Last Updated or Confirmed (please reconfirm and update whenever possible)(3674)`),
  #                      lubridate::ymd(bnl_date) - as.Date(`IC Walk-In Info`),
  #                      na.rm = TRUE) <= mean_days) |>
  #   dplyr::filter(`Client First Name` != "Test") |>
  #   dplyr::mutate(HOH = "Self",
  #                 ProjectType = "Unsheltered") |>
  #   select_bnl_columns()
  
  # full prioritization
  priority_list <- psh_priority |>
    dplyr::bind_rows(rrh_priority) |>
    dplyr::arrange(DaysSinceAssessment) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::mutate(Youth = dplyr::if_else(Age < 25, 1, 0)) |>
    dplyr::mutate(List = "Prioritization")
  
  # PSH referred
  psh_referred <- ce_bnl$pshStaffing |> 
    dplyr::filter(`Client First Name` != "Test") |>
    dplyr::mutate(HOH = "Self",
                  ProjectType = "Unsheltered",
                  DV = "Referred") |>
    select_bnl_columns()
  
  # psh_referred <- readxl::read_excel(ce_bnl, sheet = 3, skip = 1) |>
  #   dplyr::filter(`Client First Name` != "Test") |>
  #   dplyr::mutate(HOH = "Self",
  #                 ProjectType = "Unsheltered",
  #                 DV = "Referred") |>
  #   select_bnl_columns()
  
  # RRH referred
  rrh_referred <- ce_bnl$rrhStaffing |> 
    dplyr::filter(`Client First Name` != "Test") |>
    dplyr::mutate(HOH = "Self",
                  ProjectType = "Unsheltered",
                  DV = "Referred") |>
    select_bnl_columns()
  
  # rrh_referred <- readxl::read_excel(ce_bnl, sheet = 4, skip = 1) |>
  #   dplyr::filter(`Client First Name` != "Test") |>
  #   dplyr::mutate(HOH = "Self",
  #                 ProjectType = "Unsheltered",
  #                 DV = "Referred") |>
  #   select_bnl_columns()
  
  # full referred
  referred <- psh_referred |>
    dplyr::bind_rows(rrh_referred) |>
    dplyr::arrange(DaysSinceAssessment) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::mutate(Youth = dplyr::if_else(Age < 25, 1, 0)) |>
    dplyr::mutate(List = "Referred")
  
  # priority list + referred clients
  total_ca <- priority_list |> 
    dplyr::bind_rows(referred) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::select(-DaysSinceAssessment)
  
  # unsheltered
  unsheltered <- total_ca |>
    dplyr::filter(is.na(Shelter)) |>
    dplyr::select(-Shelter)
  
  ce_entries <- hmis_extract$entry |>
    dplyr::filter(ProjectID == 9339) |>
    dplyr::select(HouseholdID,
                  PersonalID,
                  EntryDate,
                  HOH,
                  DisablingCondition,
                  LengthOfStay,
                  LOSUnderThreshold,
                  PreviousStreetESSH,
                  DateToStreetESSH,
                  TimesHomelessPastThreeYears,
                  MonthsHomelessPastThreeYears) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE)
  
  client <- hmis_extract$client |>
    dplyr::select(PersonalID,
                  Race_Ethnicity,
                  Gender,
                  Transgender,
                  is_Veteran,
                  Veteran_Status,
                  Age,
                  Age_Group)
  
  unsheltered_householdIDs <- unsheltered |>
    dplyr::select(PersonalID, Chronic, DV, PrimaryRace, SecondaryRace) |>
    dplyr::left_join(ce_entries) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::select(PersonalID, HouseholdID, Chronic, DV, PrimaryRace, SecondaryRace)
  
  unsheltered_households <- unsheltered_householdIDs |>
    dplyr::left_join(ce_entries, by = c("HouseholdID")) |>
    dplyr::select(-PersonalID.x) |>
    dplyr::rename(PersonalID = PersonalID.y) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::left_join(client) |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(Youth = dplyr::if_else(any(HOH == 1 & Age < 25), 1, 0),
                  HHType = dplyr::if_else(dplyr::n() > 1 & any(Age < 18), "Family with Children", "Other"),
                  HOH = dplyr::case_match(HOH,
                                          1 ~ "Self",
                                          2 ~ "Child",
                                          3 ~ "Spouse",
                                          4 ~ "Other",
                                          5 ~ "Other (Not Related)",
                                          99 ~ "Unknown")) |>
    dplyr::ungroup() |>
    dplyr::mutate(Veteran = dplyr::if_else(Veteran_Status %in% c("Yes", "No"), Veteran_Status, "Unknown"),
                  HHType = factor(dplyr::case_when(is.na(HHType) ~ "Unknown", .default = HHType),
                                  levels = c("Family with Children", "Other", "Unknown")),
                  NewlyAssessed_Def1 = dplyr::if_else(PersonalID %in% clients_newly_assessed_def1$uid, 1, 0),
                  NewlyAssessed_Def2 = dplyr::if_else(PersonalID %in% clients_newly_assessed_def2$uid, 1, 0),
                  ProjectType = "Unsheltered") |>
    dplyr::select(HouseholdID,
                  PersonalID,
                  HHType,
                  Race_Ethnicity,
                  Gender,
                  Age,
                  Age_Group,
                  Chronic,
                  DV,
                  HOH,
                  Veteran,
                  ProjectType, 
                  Youth,
                  NewlyAssessed_Def1,
                  NewlyAssessed_Def2)
  
  return(unsheltered_households)
}

#' HRS Sheltered Snapshot
#'
#' This function returns a tibble with all of the sheltered clients, per the
#' HMIS extract, who have not touched the system in the past 180 days, per the
#' information available in the CA Reference List.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the HMIS CSV Extract.
#' @param report_effective_date The date of the snapshot.
#' @param extract_date The date of the HMIS CSV Extract. (Defaults to the
#'   extract date of `hmis_extract`, but it can be set manually)
#'
#' @return A tibble with an estimate of all of the sheltered clients as of the
#'   `report_effective_date`.
#' @export
hrs_sheltered_snapshot <- function(hmis_extract = NULL, ..., report_effective_date, extract_date = NULL)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("readr",     quietly = TRUE)
  requireNamespace("readxl",    quietly = TRUE)
  # requireNamespace("stringr",   quietly = TRUE)
  
  extract_date <- hmis_extract$extractDate
  
  shelteredPIT <- dplyr::bind_rows(check_pit(hmis_extract,
                                             report_effective_date,
                                             project_type = 0,
                                             dedup_only = TRUE),
                                   check_pit(hmis_extract,
                                             report_effective_date,
                                             project_type = 1,
                                             dedup_only = TRUE))
  
  sheltered <- hmis_extract$entry |>
    dplyr::left_join(hmis_extract$exit, by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(hmis_extract$client, by = c("PersonalID")) |>
    dplyr::left_join(hmis_extract$project, by = c("ProjectID")) |>
    dplyr::left_join(dplyr::rename(dplyr::select(hmis_extract$healthanddv, -CurrentlyFleeing), DV = DomesticViolenceSurvivor), by = c("PersonalID")) |>
    dplyr::filter(ProjectType %in% c(0:1),
                  PersonalID %in% shelteredPIT$PersonalID) |>
    dplyr::select(HouseholdID,
                  PersonalID,
                  Race_Ethnicity,
                  Gender,
                  Age,
                  Age_Group,
                  HOH, 
                  is_Veteran,
                  Veteran_Status,
                  DV,
                  Chronic,
                  ProjectType) |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(Youth = dplyr::if_else(any(HOH == 1 & Age < 25), 1, 0),
                  HHType = dplyr::if_else(dplyr::n() > 1 & any(Age < 18), "Family with Children", "Other")) |>
    dplyr::ungroup() |>
    dplyr::mutate(Veteran = dplyr::case_when(Veteran_Status == "Yes" ~ 1,
                                             Veteran_Status == "No" ~ 0,
                                             .default = NA_real_),
                  DV = dplyr::if_else(DV %in% 0:1, DV, NA_real_),
                  HHType = factor(dplyr::case_when(is.na(HHType) ~ "Unknown", .default = HHType),
                                  levels = c("Family with Children", "Other", "Unknown"))) |>
    dplyr::select(HouseholdID,
                  PersonalID,
                  Race_Ethnicity,
                  Gender,
                  Age,
                  Age_Group,
                  HOH,
                  HHType,
                  Veteran,
                  DV,
                  Chronic,
                  ProjectType, 
                  Youth) |>
    dplyr::group_by(PersonalID) |>
    dplyr::arrange(dplyr::desc(PersonalID)) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE) |>
    dplyr::ungroup()
  
  return(sheltered)
}

#' HRS Enrollments Table Generator
#'
#' This function returns a tibble with all of the unsheltered clients from the
#' CA Reference list that have not touched the system in the past 180 days, per
#' the information available in the CA Reference List.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the HMIS CSV Extract.
#' @param reporting_period_start The start date of the reporting period.
#' @param reporting_period_end The end date of the reporting period.
#' @param extract_date The date of the HMIS CSV Extract. (Defaults to the
#'   extract date of `hmis_extract`, but it can be set manually)
#'
#' @return A tibble with an estimate of all of the sheltered clients as of the
#'   `report_effective_date`.
#' @export
hrs_enrollments <- function(hmis_extract = NULL, ...,
                            reporting_period_start, 
                            reporting_period_end,
                            extractdate = NULL)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  if (is.null(extractdate))
  {
    extractdate <- hmis_extract$extractDate
  }
  
  reporting_period <- lubridate::interval(reporting_period_start, reporting_period_end)
  
  data0 <- hmis_extract$entry |>
    dplyr::left_join(hmis_extract$exit, by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(hmis_extract$client, by = c("PersonalID")) |>
    dplyr::left_join(hmis_extract$services, by = c("EnrollmentID", "PersonalID")) |>
    dplyr::left_join(hmis_extract$project, by = c("ProjectID"))
  
  data <- data0 |>
    dplyr::mutate(LatestDate = pmax(EntryDate, LatestServiceDate, MoveInDate, ExitDate, na.rm = TRUE),
                  ExitDateUpdated = dplyr::if_else(is.na(ExitDate),
                                                   dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ lubridate::ymd(extractdate),
                                                                    ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate < (lubridate::ymd(extractdate) - lubridate::days(395)) ~ LatestDate,
                                                                    ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate >= (lubridate::ymd(extractdate) - lubridate::days(395)) ~ lubridate::ymd(extractdate),
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate < (lubridate::ymd(extractdate) - lubridate::days(180)) ~ LatestDate,
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate >= (lubridate::ymd(extractdate) - lubridate::days(180)) ~ lubridate::ymd(extractdate)),
                                                   ExitDate),
                  EnrollmentInterval = lubridate::interval(EntryDate, ExitDateUpdated)) |>
    dplyr::filter(lubridate::int_overlaps(reporting_period, EnrollmentInterval)) |>
    dplyr::arrange(dplyr::desc(ExitDateUpdated)) |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(Youth = dplyr::if_else(any(HOH == 1 & Age < 25), 1, 0),
                  HHType = dplyr::if_else(dplyr::n() > 1 & any(Age < 18), "Family with Children", "Other"),
                  HOH = dplyr::case_match(HOH,
                                          1 ~ "Self",
                                          2 ~ "Child",
                                          3 ~ "Spouse",
                                          4 ~ "Other",
                                          5 ~ "Other (Not Related)",
                                          99 ~ "Unknown")) |>
    dplyr::ungroup() |>
    dplyr::mutate(HHType = factor(dplyr::case_when(is.na(HHType) ~ "Unknown",
                                                   .default = HHType),
                                  levels = c("Family with Children", "Other", "Unknown")))
  
  # Interval for 2 years prior to reporting period
  previous_period <- lubridate::interval(lubridate::int_start(reporting_period) - lubridate::years(2), lubridate::int_end(reporting_period) - lubridate::years(1))
  
  # Create df of enrollments in the 2 years prior to the reporting period
  previous_data <- data0 |>
    dplyr::filter(EntryDate < lubridate::int_end(previous_period)) |>
    dplyr::mutate(LatestDate = pmax(EntryDate, LatestServiceDate, MoveInDate, ExitDate, na.rm = TRUE),
                  ExitDateUpdated = dplyr::if_else(is.na(ExitDate),
                                                   dplyr::case_when(ProjectType %in% c(3, 9, 10, 13) ~ lubridate::ymd(extractdate),
                                                                    ProjectType %in% c(0, 1, 2, 8) & LatestDate < (lubridate::ymd(extractdate) - lubridate::days(395)) ~ LatestDate,
                                                                    ProjectType %in% c(0, 1, 2, 8, 11) & LatestDate >= (lubridate::ymd(extractdate) - lubridate::days(395)) ~ lubridate::ymd(extractdate),
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate < (lubridate::ymd(extractdate) - lubridate::days(180)) ~ LatestDate,
                                                                    ProjectType %in% c(4, 6, 7, 12, 14) & LatestDate >= (lubridate::ymd(extractdate) - lubridate::days(180)) ~ lubridate::ymd(extractdate)),
                                                   ExitDate)) |>
    dplyr::filter(ExitDateUpdated >= lubridate::int_start(previous_period)) |>
    dplyr::arrange(dplyr::desc(ExitDateUpdated)) |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(Youth = dplyr::if_else(any(HOH == 1 & Age < 25), 1, 0),
                  HHType = dplyr::if_else(dplyr::n() > 1 & any(Age < 18), "Family with Children", "Other"),
                  HOH = dplyr::case_match(HOH,
                                          1 ~ "Self",
                                          2 ~ "Child",
                                          3 ~ "Spouse",
                                          4 ~ "Other",
                                          5 ~ "Other (Not Related)",
                                          99 ~ "Unknown")) |>
    dplyr::ungroup() |>
    dplyr::mutate(HHType = factor(dplyr::case_when(is.na(HHType) ~ "Unknown",
                                                   .default = HHType),
                                  levels = c("Family with Children", "Other", "Unknown")))
  
  # List all new clients in the reporting period. This should fulfill "Newly enrolled in HRS" from the data dictionary
  newlyenrolled <- data |>
    dplyr::filter(!(PersonalID %in% previous_data$PersonalID))
  
  return(list(newlyenrolled = tibble::as_tibble(newlyenrolled),
              enrolled = tibble::as_tibble(data)))
}
