#' Read in an HMIS CSV/XML Export
#'
#' Reads in .csv files from data exported from HMIS per the specified date and
#' returns a list of tibbles.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param extract_path the path to the folder containing the HMIS CSV/XML Export
#'   .csv files.
#' @param extract_date the date of the HMIS CSV/XML Export.
#' @param include_disabilities OPTIONAL: specifies whether to import the
#'   disabilities file (defaults to `FALSE`).
#' @param .FY The HMIS Data Standards Fiscal Year (entered numerically with two
#'   digits, i.e., `22` for FY22). _**Defaults to FY24.**_ Can be set backwards
#'   up to FY22 for backwards compatibility with older HMIS Extracts.
#'
#' @return a list of 8-9 tibbles: client, services, entry, exit, project,
#'   healthanddv, incomebenefits, & organization (and disabilities if
#'   include_disabilities = TRUE).
#' @export
import_hmis <- function(extract_path,
                        # extract_date,
                        ...,
                        include_disabilities = FALSE,
                        # include_roi_report = FALSE,
                        all_files = FALSE,
                        suppressNormalReadrCsvWarnings = TRUE,
                        runningStandalone = TRUE,
                        .FY = 24)
{
  rlang::check_dots_empty()
  
  import_hmis_fy <- switch(as.character(.FY),
                           "22" = import_hmis_fy22(extract_path,
                                                   # extract_date,
                                                   .include_disabilities = include_disabilities,
                                                   .suppressNormalReadrCsvWarnings = suppressNormalReadrCsvWarnings),
                           "24" = import_hmis_fy24(extract_path,
                                                   # extract_date,
                                                   .include_disabilities = include_disabilities,
                                                   .all_files = all_files,
                                                   .suppressNormalReadrCsvWarnings = suppressNormalReadrCsvWarnings))
  
  if (runningStandalone)
  {
    # requireNamespace("lubridate", quietly = TRUE)
    
    # applyHmisClass <- function(x)
    # {
    #   if ("Interval" %in% class(x))
    #   {
    #     return(x)
    #   }
    #   else if ("Date" %in% class(x))
    #   {
    #     return(x)
    #   }
    #   else if ("numeric" %in% class(x) & !"Date" %in% class(x))
    #   {
    #     class(x) <- c("HMIS Data Standards Fiscal Year", class(x))
    #     
    #     return(x)
    #   }
    #   else
    #   {
    #     class(x) <- c("HMIS Data File", class(x))
    #     
    #     return(x)
    #   }
    # }
    
    import_hmis_fy <- lapply(import_hmis_fy, applyHmisClass)
    
    class(import_hmis_fy) <- c("HMIS Extract", class(import_hmis_fy))
  }
  
  return(import_hmis_fy)
}

import_hmis_fy22 <- function(.extract_path,
                             # .extract_date,
                             ...,
                             .include_disabilities,
                             .suppressNormalReadrCsvWarnings)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",      quietly = TRUE)
  requireNamespace("lubridate",  quietly = TRUE)
  requireNamespace("readr",      quietly = TRUE)
  requireNamespace("stringr",    quietly = TRUE)
  requireNamespace("tibble",     quietly = TRUE)
  requireNamespace("tidyr",      quietly = TRUE)
  requireNamespace("tidyselect", quietly = TRUE)
  
  if (.suppressNormalReadrCsvWarnings)
  {
    options(warn = - 1)
  }
  
  .extract_date <- as.Date(readr::read_csv(paste0(.extract_path, "/Export.csv"), show_col_types = FALSE)$ExportEndDate[1])
  
  client <- readr::read_csv(stringr::str_c(.extract_path, "Client.csv"), show_col_types = FALSE) %>%
    dplyr::mutate(Gender = dplyr::case_when(is.na(GenderNone) == FALSE ~ "Data not collected",
                                            NoSingleGender == 1 ~ "No Single Gender",
                                            Questioning == 1 ~ "Questioning",
                                            Female == 1 ~ "Female",
                                            Male == 1 ~ "Male",
                                            TRUE ~ "Other"),
                  Transgender = dplyr::case_when(is.na(GenderNone) == FALSE ~ "Data not collected",
                                                 Transgender == 1 ~ "Transgender",
                                                 TRUE ~ "Not transgender"),
                  Ethnicity = dplyr::case_when(Ethnicity == 0 ~ "Non-Hispanic/Non-Latino",
                                               Ethnicity == 1 ~ "Hispanic/Latino",
                                               Ethnicity %in% c(8, 9, 99) ~ "Unknown"),
                  Race = dplyr::case_when(AmIndAKNative == 1 & (Asian + BlackAfAmerican + NativeHIPacific + White) == 0 ~ "AmIndAKNative",
                                          Asian == 1 & (AmIndAKNative + BlackAfAmerican + NativeHIPacific + White) == 0 ~ "Asian",
                                          BlackAfAmerican == 1 & (AmIndAKNative + Asian + NativeHIPacific + White) == 0 ~ "BlackAfAmerican",
                                          NativeHIPacific == 1 & (AmIndAKNative + Asian + BlackAfAmerican + White) == 0 ~ "NativeHIOtherPacific",
                                          White == 1 & (AmIndAKNative + Asian + BlackAfAmerican + NativeHIPacific) == 0 ~ "White",
                                          (AmIndAKNative + Asian + BlackAfAmerican + NativeHIPacific + White) >= 2 ~ "Two or more races",
                                          is.na(RaceNone) == FALSE ~ "Data not collected"),
                  Race_Ethnicity = dplyr::case_when(Ethnicity == "Hispanic/Latino" ~ "Hispanic/Latino",
                                                    TRUE ~ Race),
                  Age = lubridate::interval(DOB, lubridate::ymd(.extract_date)) %/% lubridate::years(1)) %>%
    dplyr::select(PersonalID, Race, Ethnicity, Race_Ethnicity, Gender, Transgender, VeteranStatus, DOB, Age) #%>%
  # dplyr::mutate(AgeAtEntry_interval = DOB %--% EntryDate,
  #        Age = AgeAtEntry_interval %/% lubridate::years(1),
  #        AgeGroup = dplyr::case_when(Age %in% c(0:17) ~ "Under 18",
  #                             Age %in% c(18:24) ~ "18-24",
  #                             Age %in% c(25:34) ~ "25-34",
  #                             Age %in% c(35:44) ~ "35-44",
  #                             Age %in% c(45:54) ~ "45-54",
  #                             Age %in% c(55:64) ~ "55-64",
  #                             Age >= 65 ~ "65 +"),
  #        AgeGroup = factor(AgeGroup,
  #                          levels = c("Under 18",
  #                                     "18-24",
  #                                     "25-34",
  #                                     "35-44",
  #                                     "45-54",
  #                                     "55-64",
  #                                     "65 +")))
  entry <- readr::read_csv(stringr::str_c(.extract_path, "Enrollment.csv"), show_col_types = FALSE) %>%
    dplyr::select(EnrollmentID,
                  HouseholdID,
                  PersonalID,
                  ProjectID,
                  EntryDate,
                  MoveInDate,
                  LivingSituationEntry = LivingSituation,
                  HOH = RelationshipToHoH,
                  DisablingCondition,
                  LengthOfStay,
                  LOSUnderThreshold,
                  PreviousStreetESSH,
                  DateToStreetESSH,
                  TimesHomelessPastThreeYears,
                  MonthsHomelessPastThreeYears) %>%
    # dplyr::mutate(ConsolidatedProject = if_else(ProjectID %in% consolidatedProjects, TRUE, FALSE)) %>% 
    dplyr::mutate(Chronic = dplyr::case_when(DisablingCondition == 1 
                                             & LivingSituationEntry %in% c(1, 16, 18) 
                                             & LengthOfStay == 5 
                                             | EntryDate - DateToStreetESSH >= 365 
                                             ~ 1,
                                             
                                             DisablingCondition == 1 
                                             & LivingSituationEntry %in% c(1, 16, 18) 
                                             & TimesHomelessPastThreeYears == 4 
                                             & MonthsHomelessPastThreeYears %in% c(112, 113) 
                                             ~ 1,
                                             
                                             DisablingCondition == 1 
                                             & LivingSituationEntry %in% c(15, 6, 7, 25, 4, 5) 
                                             & LOSUnderThreshold == 1 
                                             & PreviousStreetESSH == 1 
                                             & EntryDate - DateToStreetESSH >= 365 
                                             ~ 1,
                                             
                                             DisablingCondition == 1 
                                             & LivingSituationEntry %in% c(15, 6, 7, 25, 4, 5) 
                                             & LOSUnderThreshold == 1 
                                             & PreviousStreetESSH == 1 
                                             & TimesHomelessPastThreeYears == 4 
                                             & MonthsHomelessPastThreeYears %in% c(112, 113) 
                                             ~ 1,
                                             
                                             TRUE ~ 0))
  
  chronic <- entry %>%
    dplyr::mutate(MaybeChronic = maybeChronic_list(entry, .extract_date)) %>%
    dplyr::select(c("EnrollmentID", "PersonalID", "MaybeChronic"))
  
  entry <- entry %>%
    dplyr::left_join(chronic, by = c("EnrollmentID", "PersonalID"))
  
  exit <- readr::read_csv(stringr::str_c(.extract_path, "Exit.csv"), show_col_types = FALSE) %>%
    dplyr::select(EnrollmentID, PersonalID, ExitDate, Destination)
  
  # Creates list of most recent service per enrollment
  services <- readr::read_csv(stringr::str_c(.extract_path, "Services.csv"), show_col_types = FALSE) %>%
    dplyr::select(EnrollmentID, PersonalID, LatestServiceDate = DateProvided) %>%
    dplyr::arrange(dplyr::desc(LatestServiceDate)) %>%
    dplyr::distinct(EnrollmentID, .keep_all = TRUE)
  
  project <- readr::read_csv(stringr::str_c(.extract_path, "Project.csv"), show_col_types = FALSE) %>%
    dplyr::select(OrganizationID, ProjectID, ProjectName, ProjectType) %>%
    dplyr::mutate(ConsolidatedProject = if_else(ProjectID %in% consolidatedProjects, TRUE, FALSE)) %>%
    dplyr::mutate(ActiveProject = if_else(!str_detect(ProjectName, "ZZZ"), TRUE, FALSE))
  
  projectcoc <- readr::read_csv(stringr::str_c(.extract_path, "ProjectCoC.csv"), show_col_types = FALSE) %>%
    dplyr::select(ProjectID, CoCCode)
  
  healthanddv <- readr::read_csv(stringr::str_c(.extract_path, "HealthAndDV.csv"), show_col_types = FALSE) %>%
    dplyr::arrange(dplyr::desc(InformationDate)) %>%
    dplyr::distinct(PersonalID, .keep_all = TRUE) %>%
    dplyr::select(PersonalID, DomesticViolenceVictim, CurrentlyFleeing)
  
  incomebenefits <- readr::read_csv(stringr::str_c(.extract_path, "IncomeBenefits.csv"), show_col_types = FALSE) %>%
    dplyr::group_by(EnrollmentID, PersonalID) %>%
    dplyr::filter(DataCollectionStage == max(DataCollectionStage)) %>%
    dplyr::ungroup() %>%
    dplyr::select(EnrollmentID, PersonalID, IncomeFromAnySource, TotalMonthlyIncome, Earned, EarnedAmount, DataCollectionStage)
  
  organization <- readr::read_csv(stringr::str_c(.extract_path, "Organization.csv"), show_col_types = FALSE) %>%
    dplyr::select(OrganizationID, OrganizationName) %>%
    dplyr::left_join(project, by = "OrganizationID") %>%
    dplyr::select(ProjectID, OrganizationName)
  
  funder <- readr::read_csv(stringr::str_c(.extract_path, "Funder.csv"), show_col_types = FALSE)
  
  if (!.include_disabilities)
  {
    output <- list(client = tibble::as_tibble(client),
                   entry = tibble::as_tibble(entry),
                   exit = tibble::as_tibble(exit),
                   services = tibble::as_tibble(services),
                   project = dplyr::distinct(tibble::as_tibble(dplyr::left_join(project, projectcoc, by = "ProjectID")), ProjectID, .keep_all = TRUE) %>%
                     dplyr::relocate(ActiveProject, .after = tidyselect::last_col()) %>%
                     dplyr::relocate(ConsolidatedProject, .after = tidyselect::last_col()),
                   healthanddv = tibble::as_tibble(healthanddv),
                   incomebenefits = tibble::as_tibble(incomebenefits),
                   organization = tibble::as_tibble(organization),
                   funder = tibble::as_tibble(funder))
  }
  else
  {
    disabilities <- readr::read_csv(stringr::str_c(.extract_path, "Disabilities.csv"), show_col_types = FALSE) %>%
      dplyr::filter(DataCollectionStage == 1) %>% 
      dplyr::count(PersonalID, EnrollmentID, DisabilityType, DisabilityResponse) %>% 
      # dplyr::mutate(DisabilityType_Label = dplyr::case_when(DisabilityType == 5 ~ "Physical disability",
      #                                                       DisabilityType == 6 ~ "Developmental disability",
      #                                                       DisabilityType == 7 ~ "Chronic health condition",
      #                                                       DisabilityType == 8 ~ "HIV/AIDS",
      #                                                       DisabilityType == 9 ~ "Mental health problem",
      #                                                       DisabilityType == 10 ~ "Substance abuse")) %>% 
      dplyr::mutate(DisabilityType_Label = dplyr::case_match(DisabilityType,
                                                             5 ~ "Physical disability",
                                                             6 ~ "Developmental disability",
                                                             7 ~ "Chronic health condition",
                                                             8 ~ "HIV/AIDS",
                                                             9 ~ "Mental health problem",
                                                             10 ~ "Substance abuse")) %>% 
      tidyr::pivot_wider(id_cols = c(PersonalID, EnrollmentID), names_from = DisabilityType_Label, values_from = DisabilityResponse) %>% 
      dplyr::mutate(dplyr::across(`Mental health problem`:`Substance abuse`, ~ tidyr::replace_na(.x, 0)),
                    dplyr::across(`Mental health problem`:`Substance abuse`, ~ replace(., . %in% c(8, 9, 99), NA)),
                    "Substance abuse" = dplyr::case_when(`Substance abuse` > 0 ~ 1,
                                                         TRUE ~ 0))
    # dplyr::select(PersonalID, DisabilityType, DisabilityResponse) %>%
    # dplyr::mutate(DisabilityType_Label = dplyr::case_when(DisabilityResponse %in% c(0, unknown) ~ "Remove", #### NOTE: unknown used to say 8,9,99 ----
    #                                         DisabilityType == 10 & DisabilityResponse == 1 ~ "Alcohol abuse",
    #                                         DisabilityType == 10 & DisabilityResponse == 2 ~ "Drug abuse",
    #                                         DisabilityType == 10 & DisabilityResponse == 1 ~ "Alcohol and drug abuse",
    #                                         DisabilityType == 5 ~ "Physical disability",
    #                                         DisabilityType == 6 ~ "Developmental disability",
    #                                         DisabilityType == 7 ~ "Chronic health condition",
    #                                         DisabilityType == 8 ~ "HIV/AIDS",
    #                                         DisabilityType == 9 ~ "Mental health problem")) %>%
    # dplyr::select(PersonalID, DisabilityType_Label) %>%
    # dplyr::filter(DisabilityType_Label != "Remove") %>%
    # dplyr::distinct(PersonalID, DisabilityType_Label, .keep_all = TRUE) %>%
    # tidyr::pivot_wider(names_from = DisabilityType_Label,
    #             values_from = DisabilityType_Label) %>%
    # dplyr::mutate(dplyr::across(2:8,
    #               ~ if_else(is.na(.) == TRUE,
    #                         0,1)))
    output <- list(extractDate = .extract_date, # Added 03-13-24 ----
                   # exportStart = .extract_date,
                   # exportEnd = .extract_date,
                   # extractDate = as.Date(.extract_date), # Added 11-17-23 ----
                   FY = 22, # Added 11-17-23 ----
                   client = tibble::as_tibble(client),
                   entry = tibble::as_tibble(entry),
                   exit = tibble::as_tibble(exit),
                   services = tibble::as_tibble(services),
                   project = dplyr::distinct(tibble::as_tibble(dplyr::left_join(project, projectcoc, by = "ProjectID")), ProjectID, .keep_all = TRUE) %>%
                     dplyr::relocate(ActiveProject, .after = tidyselect::last_col()) %>%
                     dplyr::relocate(ConsolidatedProject, .after = tidyselect::last_col()),
                   healthanddv = tibble::as_tibble(healthanddv),
                   incomebenefits = tibble::as_tibble(incomebenefits),
                   organization = tibble::as_tibble(organization),
                   disabilities = tibble::as_tibble(disabilities),
                   funder = tibble::as_tibble(funder))
  }
  
  # if (lubridate::ymd(stringr::str_sub(.extract_path, -8, -1)) != as.Date(.extract_date))
  # {
  #   mismatched_date <- lubridate::ymd(stringr::str_sub(.extract_path, -8, -1))
  #   
  #   display_date_warning_message <- function()
  #   {
  #     cli::cli_div()
  #     cli::cli_text("\n")
  #     cli::cli_warn(c("!" = "{.strong The date supplied for {.arg .extract_date} does not match the date in the file path supplied for {.arg .extract_path}.}",
  #                     "i" = "You entered {.var {.extract_date}} for {.arg .extract_date}, but the date in the file path for {.arg .extract_path} is {.var {mismatched_date}}",
  #                     " " = "\n"))
  #     cli::cli_end()
  #   }
  #   
  #   options(warn = 0)
  #   
  #   display_date_warning_message()
  # }
  
  return(output)
}

import_hmis_fy24 <- function(.extract_path,
                             # .extract_date,
                             ...,
                             .all_files = FALSE,
                             .include_disabilities = FALSE,
                             # include_roi_report = FALSE,
                             .suppressNormalReadrCsvWarnings = TRUE)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",      quietly = TRUE)
  requireNamespace("lubridate",  quietly = TRUE)
  requireNamespace("readr",      quietly = TRUE)
  requireNamespace("tibble",     quietly = TRUE)
  requireNamespace("tidyr",      quietly = TRUE)
  requireNamespace("tidyselect", quietly = TRUE)
  
  if (.suppressNormalReadrCsvWarnings)
  {
    options(warn = - 1)
  }
  
  .extract_date <- as.Date(readr::read_csv(paste0(.extract_path, "/Export.csv"), show_col_types = FALSE)$ExportEndDate[1])
  exportStart <- as.Date(readr::read_csv(paste0(.extract_path, "/Export.csv"), show_col_types = FALSE)$ExportStartDate[1])
  
  if (!.all_files)
  {
    client <- readr::read_csv(paste0(.extract_path, "Client.csv"), show_col_types = FALSE) |>
      dplyr::mutate(Gender = factor(dplyr::case_when(Woman == 1 ~ "Female",
                                                     Man == 1 ~ "Male",
                                                     NonBinary == 1 ~ "No Single Gender",
                                                     CulturallySpecific == 1 ~ "Culterally Specific",
                                                     Questioning == 1 ~ "Questioning",
                                                     DifferentIdentity == 1 ~ "Different Identity",
                                                     !is.na(GenderNone) ~ "Data not collected",
                                                     .default = "Other"),
                                    levels = c("Female",
                                               "Male",
                                               "Culterally Specific",
                                               "No Single Gender",
                                               "Questioning",
                                               "Other",
                                               "Data not collected")),
                    is_Transgender = dplyr::case_when(Transgender == 1 ~ TRUE,
                                                      !is.na(GenderNone) ~ NA,
                                                      .default = FALSE),
                    Veteran_Status = factor(dplyr::case_when(VeteranStatus == 1 ~ "Yes",
                                                             VeteranStatus == 0 ~ "No",
                                                             VeteranStatus %in% c(8, 9) ~ "Unknown",
                                                             VeteranStatus == 99 ~ "Data not collected",
                                                             .default = NA_character_),
                                            levels = c("Yes",
                                                       "No",
                                                       "Unknown",
                                                       "Data not collected",
                                                       NA_character_)),
                    is_Veteran = dplyr::if_else(Veteran_Status == "Yes", TRUE, FALSE),
                    Race_Ethnicity = factor(dplyr::case_when(AmIndAKNative    == 1 & (Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "American Indian",
                                                             Asian            == 1 & (AmIndAKNative + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Asian",
                                                             BlackAfAmerican  == 1 & (AmIndAKNative + Asian + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Black",
                                                             HispanicLatinaeo == 1 & (AmIndAKNative + Asian + BlackAfAmerican + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Hispanic/Latino",
                                                             HispanicLatinaeo == 1 & White == 1 & (AmIndAKNative + Asian + BlackAfAmerican + MidEastNAfrican + NativeHIPacific) == 0 ~ "Hispanic/Latino",
                                                             MidEastNAfrican  == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + NativeHIPacific + White) == 0 ~ "Middle Eastern / North African",
                                                             NativeHIPacific  == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + White) == 0 ~ "Pacific Islander",
                                                             White            == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific) == 0 ~ "White",
                                                             (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) >= 2 & (HispanicLatinaeo + White) <= 1 ~ "Two or more races",
                                                             !is.na(RaceNone) ~ "Data not collected",
                                                             .default = "Data not collected"),
                                            levels = c("American Indian",
                                                       "Asian",
                                                       "Black",
                                                       "Hispanic/Latino",
                                                       "Middle Eastern / North African",
                                                       "Pacific Islander",
                                                       "White",
                                                       "Two or more races",
                                                       "Data not collected")),
                    Age = lubridate::interval(DOB, lubridate::ymd(.extract_date)) %/% lubridate::years(1),
                    Age_Group = factor(dplyr::case_when(Age %in% c(0:17) ~ "Under 18",
                                                        Age %in% c(18:24) ~ "18 to 24",
                                                        Age %in% c(25:34) ~ "25 to 34",
                                                        Age %in% c(35:44) ~ "35 to 44",
                                                        Age %in% c(45:54) ~ "45 to 54",
                                                        Age %in% c(55:64) ~ "55 to 64",
                                                        Age >= 65 ~ "65 and Over",
                                                        is.na(Age) ~ "Unknown",
                                                        .default = "Unknown"),
                                       levels = c("Under 18",
                                                  "18 to 24",
                                                  "25 to 34",
                                                  "35 to 44",
                                                  "45 to 54",
                                                  "55 to 64",
                                                  "65 and Over",
                                                  "Unknown"))) |>
      dplyr::select(PersonalID,
                    Race_Ethnicity,
                    Gender,
                    Transgender = is_Transgender,
                    is_Veteran,
                    Veteran_Status,
                    DOB,
                    Age,
                    Age_Group)
    
    client_dummy <- client |>
      dplyr::select(PersonalID, DOB)
    
    entry <- readr::read_csv(paste0(.extract_path, "Enrollment.csv"), show_col_types = FALSE) |>
      dplyr::select(EnrollmentID,
                    HouseholdID,
                    PersonalID,
                    ProjectID,
                    EntryDate,
                    MoveInDate,
                    LivingSituationEntry = LivingSituation,
                    HOH = RelationshipToHoH,
                    DisablingCondition,
                    LengthOfStay,
                    LOSUnderThreshold,
                    PreviousStreetESSH,
                    DateToStreetESSH,
                    TimesHomelessPastThreeYears,
                    MonthsHomelessPastThreeYears) |>
      dplyr::left_join(dplyr::select(client, PersonalID, DOB), by = "PersonalID") |> 
      dplyr::left_join(dplyr::select(readr::read_csv(paste0(.extract_path, "Project.csv"), show_col_types = FALSE), ProjectID, ProjectType), by = "ProjectID") |> 
      dplyr::mutate(Chronic = dplyr::case_match(DisablingCondition,
                                                1 ~ dplyr::case_when(LivingSituationEntry %in% c(101, 116, 118)
                                                                     & LengthOfStay == 5
                                                                     | EntryDate - DateToStreetESSH >= 365
                                                                     ~ TRUE,
                                                                     
                                                                     LivingSituationEntry %in% c(101, 116, 118)
                                                                     & TimesHomelessPastThreeYears == 4
                                                                     & MonthsHomelessPastThreeYears %in% c(112, 113)
                                                                     ~ TRUE,
                                                                     
                                                                     LivingSituationEntry %in% c(215, 206, 207, 225, 204, 205)
                                                                     & LOSUnderThreshold == 1
                                                                     & PreviousStreetESSH == 1
                                                                     & EntryDate - DateToStreetESSH >= 365
                                                                     ~ TRUE,
                                                                     
                                                                     LivingSituationEntry %in% c(215, 206, 207, 225, 204, 205)
                                                                     & LOSUnderThreshold == 1
                                                                     & PreviousStreetESSH == 1
                                                                     & TimesHomelessPastThreeYears == 4
                                                                     & MonthsHomelessPastThreeYears %in% c(112, 113)
                                                                     ~ TRUE,
                                                                     
                                                                     .default = FALSE),
                                                0 ~ FALSE,
                                                .default = FALSE),
                    Chronic_at_Entry = factor(dplyr::if_else(HOH == 1 | floor(as.double(EntryDate - DOB) / 365) >= 18,
                                                             dplyr::case_match(DisablingCondition,
                                                                               1 ~ dplyr::if_else(ProjectType %in% c(0, 1, 4, 8),
                                                                                                  dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                   TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                       MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                       MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                       MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                       .default = "missing"),
                                                                                                                   TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                   TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                   TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                   .default = "missing"),
                                                                                                  dplyr::case_when(LivingSituationEntry %in% c(100:199) ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                           TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                               MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                               MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                               MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                               .default = "missing"),
                                                                                                                                                                           TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                           TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                           TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                           .default = "missing"),
                                                                                                                   LivingSituationEntry %in% c(200:299) ~ dplyr::case_when(LOSUnderThreshold == 1 ~ dplyr::case_when(PreviousStreetESSH == 1 ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                                                                                                                TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                                                                                                                    MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                                                                                                                    MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                                                    MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                                                    .default = "missing"),
                                                                                                                                                                                                                                                                TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                                                                                                                TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                .default = "missing"),
                                                                                                                                                                                                                     PreviousStreetESSH == 0 ~ "NO",
                                                                                                                                                                                                                     .default = "missing"),
                                                                                                                                                                           LOSUnderThreshold == 0 ~ "NO",
                                                                                                                                                                           .default = "missing"),
                                                                                                                   LivingSituationEntry %in% c(0:99, 300:499) ~ dplyr::case_when(LOSUnderThreshold == 1 ~ dplyr::case_when(PreviousStreetESSH == 1 ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                                                                                                                      TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                                                                                                                          MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                                                                                                                          MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                                                          MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                                                          .default = "missing"),
                                                                                                                                                                                                                                                                      TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                                                                                                                      TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                      TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                      .default = "missing"),
                                                                                                                                                                                                                           PreviousStreetESSH == 0 ~ "NO",
                                                                                                                                                                                                                           .default = "missing"),
                                                                                                                                                                                 LOSUnderThreshold == 0 ~ "NO",
                                                                                                                                                                                 .default = "missing"),
                                                                                                                   is.na(LivingSituationEntry) ~ "missing",
                                                                                                                   .default = NA)),
                                                                               0 ~ "NO",
                                                                               8 ~ "DK/PNTA",
                                                                               9 ~ "DK/PNTA",
                                                                               99 ~ "missing",
                                                                               .default = "missing"),
                                                             NA),
                                              levels = c("YES", "NO", "DK/PNTA", "missing"))) |> 
      dplyr::select(-DOB, -ProjectType)
    
    entry_dummy <- entry |>
      dplyr::select(PersonalID, EnrollmentID, EntryDate) |>
      dplyr::left_join(client_dummy, by = c("PersonalID")) |>
      dplyr::mutate(Age_at_Entry = lubridate::interval(DOB, EntryDate) %/% lubridate::years(1)) |>
      dplyr::select(-EntryDate, -DOB)
    
    entry <- entry |>
      dplyr::left_join(entry_dummy, by = c("PersonalID", "EnrollmentID"))
    
    rm(client_dummy, entry_dummy)
    
    # chronic <- entry |>
    #   dplyr::mutate(MaybeChronic = maybeChronic_list(entry, .extract_date)) |>
    #   dplyr::select(c("EnrollmentID", "PersonalID", "MaybeChronic"))
    
    # entry <- entry |>
    #   dplyr::left_join(chronic, by = c("EnrollmentID", "PersonalID"))
    
    exit <- readr::read_csv(paste0(.extract_path, "Exit.csv"), show_col_types = FALSE) |>
      dplyr::select(EnrollmentID, PersonalID, ExitDate, Destination)
    
    services <- readr::read_csv(paste0(.extract_path, "Services.csv"), show_col_types = FALSE) |>
      dplyr::select(EnrollmentID, PersonalID, LatestServiceDate = DateProvided) |>
      dplyr::arrange(dplyr::desc(LatestServiceDate)) |>
      dplyr::distinct(EnrollmentID, .keep_all = TRUE)
    
    ceparticipation0 <- readr::read_csv(paste0(.extract_path, "CEParticipation.csv"), show_col_types = FALSE) |>
      dplyr::mutate(dplyr::across(AccessPoint:ReceivesReferrals, as.logical))
    
    ceparticipation_dummy <- ceparticipation0 |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(CEParticipationStatusStartDate)) |>
      dplyr::summarise(CE_History = sum(ReceivesReferrals)) |>
      dplyr::ungroup()
    
    ceparticipation <- dplyr::left_join(ceparticipation0, ceparticipation_dummy, by = c("ProjectID")) |>
      dplyr::mutate(CE_Participant = ReceivesReferrals,
                    CE_Assessor = AccessPoint,
                    CE_Status = factor(dplyr::case_when(CE_Participant ~ "Current",
                                                        !CE_Participant & CE_History > 0 ~ "Former",
                                                        !CE_Participant & CE_History == 0 ~ "Never",
                                                        .default = "Unknown"),
                                       levels = c("Current", "Former", "Never", "Unknown"))) |>
      dplyr::select(-CEParticipationID, -AccessPoint, -PreventionAssessment, -CrisisAssessment, -HousingAssessment, -DirectServices, -ReceivesReferrals, -DateCreated, -DateUpdated, -UserID, -DateDeleted, -ExportID, -CE_History) |>
      dplyr::relocate(CE_Participant:CE_Assessor, .after = ProjectID) |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(CEParticipationStatusStartDate)) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      dplyr::ungroup()
    
    rm(ceparticipation0, ceparticipation_dummy)
    
    hmisparticipation0 <- readr::read_csv(paste0(.extract_path, "HMISParticipation.csv"), show_col_types = FALSE)
    
    hmisparticipation_dummy <- hmisparticipation0 |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(HMISParticipationStatusStartDate)) |>
      dplyr::summarise(HMIS_History = sum(HMISParticipationType)) |>
      dplyr::ungroup()
    
    hmisparticipation <- dplyr::left_join(hmisparticipation0, hmisparticipation_dummy, by = c("ProjectID")) |>
      dplyr::mutate(HMIS_Participant = as.logical(HMISParticipationType),
                    HMIS_Status = factor(dplyr::case_when(HMIS_Participant ~ "Current",
                                                          !HMIS_Participant & HMIS_History > 0 ~ "Former",
                                                          !HMIS_Participant & HMIS_History == 0 ~ "Never",
                                                          .default = "Unknown"),
                                         levels = c("Current", "Former", "Never", "Unknown"))) |>
      dplyr::select(-HMISParticipationID, -HMISParticipationType, -DateCreated, -DateUpdated, -UserID, -DateDeleted, -ExportID, -HMIS_History) |>
      dplyr::relocate(HMIS_Participant, .after = ProjectID) |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(HMISParticipationStatusStartDate)) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      dplyr::ungroup()
    
    rm(hmisparticipation0, hmisparticipation_dummy)
    
    # pTypeLabels <- pType_FY24$Title
    
    project <- readr::read_csv(paste0(.extract_path, "Project.csv"), show_col_types = FALSE) |>
      dplyr::mutate(ConsolidatedProject = dplyr::if_else(ProjectID %in% consolidatedProjects, TRUE, FALSE),
                    # ActiveProject = dplyr::if_else(!stringr::str_detect(ProjectName, "ZZZ"), TRUE, FALSE),
                    ActiveProject = dplyr::if_else(!grepl("ZZZ", ProjectName), TRUE, FALSE),
                    Target_Population = factor(dplyr::case_match(TargetPopulation,
                                                                 1 ~ "DV",
                                                                 3 ~ "HIV",
                                                                 4 ~ "None",
                                                                 .default = NA_character_),
                                               levels = c("DV", "HIV", "None", NA_character_)),
                    RRH_Subtype = factor(dplyr::case_when(ProjectType == 13 & RRHSubType == 1 ~ "SSO",
                                                          ProjectType == 13 & RRHSubType == 2 ~ "Housing",
                                                          ProjectType == 13 & !RRHSubType %in% c(1, 2) ~ "Unspecified (Missing Data)",
                                                          .default = NA_character_),
                                         levels = c("SSO", "Housing", "Unspecified (Missing Data)", NA_character_)),
                    # proj_TypeLabels = these_project("types", ProjectType, full_label = TRUE, hmis_extract = "ignore"),
                    # Project_Label = factor(proj_TypeLabels,
                    #                        levels = pTypeLabels),
                    Project_Label = factor(these_project("types", ProjectType, full_label = TRUE, hmis_extract = "ignore"),
                                           levels = pType_FY24$Title)) |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(DateUpdated)) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      dplyr::select(OrganizationID, ProjectID, ProjectName, ProjectType, Project_Label, RRH_Subtype, Target_Population, PITCount, ActiveProject, ConsolidatedProject) |>
      dplyr::ungroup()
    
    projectcoc <- readr::read_csv(paste0(.extract_path, "ProjectCoC.csv"), show_col_types = FALSE) |>
      dplyr::group_by(ProjectID) |>
      dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(DateUpdated)) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      dplyr::select(ProjectID, CoCCode) |>
      dplyr::ungroup()
    
    project <- project |>
      dplyr::left_join(projectcoc, by = c("ProjectID")) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      dplyr::left_join(hmisparticipation, by = c("ProjectID")) |>
      dplyr::left_join(ceparticipation, by = c("ProjectID")) |>
      dplyr::relocate(ActiveProject, .after = tidyselect::last_col()) |>
      dplyr::relocate(ConsolidatedProject, .after = tidyselect::last_col())
    
    healthanddv <- readr::read_csv(paste0(.extract_path, "HealthAndDV.csv"), show_col_types = FALSE) |>
      dplyr::arrange(dplyr::desc(InformationDate)) |>
      dplyr::distinct(PersonalID, .keep_all = TRUE) |>
      dplyr::select(PersonalID, DomesticViolenceSurvivor, CurrentlyFleeing) |> 
      encryptr::encrypt(CurrentlyFleeing,
                        public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_d.pub"))
    
    incomebenefits <- readr::read_csv(paste0(.extract_path, "IncomeBenefits.csv"), show_col_types = FALSE) |>
      dplyr::group_by(EnrollmentID, PersonalID) |>
      dplyr::filter(DataCollectionStage == max(DataCollectionStage)) |>
      dplyr::ungroup() |>
      dplyr::select(EnrollmentID, PersonalID, IncomeFromAnySource, TotalMonthlyIncome, Earned, EarnedAmount, DataCollectionStage)
    
    inventory <- readr::read_csv(paste0(.extract_path, "Inventory.csv"), show_col_types = FALSE)
    
    organization <- readr::read_csv(paste0(.extract_path, "Organization.csv"), show_col_types = FALSE) |>
      dplyr::select(OrganizationID, OrganizationName) |>
      dplyr::left_join(project, by = "OrganizationID") |>
      dplyr::select(ProjectID, OrganizationName)
    
    funder <- readr::read_csv(paste0(.extract_path, "Funder.csv"), show_col_types = FALSE)
    
    if (!.include_disabilities)
    {
      output <- list(exportInterval = lubridate::interval(exportStart, .extract_date), # NEW: 06/17/2024 ----
                     # extractDate = as.Date(.extract_date),
                     # exportStart = exportStart, # NEW: 06/17/2024 ----
                     # exportEnd = .extract_date, # NEW: 06/17/2024 ----
                     FY = 24,
                     client = tibble::as_tibble(client),
                     entry = tibble::as_tibble(entry),
                     exit = tibble::as_tibble(exit),
                     funder = tibble::as_tibble(funder),
                     healthanddv = tibble::as_tibble(healthanddv),
                     incomebenefits = tibble::as_tibble(incomebenefits),
                     inventory = tibble::as_tibble(inventory),
                     organization = tibble::as_tibble(organization),
                     project = tibble::as_tibble(project),
                     services = tibble::as_tibble(services))
    }
    else
    {
      disabilities <- readr::read_csv(paste0(.extract_path, "Disabilities.csv"), show_col_types = FALSE) |>
        dplyr::filter(DataCollectionStage == 1) |>
        dplyr::count(PersonalID, EnrollmentID, DisabilityType, DisabilityResponse) |>
        dplyr::mutate(DisabilityType_Label = dplyr::case_match(DisabilityType,
                                                               5 ~ "Physical",
                                                               6 ~ "Developmental",
                                                               7 ~ "Chronic Health",
                                                               8 ~ "HIV/AIDS",
                                                               9 ~ "Mental Health",
                                                               10 ~ "Substance Use")) |>
        tidyr::pivot_wider(id_cols = c(PersonalID, EnrollmentID), names_from = DisabilityType_Label, values_from = DisabilityResponse) |>
        dplyr::mutate(dplyr::across(`Mental Health`:`Substance Use`, \(x) tidyr::replace_na(x, 0)),
                      dplyr::across(`Mental Health`:`Substance Use`, \(x) dplyr::case_match(x,
                                                                                            8 ~ NA,
                                                                                            9 ~ NA,
                                                                                            99 ~ NA,
                                                                                            .default = x)),
                      "Substance Use" = dplyr::if_else(`Substance Use` > 0, 1, 0))
      
      output <- list(exportInterval = lubridate::interval(exportStart, .extract_date), # NEW: 06/17/2024 ----
                     # extractDate = as.Date(.extract_date),
                     # exportStart = exportStart, # NEW: 06/17/2024 ----
                     # exportEnd = .extract_date, # NEW: 06/17/2024 ----
                     FY = 24,
                     client = tibble::as_tibble(client),
                     disabilities = tibble::as_tibble(disabilities),
                     entry = tibble::as_tibble(entry),
                     exit = tibble::as_tibble(exit),
                     funder = tibble::as_tibble(funder),
                     healthanddv = tibble::as_tibble(healthanddv),
                     incomebenefits = tibble::as_tibble(incomebenefits),
                     inventory = tibble::as_tibble(inventory),
                     organization = tibble::as_tibble(organization),
                     project = tibble::as_tibble(project),
                     services = tibble::as_tibble(services))
      
      # if (include_roi_report)
      # {
      #   output <- output |> append(list(roi), after = 12)
      #   
      #   names(output)[13] <- "roi"
      # }
    }
    
    # if (lubridate::ymd(stringr::str_sub(.extract_path, -9, -2)) != as.Date(.extract_date))
    # if (lubridate::ymd(substr(.extract_path, nchar(.extract_path) -8, nchar(.extract_path) -1)) != as.Date(.extract_date))
    # {
    #   # mismatched_date <- lubridate::ymd(stringr::str_sub(.extract_path, -8, -1))
    #   
    #   mismatched_date <- lubridate::ymd(substr(.extract_path, nchar(.extract_path) -8, nchar(.extract_path) -1))
    #   
    #   display_date_warning_message <- function()
    #   {
    #     cli::cli_div()
    #     cli::cli_text("\n")
    #     cli::cli_warn(c("!" = "{.strong The date supplied for {.arg .extract_date} does not match the date in the file path supplied for {.arg .extract_path}.}",
    #                     "i" = "You entered {.var {.extract_date}} for {.arg .extract_date}, but the date in the file path for {.arg .extract_path} is {.var {mismatched_date}}",
    #                     " " = "\n"))
    #     cli::cli_end()
    #   }
    #   
    #   options(warn = 0)
    #   
    #   display_date_warning_message()
    # }
    
    return(output)
  }
  else # Read in the FULL HMIS extract
  {
    affiliation <- readr::read_csv(paste0(.extract_path, "/Affiliation.csv"), show_col_types = FALSE)
    
    assessment <- readr::read_csv(paste0(.extract_path, "/Assessment.csv"), show_col_types = FALSE)
    
    assessment_questions <- readr::read_csv(paste0(.extract_path, "/AssessmentQuestions.csv"), show_col_types = FALSE)
    
    assessment_results <- readr::read_csv(paste0(.extract_path, "/AssessmentResults.csv"), show_col_types = FALSE)
    
    ce_participation <- readr::read_csv(paste0(.extract_path, "/CEParticipation.csv"), show_col_types = FALSE)
    
    client <- readr::read_csv(paste0(.extract_path, "/Client.csv"), show_col_types = FALSE) |> 
      encryptr::encrypt(FirstName,
                        MiddleName,
                        LastName,
                        SSN,
                        public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_d.pub"))
    
    current_living_situation <- readr::read_csv(paste0(.extract_path, "/CurrentLivingSituation.csv"), show_col_types = FALSE)
    
    disabilities <- readr::read_csv(paste0(.extract_path, "/Disabilities.csv"), show_col_types = FALSE)
    
    employment_education <- readr::read_csv(paste0(.extract_path, "/EmploymentEducation.csv"), show_col_types = FALSE)
    
    enrollment <- readr::read_csv(paste0(.extract_path, "/Enrollment.csv"), show_col_types = FALSE)
    
    event <- readr::read_csv(paste0(.extract_path, "/Event.csv"), show_col_types = FALSE)
    
    exit <- readr::read_csv(paste0(.extract_path, "/Exit.csv"), show_col_types = FALSE) # # |> 
    # encryptr::encrypt(EmailSocialMedia,
    #                   Telephone,
    #                   public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_d.pub"))
    
    export <- readr::read_csv(paste0(.extract_path, "/Export.csv"), show_col_types = FALSE)
    
    funder <- readr::read_csv(paste0(.extract_path, "/Funder.csv"), show_col_types = FALSE)
    
    health_and_dv <- readr::read_csv(paste0(.extract_path, "/HealthAndDV.csv"), show_col_types = FALSE) |> 
      encryptr::encrypt(CurrentlyFleeing,
                        # GeneralHealthStatus,
                        # DentalHealthStatus,
                        # MentalHealthStatus,
                        # PregnancyStatus,
                        # DueDate,
                        public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_d.pub"))
    
    hmis_participation <- readr::read_csv(paste0(.extract_path, "/HMISParticipation.csv"), show_col_types = FALSE)
    
    income_benefits <- readr::read_csv(paste0(.extract_path, "/IncomeBenefits.csv"), show_col_types = FALSE)
    
    inventory <- readr::read_csv(paste0(.extract_path, "/Inventory.csv"), show_col_types = FALSE)
    
    organization <- readr::read_csv(paste0(.extract_path, "/Organization.csv"), show_col_types = FALSE)
    
    project <- readr::read_csv(paste0(.extract_path, "/Project.csv"), show_col_types = FALSE)
    
    project_coc <- readr::read_csv(paste0(.extract_path, "/ProjectCoC.csv"), show_col_types = FALSE)
    
    services <- readr::read_csv(paste0(.extract_path, "/Services.csv"), show_col_types = FALSE)
    
    user <- readr::read_csv(paste0(.extract_path, "/User.csv"), show_col_types = FALSE) # # |> 
    # encryptr::encrypt(UserPhone,
    #                   UserEmail,
    #                   public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_d.pub"))
    
    youth_education_status <- readr::read_csv(paste0(.extract_path, "/YouthEducationStatus.csv"), show_col_types = FALSE)
    
    output <- list(export_interval = lubridate::interval(exportStart, .extract_date), # NEW: 06/17/2024 ----
                   # extractDate = as.Date(.extract_date),
                   # exportStart = exportStart, # NEW: 06/17/2024 ----
                   # exportEnd = .extract_date, # NEW: 06/17/2024 ----
                   affiliation = affiliation,
                   assessment = assessment,
                   assessment_questions = assessment_questions,
                   assessment_results = assessment_results,
                   ce_participation = ce_participation,
                   client = client,
                   current_living_situation = current_living_situation,
                   disabilities = disabilities,
                   employment_education = employment_education,
                   enrollment = enrollment,
                   event = event,
                   exit = exit,
                   export = export,
                   funder = funder,
                   health_and_dv = health_and_dv,
                   hmis_participation = hmis_participation,
                   income_benefits = income_benefits,
                   inventory = inventory,
                   organization = organization,
                   project = project,
                   project_coc = project_coc,
                   services = services,
                   user = user,
                   youth_education_status = youth_education_status)
    
    # if (include_roi_report)
    # {
    #   output <- output |> append(list(roi), after = 22)
    #   
    #   names(output)[23] <- "roi"
    # }
    
    return(output)
  }
}

# import_hmis_fy24 <- function(.extract_path,
#                              .extract_date,
#                              ...,
#                              .include_disabilities = FALSE,
#                              include_roi_report = FALSE,
#                              .suppressNormalReadrCsvWarnings = TRUE)
# {
#   rlang::check_dots_empty()
#   
#   requireNamespace("dplyr",      quietly = TRUE)
#   requireNamespace("lubridate",  quietly = TRUE)
#   requireNamespace("readr",      quietly = TRUE)
#   # requireNamespace("stringr",    quietly = TRUE)
#   requireNamespace("tibble",     quietly = TRUE)
#   requireNamespace("tidyr",      quietly = TRUE)
#   requireNamespace("tidyselect", quietly = TRUE)
#   
#   if (.suppressNormalReadrCsvWarnings)
#   {
#     options(warn = - 1)
#   }
#   
#   client <- readr::read_csv(paste0(.extract_path, "Client.csv"), show_col_types = FALSE) |>
#     dplyr::mutate(Gender = factor(dplyr::case_when(Woman == 1 ~ "Female",
#                                                    Man == 1 ~ "Male",
#                                                    NonBinary == 1 ~ "No Single Gender",
#                                                    CulturallySpecific == 1 ~ "Culterally Specific",
#                                                    Questioning == 1 ~ "Questioning",
#                                                    DifferentIdentity == 1 ~ "Different Identity",
#                                                    !is.na(GenderNone) ~ "Data not collected",
#                                                    .default = "Other"),
#                                   levels = c("Female",
#                                              "Male",
#                                              "Culterally Specific",
#                                              "No Single Gender",
#                                              "Questioning",
#                                              "Other",
#                                              "Data not collected")),
#                   is_Transgender = dplyr::case_when(Transgender == 1 ~ TRUE,
#                                                     !is.na(GenderNone) ~ NA,
#                                                     .default = FALSE),
#                   Veteran_Status = factor(dplyr::case_when(VeteranStatus == 1 ~ "Yes",
#                                                            VeteranStatus == 0 ~ "No",
#                                                            VeteranStatus %in% c(8, 9) ~ "Unknown",
#                                                            VeteranStatus == 99 ~ "Data not collected",
#                                                            .default = NA_character_),
#                                           levels = c("Yes",
#                                                      "No",
#                                                      "Unknown",
#                                                      "Data not collected",
#                                                      NA_character_)),
#                   is_Veteran = dplyr::if_else(Veteran_Status == "Yes", TRUE, FALSE),
#                   Race_Ethnicity = factor(dplyr::case_when(AmIndAKNative    == 1 & (Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "American Indian",
#                                                            Asian            == 1 & (AmIndAKNative + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Asian",
#                                                            BlackAfAmerican  == 1 & (AmIndAKNative + Asian + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Black",
#                                                            HispanicLatinaeo == 1 & (AmIndAKNative + Asian + BlackAfAmerican + MidEastNAfrican + NativeHIPacific + White) == 0 ~ "Hispanic/Latino",
#                                                            HispanicLatinaeo == 1 & White == 1 & (AmIndAKNative + Asian + BlackAfAmerican + MidEastNAfrican + NativeHIPacific) == 0 ~ "Hispanic/Latino",
#                                                            MidEastNAfrican  == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + NativeHIPacific + White) == 0 ~ "Middle Eastern / North African",
#                                                            NativeHIPacific  == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + White) == 0 ~ "Pacific Islander",
#                                                            White            == 1 & (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific) == 0 ~ "White",
#                                                            (AmIndAKNative + Asian + BlackAfAmerican + HispanicLatinaeo + MidEastNAfrican + NativeHIPacific + White) >= 2 & (HispanicLatinaeo + White) <= 1 ~ "Two or more races",
#                                                            !is.na(RaceNone) ~ "Data not collected",
#                                                            .default = "Data not collected"),
#                                           levels = c("American Indian",
#                                                      "Asian",
#                                                      "Black",
#                                                      "Hispanic/Latino",
#                                                      "Middle Eastern / North African",
#                                                      "Pacific Islander",
#                                                      "White",
#                                                      "Two or more races",
#                                                      "Data not collected")),
#                   Age = lubridate::interval(DOB, lubridate::ymd(.extract_date)) %/% lubridate::years(1),
#                   Age_Group = factor(dplyr::case_when(Age %in% c(0:17) ~ "Under 18",
#                                                       Age %in% c(18:24) ~ "18 to 24",
#                                                       Age %in% c(25:34) ~ "25 to 34",
#                                                       Age %in% c(35:44) ~ "35 to 44",
#                                                       Age %in% c(45:54) ~ "45 to 54",
#                                                       Age %in% c(55:64) ~ "55 to 64",
#                                                       Age >= 65 ~ "65 and Over",
#                                                       is.na(Age) ~ "Unknown",
#                                                       .default = "Unknown"),
#                                      levels = c("Under 18",
#                                                 "18 to 24",
#                                                 "25 to 34",
#                                                 "35 to 44",
#                                                 "45 to 54",
#                                                 "55 to 64",
#                                                 "65 and Over",
#                                                 "Unknown"))) |>
#     dplyr::select(PersonalID,
#                   Race_Ethnicity,
#                   Gender,
#                   Transgender = is_Transgender,
#                   is_Veteran,
#                   Veteran_Status,
#                   DOB,
#                   Age,
#                   Age_Group)
#   
#   client_dummy <- client |>
#     dplyr::select(PersonalID, DOB)
#   
#   entry <- readr::read_csv(paste0(.extract_path, "Enrollment.csv"), show_col_types = FALSE) |>
#     dplyr::select(EnrollmentID,
#                   HouseholdID,
#                   PersonalID,
#                   ProjectID,
#                   EntryDate,
#                   MoveInDate,
#                   LivingSituationEntry = LivingSituation,
#                   HOH = RelationshipToHoH,
#                   DisablingCondition,
#                   LengthOfStay,
#                   LOSUnderThreshold,
#                   PreviousStreetESSH,
#                   DateToStreetESSH,
#                   TimesHomelessPastThreeYears,
#                   MonthsHomelessPastThreeYears) |>
#     dplyr::mutate(Chronic = dplyr::case_match(DisablingCondition,
#                                               1 ~ dplyr::case_when(LivingSituationEntry %in% c(101, 116, 118)
#                                                                    & LengthOfStay == 5
##                                                                    | EntryDate - DateToStreetESSH >= 365
#                                                                    ~ TRUE,
#                                                                    
#                                                                    LivingSituationEntry %in% c(101, 116, 118)
#                                                                    & TimesHomelessPastThreeYears == 4
#                                                                    & MonthsHomelessPastThreeYears %in% c(112, 113)
#                                                                    ~ TRUE,
#                                                                    
#                                                                    LivingSituationEntry %in% c(215, 206, 207, 225, 204, 205)
#                                                                    & LOSUnderThreshold == 1
#                                                                    & PreviousStreetESSH == 1
#                                                                    & EntryDate - DateToStreetESSH >= 365
#                                                                    ~ TRUE,
#                                                                    
#                                                                    LivingSituationEntry %in% c(215, 206, 207, 225, 204, 205)
#                                                                    & LOSUnderThreshold == 1
#                                                                    & PreviousStreetESSH == 1
#                                                                    & TimesHomelessPastThreeYears == 4
#                                                                    & MonthsHomelessPastThreeYears %in% c(112, 113)
#                                                                    ~ TRUE,
#                                                                    
#                                                                    .default = FALSE),
#                                               0 ~ FALSE,
#                                               .default = FALSE))
#   
#   entry_dummy <- entry |>
#     dplyr::select(PersonalID, EnrollmentID, EntryDate) |>
#     dplyr::left_join(client_dummy, by = c("PersonalID")) |>
#     dplyr::mutate(Age_at_Entry = lubridate::interval(DOB, EntryDate) %/% lubridate::years(1)) |>
#     dplyr::select(-EntryDate, -DOB)
#   
#   entry <- entry |>
#     dplyr::left_join(entry_dummy, by = c("PersonalID", "EnrollmentID"))
#   
#   rm(client_dummy, entry_dummy)
#   
#   chronic <- entry |>
#     dplyr::mutate(MaybeChronic = maybeChronic_list(entry, .extract_date)) |>
#     dplyr::select(c("EnrollmentID", "PersonalID", "MaybeChronic"))
#   
#   entry <- entry |>
#     dplyr::left_join(chronic, by = c("EnrollmentID", "PersonalID"))
#   
#   exit <- readr::read_csv(paste0(.extract_path, "Exit.csv"), show_col_types = FALSE) |>
#     dplyr::select(EnrollmentID, PersonalID, ExitDate, Destination)
#   
#   services <- readr::read_csv(paste0(.extract_path, "Services.csv"), show_col_types = FALSE) |>
#     dplyr::select(EnrollmentID, PersonalID, LatestServiceDate = DateProvided) |>
#     dplyr::arrange(dplyr::desc(LatestServiceDate)) |>
#     dplyr::distinct(EnrollmentID, .keep_all = TRUE)
#   
#   ceparticipation0 <- readr::read_csv(paste0(.extract_path, "CEParticipation.csv"), show_col_types = FALSE) |>
#     dplyr::mutate(dplyr::across(AccessPoint:ReceivesReferrals, as.logical))
#   
#   ceparticipation_dummy <- ceparticipation0 |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(CEParticipationStatusStartDate)) |>
#     dplyr::summarise(CE_History = sum(ReceivesReferrals)) |>
#     dplyr::ungroup()
#   
#   ceparticipation <- dplyr::left_join(ceparticipation0, ceparticipation_dummy, by = c("ProjectID")) |>
#     dplyr::mutate(CE_Participant = ReceivesReferrals,
#                   CE_Assessor = AccessPoint,
#                   CE_Status = factor(dplyr::case_when(CE_Participant ~ "Current",
#                                                       !CE_Participant & CE_History > 0 ~ "Former",
#                                                       !CE_Participant & CE_History == 0 ~ "Never",
#                                                       .default = "Unknown"),
#                                      levels = c("Current", "Former", "Never", "Unknown"))) |>
#     dplyr::select(-CEParticipationID, -AccessPoint, -PreventionAssessment, -CrisisAssessment, -HousingAssessment, -DirectServices, -ReceivesReferrals, -DateCreated, -DateUpdated, -UserID, -DateDeleted, -ExportID, -CE_History) |>
#     dplyr::relocate(CE_Participant:CE_Assessor, .after = ProjectID) |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(CEParticipationStatusStartDate)) |>
#     dplyr::distinct(ProjectID, .keep_all = TRUE) |>
#     dplyr::ungroup()
#   
#   rm(ceparticipation0, ceparticipation_dummy)
#   
#   hmisparticipation0 <- readr::read_csv(paste0(.extract_path, "HMISParticipation.csv"), show_col_types = FALSE)
#   
#   hmisparticipation_dummy <- hmisparticipation0 |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(HMISParticipationStatusStartDate)) |>
#     dplyr::summarise(HMIS_History = sum(HMISParticipationType)) |>
#     dplyr::ungroup()
#   
#   hmisparticipation <- dplyr::left_join(hmisparticipation0, hmisparticipation_dummy, by = c("ProjectID")) |>
#     dplyr::mutate(HMIS_Participant = as.logical(HMISParticipationType),
#                   HMIS_Status = factor(dplyr::case_when(HMIS_Participant ~ "Current",
#                                                         !HMIS_Participant & HMIS_History > 0 ~ "Former",
#                                                         !HMIS_Participant & HMIS_History == 0 ~ "Never",
#                                                         .default = "Unknown"),
#                                        levels = c("Current", "Former", "Never", "Unknown"))) |>
#     dplyr::select(-HMISParticipationID, -HMISParticipationType, -DateCreated, -DateUpdated, -UserID, -DateDeleted, -ExportID, -HMIS_History) |>
#     dplyr::relocate(HMIS_Participant, .after = ProjectID) |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(HMISParticipationStatusStartDate)) |>
#     dplyr::distinct(ProjectID, .keep_all = TRUE) |>
#     dplyr::ungroup()
#   
#   rm(hmisparticipation0, hmisparticipation_dummy)
#   
#   # pTypeLabels <- pType_FY24$Title
#   
#   project <- readr::read_csv(paste0(.extract_path, "Project.csv"), show_col_types = FALSE) |>
#     dplyr::mutate(ConsolidatedProject = dplyr::if_else(ProjectID %in% consolidatedProjects, TRUE, FALSE),
#                   # ActiveProject = dplyr::if_else(!stringr::str_detect(ProjectName, "ZZZ"), TRUE, FALSE),
#                   ActiveProject = dplyr::if_else(!grepl("ZZZ", ProjectName), TRUE, FALSE),
#                   Target_Population = factor(dplyr::case_match(TargetPopulation,
#                                                                1 ~ "DV",
#                                                                3 ~ "HIV",
#                                                                4 ~ "None",
#                                                                .default = NA_character_),
#                                              levels = c("DV", "HIV", "None", NA_character_)),
#                   RRH_Subtype = factor(dplyr::case_when(ProjectType == 13 & RRHSubType == 1 ~ "SSO",
#                                                         ProjectType == 13 & RRHSubType == 2 ~ "Housing",
#                                                         ProjectType == 13 & !RRHSubType %in% c(1, 2) ~ "Unspecified (Missing Data)",
#                                                         .default = NA_character_),
#                                        levels = c("SSO", "Housing", "Unspecified (Missing Data)", NA_character_)),
#                   # proj_TypeLabels = these_project("types", ProjectType, full_label = TRUE, hmis_extract = "ignore"),
#                   # Project_Label = factor(proj_TypeLabels,
#                   #                        levels = pTypeLabels),
#                   Project_Label = factor(these_project("types", ProjectType, full_label = TRUE, hmis_extract = "ignore"),
#                                          levels = pType_FY24$Title)) |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(DateUpdated)) |>
#     dplyr::distinct(ProjectID, .keep_all = TRUE) |>
#     dplyr::select(OrganizationID, ProjectID, ProjectName, ProjectType, Project_Label, RRH_Subtype, Target_Population, PITCount, ActiveProject, ConsolidatedProject) |>
#     dplyr::ungroup()
#   
#   projectcoc <- readr::read_csv(paste0(.extract_path, "ProjectCoC.csv"), show_col_types = FALSE) |>
#     dplyr::group_by(ProjectID) |>
#     dplyr::arrange(-dplyr::desc(ProjectID), dplyr::desc(DateUpdated)) |>
#     dplyr::distinct(ProjectID, .keep_all = TRUE) |>
#     dplyr::select(ProjectID, CoCCode) |>
#     dplyr::ungroup()
#   
#   project <- project |>
#     dplyr::left_join(projectcoc, by = c("ProjectID")) |>
#     dplyr::distinct(ProjectID, .keep_all = TRUE) |>
#     dplyr::left_join(hmisparticipation, by = c("ProjectID")) |>
#     dplyr::left_join(ceparticipation, by = c("ProjectID")) |>
#     dplyr::relocate(ActiveProject, .after = tidyselect::last_col()) |>
#     dplyr::relocate(ConsolidatedProject, .after = tidyselect::last_col())
#   
#   healthanddv <- readr::read_csv(paste0(.extract_path, "HealthAndDV.csv"), show_col_types = FALSE) |>
#     dplyr::arrange(dplyr::desc(InformationDate)) |>
#     dplyr::distinct(PersonalID, .keep_all = TRUE) |>
#     dplyr::select(PersonalID, DomesticViolenceSurvivor, CurrentlyFleeing)
#   
#   incomebenefits <- readr::read_csv(paste0(.extract_path, "IncomeBenefits.csv"), show_col_types = FALSE) |>
#     dplyr::group_by(EnrollmentID, PersonalID) |>
#     dplyr::filter(DataCollectionStage == max(DataCollectionStage)) |>
#     dplyr::ungroup() |>
#     dplyr::select(EnrollmentID, PersonalID, IncomeFromAnySource, TotalMonthlyIncome, Earned, EarnedAmount, DataCollectionStage)
#   
#   organization <- readr::read_csv(paste0(.extract_path, "Organization.csv"), show_col_types = FALSE) |>
#     dplyr::select(OrganizationID, OrganizationName) |>
#     dplyr::left_join(project, by = "OrganizationID") |>
#     dplyr::select(ProjectID, OrganizationName)
#   
#   funder <- readr::read_csv(paste0(.extract_path, "Funder.csv"), show_col_types = FALSE)
#   
#   if (!.include_disabilities)
#   {
#     output <- list(extractDate = as.Date(.extract_date),
#                    FY = 24,
#                    client = tibble::as_tibble(client),
#                    entry = tibble::as_tibble(entry),
#                    exit = tibble::as_tibble(exit),
#                    funder = tibble::as_tibble(funder),
#                    healthanddv = tibble::as_tibble(healthanddv),
#                    incomebenefits = tibble::as_tibble(incomebenefits),
#                    organization = tibble::as_tibble(organization),
#                    project = tibble::as_tibble(project),
#                    services = tibble::as_tibble(services))
#   }
#   else
#   {
#     disabilities <- readr::read_csv(paste0(.extract_path, "Disabilities.csv"), show_col_types = FALSE) |>
#       dplyr::filter(DataCollectionStage == 1) |>
#       dplyr::count(PersonalID, EnrollmentID, DisabilityType, DisabilityResponse) |>
#       dplyr::mutate(DisabilityType_Label = dplyr::case_match(DisabilityType,
#                                                              5 ~ "Physical",
#                                                              6 ~ "Developmental",
#                                                              7 ~ "Chronic Health",
#                                                              8 ~ "HIV/AIDS",
#                                                              9 ~ "Mental Health",
#                                                              10 ~ "Substance Use")) |>
#       tidyr::pivot_wider(id_cols = c(PersonalID, EnrollmentID), names_from = DisabilityType_Label, values_from = DisabilityResponse) |>
#       dplyr::mutate(dplyr::across(`Mental Health`:`Substance Use`, \(x) tidyr::replace_na(x, 0)),
#                     dplyr::across(`Mental Health`:`Substance Use`, \(x) dplyr::case_match(x,
#                                                                                           8 ~ NA,
#                                                                                           9 ~ NA,
#                                                                                           99 ~ NA,
#                                                                                           .default = x)),
#                     "Substance Use" = dplyr::if_else(`Substance Use` > 0, 1, 0))
#     
#     output <- list(extractDate = as.Date(.extract_date),
#                    FY = 24,
#                    client = tibble::as_tibble(client),
#                    disabilities = tibble::as_tibble(disabilities),
#                    entry = tibble::as_tibble(entry),
#                    exit = tibble::as_tibble(exit),
#                    funder = tibble::as_tibble(funder),
#                    healthanddv = tibble::as_tibble(healthanddv),
#                    incomebenefits = tibble::as_tibble(incomebenefits),
#                    organization = tibble::as_tibble(organization),
#                    project = tibble::as_tibble(project),
#                    services = tibble::as_tibble(services))
#   }
#   
#   if (include_roi_report)
#   {
#     roi <- readr::read_csv(paste0(.extract_path, "ROI_report.csv"), show_col_types = FALSE) |> 
#       dplyr::filter(dplyr::if_any(`ROI Date Added`:`ROI Household Id`, \(x) !is.na(x)))
#     
#   }
#   
#   # if (lubridate::ymd(stringr::str_sub(.extract_path, -9, -2)) != as.Date(.extract_date))
#   if (lubridate::ymd(substr(.extract_path, nchar(.extract_path) -8, nchar(.extract_path) -1)) != as.Date(.extract_date))
#   {
#     # mismatched_date <- lubridate::ymd(stringr::str_sub(.extract_path, -8, -1))
#     
#     mismatched_date <- lubridate::ymd(substr(.extract_path, nchar(.extract_path) -8, nchar(.extract_path) -1))
#     
#     display_date_warning_message <- function()
#     {
#       cli::cli_div()
#       cli::cli_text("\n")
#       cli::cli_warn(c("!" = "{.strong The date supplied for {.arg .extract_date} does not match the date in the file path supplied for {.arg .extract_path}.}",
#                       "i" = "You entered {.var {.extract_date}} for {.arg .extract_date}, but the date in the file path for {.arg .extract_path} is {.var {mismatched_date}}",
#                       " " = "\n"))
#       cli::cli_end()
#     }
#     
#     options(warn = 0)
#     
#     display_date_warning_message()
#   }
#   
#   return(output)
# }
