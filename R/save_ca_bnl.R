#' Saves a CE BNL in .rda file format.
#'
#' NOTE: This requires you to have a RStudio Project open and use the [here]
#' package!
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param downloaded_bnl The `.xls` or `.xlsx` BNL file -or- the folder with the
#'   individual `.csv` files of the BNL.
#' @param list_date The date of the BNL.
#' @param directory_to_save Which folder to save this in. Defaults to the
#'   "CE_BNLs" folder in the "Datasets" folder in the R&E Dropbox.
#' @param autoclean Automatically filters down the full BNL to include only
#'   certain columns.
#'
#' @return Saves the CE BNL in `.rda` format.
#' @export
save_ca_bnl <- function(downloaded_bnl,
                        list_date,
                        directory_to_save = NULL,
                        ...,
                        autoclean = FALSE)
{
  requireNamespace("dplyr",   quietly = TRUE)
  requireNamespace("here",    quietly = TRUE)
  requireNamespace("tibble",  quietly = TRUE)
  
  rlang::check_dots_empty()
  
  bnlDate <- as.Date(list_date)
  
  testSPID <- 138537
  
  if (is.null(directory_to_save))
  {
    directory_to_save = paste0("C:/Users/",
                               strsplit(here::here(), "/")[[1]][3],
                               "/ECHO Dropbox/",
                               strsplit(here::here(), "/")[[1]][3],
                               "/Research-and-Evaluation-Team/Datasets/CE_BNLs")
  }
  
  if (grepl(".xls|.xlsx", downloaded_bnl))
  {
    requireNamespace("readxl", quietly = TRUE)
    requireNamespace("tidyselect", quietly = TRUE)
    
    for (i in 1:5)
    {
      # excel_sheet <- readxl::read_excel(shortcut("bnl excel", "20240301 CA BNL.xlsx"), n_max = 0, sheet = i)
      excel_sheet <- readxl::read_excel(downloaded_bnl, n_max = 0, sheet = i)
      
      assign(paste0("date_columns_sheet_", i),
             ifelse(grepl("Date|Completion(4653)|IC Walk-In Info", names(excel_sheet)),
                    "date",
                    "guess"))
      
      assign(paste0("names_of_date_columns_", i),
             names(excel_sheet)[grepl("Date|Completion(4653)|IC Walk-In Info", names(excel_sheet))])
      
      rm(i, excel_sheet)
    }
  }
  
  if (!autoclean)
  {
    if(grepl(".xls", downloaded_bnl) & !grepl(".xlsx", downloaded_bnl))
    {
      pshPriority <- readxl::read_xls(downloaded_bnl, sheet = 1, col_types = date_columns_sheet_1) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_1), as.Date))
      
      rrhPriority <- readxl::read_xls(downloaded_bnl, sheet = 2, col_types = date_columns_sheet_2) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_2), as.Date))
      
      pshStaffing <- readxl::read_xls(downloaded_bnl, sheet = 3, col_types = date_columns_sheet_3) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_3), as.Date))
      
      rrhStaffing <- readxl::read_xls(downloaded_bnl, sheet = 4, col_types = date_columns_sheet_4) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_4), as.Date))
      
      ceConsolidated <- readxl::read_xls(downloaded_bnl, sheet = 5, col_types = date_columns_sheet_5) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_5), as.Date))
    }
    else if (grepl(".xlsx", downloaded_bnl))
    {
      pshPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 1, col_types = date_columns_sheet_1) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_1), as.Date))
      
      rrhPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 2, col_types = date_columns_sheet_2) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_2), as.Date))
      
      pshStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 3, col_types = date_columns_sheet_3) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_3), as.Date))
      
      rrhStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 4, col_types = date_columns_sheet_4) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_4), as.Date))
      
      ceConsolidated <- readxl::read_xlsx(downloaded_bnl, sheet = 5, col_types = date_columns_sheet_5) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_5), as.Date))
    }
    else
    {
      requireNamespace("readr", quietly = TRUE)
      
      pshPriority <- readr::read_csv(paste0(downloaded_bnl, "/PSH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readr::read_csv(paste0(downloaded_bnl, "/RRH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readr::read_csv(paste0(downloaded_bnl, "/PSH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readr::read_csv(paste0(downloaded_bnl, "/RRH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readr::read_csv(paste0(downloaded_bnl, "/Consolidated.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    
    ghost_folder <- tempdir()
    
    pre_encryption_rda_file <- file.path(ghost_folder, paste0("bnl_FULL_", list_date, ".rda"))
    
    save(bnlDate,
         pshPriority,
         rrhPriority,
         pshStaffing,
         rrhStaffing,
         ceConsolidated,
         compress = "xz",
         # file = here::here(paste0(directory_to_save, "/", "bnl_FULL_", list_date, ".rda"))
         file = pre_encryption_rda_file)
    
    encryptr::encrypt_file(pre_encryption_rda_file,
                           crypt_file_name = here::here(directory_to_save, "/", paste0("bnl_FULL_", list_date, ".rda.encryptr.bin")),
                           public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_f.pub"))
    
    file.remove(pre_encryption_rda_file)
    
    rm(pre_encryption_rda_file)
  }
  else # If autoclean is TRUE ----
  {
    if (grepl(".xls", downloaded_bnl) & !grepl(".xlsx", downloaded_bnl))
    {
      pshPriority <- readxl::read_xls(downloaded_bnl, sheet = 1, col_types = date_columns_sheet_1) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_1), as.Date))
      
      rrhPriority <- readxl::read_xls(downloaded_bnl, sheet = 2, col_types = date_columns_sheet_2) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_2), as.Date))
      
      pshStaffing <- readxl::read_xls(downloaded_bnl, sheet = 3, col_types = date_columns_sheet_3) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_3), as.Date))
      
      rrhStaffing <- readxl::read_xls(downloaded_bnl, sheet = 4, col_types = date_columns_sheet_4) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_4), as.Date))
      
      ceConsolidated <- readxl::read_xls(downloaded_bnl, sheet = 5, col_types = date_columns_sheet_5) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_5), as.Date))
    }
    else if (grepl(".xlsx", downloaded_bnl))
    {
      pshPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 1, col_types = date_columns_sheet_1) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_1), as.Date))
      
      rrhPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 2, col_types = date_columns_sheet_2) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_2), as.Date))
      
      pshStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 3, col_types = date_columns_sheet_3) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_3), as.Date))
      
      rrhStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 4, col_types = date_columns_sheet_4) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_4), as.Date))
      
      ceConsolidated <- readxl::read_xlsx(downloaded_bnl, sheet = 5, col_types = date_columns_sheet_5) |>
        dplyr::filter(`Client Uid` != testSPID) |> 
        dplyr::mutate(dplyr::across(tidyselect::any_of(names_of_date_columns_5), as.Date))
    }
    else
    {
      requireNamespace("readr", quietly = TRUE)
      
      pshPriority <- readr::read_csv(paste0(downloaded_bnl, "/PSH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readr::read_csv(paste0(downloaded_bnl, "/RRH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readr::read_csv(paste0(downloaded_bnl, "/PSH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readr::read_csv(paste0(downloaded_bnl, "/RRH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readr::read_csv(paste0(downloaded_bnl, "/Consolidated.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    
    pshPriority <- pshPriority |>
      dplyr::mutate(CADate = dplyr::case_match(`API Assessment Date`,
                                               NA ~ `ID Date`,
                                               .default = `API Assessment Date`),
                    `RRH or PSH` = factor(dplyr::case_match(`RRH or PSH`,
                                                            "PSH Only" ~ "PSH",
                                                            "RRH Only" ~ "RRH",
                                                            "On Both"  ~ "Both",
                                                            .default = NA),
                                          levels = c("PSH",
                                                     "RRH",
                                                     "Both")),
                    DateReferred = `Referral Date`,
                    DateEnrolledInRRH = `RRH Enrollment Date`,
                    DateHoused = as.Date(`Housing Move-in Date(4157)`),
                    DatePSHPacketCompleted = as.Date(`PSH Packet Completion(4563)`)) |>
      dplyr::select(PersonalID = `Client Uid`,
                    CADate, # = AssessmentDate,
                    # DaysSinceCA = DA,
                    # DaysUnhoused = DH,
                    API = `API Total`,
                    # HOH = HH,
                    BoloDays = `BOLO Days`,
                    EngagementDays = `Engage Days`,
                    # Chronic = CH,
                    # Youth,
                    # Veteran = Vets,
                    HasChildren = Children,
                    PriorityList = `RRH or PSH`,
                    ceReferredProgram = Program,
                    ceReferralDate = DateReferred,
                    DaysSinceCEReferral = `Referral Days`,
                    DateEnrolledInRRHFromCE = DateEnrolledInRRH,
                    # DateHoused,
                    DatePSHPacketCompleted,
                    AssessedBy = `Assessor Agency(5082)`,
                    CALocation = `Assessment Location (where client is physically present at time of assessment)(4313)`)
    
    rrhPriority <- rrhPriority |>
      dplyr::mutate(CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`),
                    `RRH or PSH` = factor(dplyr::case_match(`RRH or PSH`,
                                                            "PSH Only" ~ "PSH",
                                                            "RRH Only" ~ "RRH",
                                                            "On Both"  ~ "Both",
                                                            .default = NA),
                                          levels = c("PSH",
                                                     "RRH",
                                                     "Both")),
                    DateReferred = `Referral Date`,
                    DateEnrolledInRRH = `RRH Enrollment Date`,
                    DateHoused = as.Date(`Housing Move-in Date(4157)`),
                    DatePSHPacketCompleted = as.Date(`PSH Packet Completion(4563)`)) |>
      dplyr::select(PersonalID = `Client Uid`,
                    CADate, # = AssessmentDate,
                    # DaysSinceCA = DA,
                    # DaysUnhoused = DH,
                    API = `API Total`,
                    # HOH = HH,
                    BoloDays = `BOLO Days`,
                    EngagementDays = `Engage Days`,
                    # Chronic = CH,
                    # Youth,
                    # Veteran = Vets,
                    HasChildren = Children,
                    PriorityList = `RRH or PSH`,
                    ceReferredProgram = Program,
                    ceReferralDate = DateReferred,
                    DaysSinceCEReferral = `Referral Days`,
                    DateEnrolledInRRHFromCE = DateEnrolledInRRH,
                    # DateHoused,
                    DatePSHPacketCompleted,
                    AssessedBy = `Assessor Agency(5082)`,
                    CALocation = `Assessment Location (where client is physically present at time of assessment)(4313)`)
    
    pshStaffing <- pshStaffing |>
      dplyr::mutate(CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`),
                    `RRH or PSH` = factor(dplyr::case_match(`RRH or PSH`,
                                                            "PSH Only" ~ "PSH",
                                                            "RRH Only" ~ "RRH",
                                                            "On Both"  ~ "Both",
                                                            .default = NA),
                                          levels = c("PSH",
                                                     "RRH",
                                                     "Both")),
                    DateReferred = `Referral Date`,
                   DateEnrolledInRRH = `RRH Enrollment Date`,
                    DateHoused = as.Date(`Housing Move-in Date(4157)`),
                    DatePSHPacketCompleted = as.Date(`PSH Packet Completion(4563)`)) |>
      dplyr::select(PersonalID = `Client Uid`,
                    CADate, # = AssessmentDate,
                    # DaysSinceCA = DA,
                    # DaysUnhoused = DH,
                    API = `API Total`,
                    # HOH = HH,
                    BoloDays = `BOLO Days`,
                    EngagementDays = `Engage Days`,
                    # Chronic = CH,
                    # Youth,
                    # Veteran = Vets,
                    HasChildren = Children,
                    PriorityList = `RRH or PSH`,
                    ceReferredProgram = Program,
                    ceReferralDate = DateReferred,
                    DaysSinceCEReferral = `Referral Days`,
                    DateEnrolledInRRHFromCE = DateEnrolledInRRH,
                    # DateHoused,
                    DatePSHPacketCompleted,
                    AssessedBy = `Assessor Agency(5082)`,
                    CALocation = `Assessment Location (where client is physically present at time of assessment)(4313)`)
    
    rrhStaffing <- rrhStaffing |>
      dplyr::mutate(CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`),
                    `RRH or PSH` = factor(dplyr::case_match(`RRH or PSH`,
                                                            "PSH Only" ~ "PSH",
                                                            "RRH Only" ~ "RRH",
                                                            "On Both"  ~ "Both",
                                                            .default = NA),
                                          levels = c("PSH",
                                                     "RRH",
                                                     "Both")),
                    DateReferred = `Referral Date`,
                    DateEnrolledInRRH = `RRH Enrollment Date`,
                    DateHoused = as.Date(`Housing Move-in Date(4157)`),
                    DatePSHPacketCompleted = as.Date(`PSH Packet Completion(4563)`)) |>
      dplyr::select(PersonalID = `Client Uid`,
                    CADate, # = AssessmentDate,
                    # DaysSinceCA = DA,
                    # DaysUnhoused = DH,
                    API = `API Total`,
                    # HOH = HH,
                    BoloDays = `BOLO Days`,
                    EngagementDays = `Engage Days`,
                    # Chronic = CH,
                    # Youth,
                    # Veteran = Vets,
                    HasChildren = Children,
                    PriorityList = `RRH or PSH`,
                    ceReferredProgram = Program,
                    ceReferralDate = DateReferred,
                    DaysSinceCEReferral = `Referral Days`,
                    DateEnrolledInRRHFromCE = DateEnrolledInRRH,
                    # DateHoused,
                    DatePSHPacketCompleted,
                    AssessedBy = `Assessor Agency(5082)`,
                    CALocation = `Assessment Location (where client is physically present at time of assessment)(4313)`)
    
    ceConsolidated <- ceConsolidated |>
      dplyr::mutate(CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`),
                    `RRH or PSH` = factor(dplyr::case_match(`RRH or PSH`,
                                                            "PSH Only" ~ "PSH",
                                                            "RRH Only" ~ "RRH",
                                                            "On Both"  ~ "Both",
                                                            .default = NA),
                                          levels = c("PSH",
                                                     "RRH",
                                                     "Both")),
                    DateReferred = `Referral Date`,
                    DateEnrolledInRRH = `RRH Enrollment Date`,
                    DateHoused = as.Date(`Housing Move-in Date(4157)`),
                    DatePSHPacketCompleted = as.Date(`PSH Packet Completion(4563)`)) |>
      dplyr::select(PersonalID = `Client Uid`,
                    CADate, # = AssessmentDate,
                    # DaysSinceCA = DA,
                    # DaysUnhoused = DH,
                    API = `API Total`,
                    # HOH = HH,
                    BoloDays = `BOLO Days`,
                    EngagementDays = `Engage Days`,
                    # Chronic = CH,
                    # Youth,
                    # Veteran = Vets,
                    HasChildren = Children,
                    PriorityList = `RRH or PSH`,
                    ceReferredProgram = Program,
                    ceReferralDate = DateReferred,
                    DaysSinceCEReferral = `Referral Days`,
                    DateEnrolledInRRHFromCE = DateEnrolledInRRH,
                    # DateHoused,
                    DatePSHPacketCompleted,
                    AssessedBy = `Assessor Agency(5082)`,
                    CALocation = `Assessment Location (where client is physically present at time of assessment)(4313)`)
    
    ghost_folder <- tempdir()
    
    pre_encryption_rda_file <- file.path(ghost_folder, paste0("bnl_", list_date, ".rda"))
    
    save(bnlDate,
         pshPriority,
         rrhPriority,
         pshStaffing,
         rrhStaffing,
         ceConsolidated,
         compress = "xz",
         # file = stringr::str_c(directory_to_save, "/bnl_", list_date, ".rda")))
         # file = paste0(directory_to_save, "/bnl_", list_date, ".rda")
         file = pre_encryption_rda_file)
    
    encryptr::encrypt_file(pre_encryption_rda_file,
                           crypt_file_name = here::here(directory_to_save, "/", paste0("bnl_", list_date, ".rda.encryptr.bin")),
                           public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_f.pub"))
    
    file.remove(pre_encryption_rda_file)
    
    rm(pre_encryption_rda_file)
  }
}
