#' Imports a CE BNL into R from its downloaded Excel file format.
#'
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param downloaded_bnl The `.xls `or `.xlsx` BNL file -or- the folder with the
#'   individual `.csv` files of the BNL.
#' @param list_date The date of the BNL.
#' @param autoclean Automatically filters down the full BNL to include only
#'   certain columns and cleans data if set to [TRUE] (this is the default). If
#'   set to `FALSE`, the BNL will be imported verbatim from Excel into R.
#' @param as.dedup Automatically deduplicates the BNL if set to `TRUE`. Defaults
#'   to `FALSE`. This keeps one instance of each PersonalID based on the most
#'   recent CADate ("API Assessment Date," if available and "ID Date" if not).
#'
#' @return Returns a list of the CE BNL data frames in tibble format with the
#'   BNL Date.
#' @export
import_bnl <- function(downloaded_bnl,
                       list_date,
                       ...,
                       autoclean = TRUE,
                       as.dedup = FALSE)
{
  requireNamespace("dplyr",   quietly = TRUE)
  requireNamespace("here",    quietly = TRUE)
  # requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tibble",  quietly = TRUE)
  
  rlang::check_dots_empty()
  
  bnlDate <- as.Date(list_date)
  
  testSPID <- 138537
  
  applyBnlClass <- function(x)
  {
    if ("Date" %in% class(x))
    {
      return(x)
    }
    else
    {
      class(x) <- c("BNL Data File", class(x))
      
      return(x)
    }
  }
  
  if(!autoclean)
  {
    # if(stringr::str_detect(downloaded_bnl, ".xls") & !stringr::str_detect(downloaded_bnl, ".xlsx"))
    if (grepl(".xls", downloaded_bnl) & !grepl(".xlsx", downloaded_bnl))
    {
      requireNamespace("readxl", quietly = TRUE)
      
      pshPriority <- readxl::read_xls(downloaded_bnl, sheet = 1) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readxl::read_xls(downloaded_bnl, sheet = 2) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readxl::read_xls(downloaded_bnl, sheet = 3) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readxl::read_xls(downloaded_bnl, sheet = 4) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readxl::read_xls(downloaded_bnl, sheet = 5) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    else if (grepl(".xlsx", downloaded_bnl)) # else if(stringr::str_detect(downloaded_bnl, ".xlsx"))
    {
      requireNamespace("readxl", quietly = TRUE)
      
      pshPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 1) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 2) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 3) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 4) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readxl::read_xlsx(downloaded_bnl, sheet = 5) |>
        dplyr::filter(`Client Uid` != testSPID)
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
    
    if (as.dedup)
    {
      pshPriority <- pshPriority |>
        dplyr::group_by(`Client Uid`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) |>
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      pshStaffing <- pshStaffing |>
        dplyr::group_by(`Client Uid`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) |>
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      rrhPriority <- rrhPriority |>
        dplyr::group_by(`Client Uid`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) |>
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      rrhStaffing <- rrhStaffing |>
        dplyr::group_by(`Client Uid`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) |>
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      ceConsolidated <- ceConsolidated |>
        dplyr::group_by(`Client Uid`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) |>
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) |>
        dplyr::ungroup()
    }
    
    bnl <- list(bnlDate = bnlDate,
                pshPriority = pshPriority,
                pshStaffing = pshStaffing,
                rrhPriority = rrhPriority,
                rrhStaffing = rrhStaffing,
                ceConsolidated = ceConsolidated)
    
    # bnl <- lapply(bnl, \(x) applyBnlClass(x))
    bnl <- lapply(bnl, applyBnlClass)
    
    class(bnl) <- c("CE BNL", class(bnl))
    
    return(bnl)
  }
  
  # If autoclean is TRUE ----
  else
  {
    # if(stringr::str_detect(downloaded_bnl, ".xls") & !stringr::str_detect(downloaded_bnl, ".xlsx"))
    if (grepl(".xls", downloaded_bnl) & !grepl(".xlsx", downloaded_bnl))
    {
      requireNamespace("readxl", quietly = TRUE)
      
      pshPriority <- readxl::read_xls(downloaded_bnl, sheet = 1) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readxl::read_xls(downloaded_bnl, sheet = 2) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readxl::read_xls(downloaded_bnl, sheet = 3) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readxl::read_xls(downloaded_bnl, sheet = 4) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readxl::read_xls(downloaded_bnl, sheet = 5) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    else if (grepl(".xlsx", downloaded_bnl)) # else if(stringr::str_detect(downloaded_bnl, ".xlsx"))
    {
      requireNamespace("readxl", quietly = TRUE)
      
      pshPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 1) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readxl::read_xlsx(downloaded_bnl, sheet = 2) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 3) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readxl::read_xlsx(downloaded_bnl, sheet = 4) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readxl::read_xlsx(downloaded_bnl, sheet = 5) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    else
    {
      requireNamespace("readr", quietly = TRUE)
      
      pshPriority <- readxl::read_csv(paste0(downloaded_bnl, "/PSH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhPriority <- readxl::read_csv(paste0(downloaded_bnl, "/RRH Prioritization List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      pshStaffing <- readxl::read_csv(paste0(downloaded_bnl, "/PSH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      rrhStaffing <- readxl::read_csv(paste0(downloaded_bnl, "/RRH Staffing List.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
      
      ceConsolidated <- readxl::read_csv(paste0(downloaded_bnl, "/Consolidated.csv")) |>
        dplyr::filter(`Client Uid` != testSPID)
    }
    
    pshPriority <- pshPriority |>
      dplyr::mutate(# AssessmentDate = as.Date(`API Assessment Date`),
        CADate = dplyr::case_match(`API Assessment Date`,
                                   NA ~ `ID Date`,
                                   .default = `API Assessment Date`), # NEW ----
        # DateReferred = as.Date(`Referral Date`),
        `RRH or PSH` = dplyr::case_match(`RRH or PSH`,
                                         "PSH Only" ~ "PSH",
                                         "RRH Only" ~ "RRH",
                                         "On Both"  ~ "Both"),
        DateReferred = `Referral Date`,
        # DateEnrolledInRRH = as.Date(`RRH Enrollment Date`),
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
      dplyr::mutate(# AssessmentDate = as.Date(`API Assessment Date`),
        CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`), # NEW ----
        # DateReferred = as.Date(`Referral Date`),
        `RRH or PSH` = dplyr::case_match(`RRH or PSH`,
                                         "PSH Only" ~ "PSH",
                                         "RRH Only" ~ "RRH",
                                         "On Both"  ~ "Both"),
        DateReferred = `Referral Date`,
        # DateEnrolledInRRH = as.Date(`RRH Enrollment Date`),
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
      dplyr::mutate(# AssessmentDate = as.Date(`API Assessment Date`),
        CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`), # NEW ----
        # DateReferred = as.Date(`Referral Date`),
        `RRH or PSH` = dplyr::case_match(`RRH or PSH`,
                                         "PSH Only" ~ "PSH",
                                         "RRH Only" ~ "RRH",
                                         "On Both"  ~ "Both"),
        DateReferred = `Referral Date`,
        # DateEnrolledInRRH = as.Date(`RRH Enrollment Date`),
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
      dplyr::mutate(# AssessmentDate = as.Date(`API Assessment Date`),
        CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`), # NEW ----
        # DateReferred = as.Date(`Referral Date`),
        `RRH or PSH` = dplyr::case_match(`RRH or PSH`,
                                         "PSH Only" ~ "PSH",
                                         "RRH Only" ~ "RRH",
                                         "On Both"  ~ "Both"),
        DateReferred = `Referral Date`,
        # DateEnrolledInRRH = as.Date(`RRH Enrollment Date`, origin = "1899-12-30"),
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
      dplyr::mutate(# AssessmentDate = as.Date(`API Assessment Date`),
        CADate = dplyr::case_match(`API Assessment Date`, NA ~ `ID Date`, .default = `API Assessment Date`), # NEW ----
        # DateReferred = as.Date(`Referral Date`),
        `RRH or PSH` = dplyr::case_match(`RRH or PSH`,
                                         "PSH Only" ~ "PSH",
                                         "RRH Only" ~ "RRH",
                                         "On Both"  ~ "Both"),
        DateReferred = `Referral Date`,
        #DateEnrolledInRRH = as.Date(`RRH Enrollment Date`),
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
    
    if (as.dedup)
    {
      pshPriority <- pshPriority |>
        dplyr::group_by(`PersonalID`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(`CADate`)) |>
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      pshStaffing <- pshStaffing |>
        dplyr::group_by(`PersonalID`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(`CADate`)) |>
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      rrhPriority <- rrhPriority |>
        dplyr::group_by(`PersonalID`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(`CADate`)) |>
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      rrhStaffing <- rrhStaffing |>
        dplyr::group_by(`PersonalID`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(`CADate`)) |>
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) |>
        dplyr::ungroup()
      
      ceConsolidated <- ceConsolidated |>
        dplyr::group_by(`PersonalID`) |>
        dplyr::arrange(`Client Uid`,
                       dplyr::desc(`CADate`)) |>
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) |>
        dplyr::ungroup()
    }
    
    bnl <- list(bnlDate = bnlDate,
                pshPriority = pshPriority,
                pshStaffing = pshStaffing,
                rrhPriority = rrhPriority,
                rrhStaffing = rrhStaffing,
                ceConsolidated = ceConsolidated)
    
    # bnl <- lapply(bnl, \(x) applyBnlClass(x))
    bnl <- lapply(bnl, applyBnlClass)
    
    class(bnl) <- c("CE BNL", class(bnl))
    
    return(bnl)
  }
}
