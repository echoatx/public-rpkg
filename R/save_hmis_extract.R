#' Saves an HMIS CSV/XML Export in .rda file format.
#'
#' Uses the [import_hmis] funtion to read in `.csv` files from data exported
#' from HMIS per the specified date and saves each `tibble` to an
#' "hmisData_**extract_date**.rda" file instead of returning a list of
#' `tibble`s. NOTE: This requires you to have a RStudio Project open and use the
#' [here] package!
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param extract_path the path to the folder containing the HMIS CSV/XML Export
#'   .csv files.
#' @param extract_date the date the CSV/XML extract was pulled from HMIS:
#'   YYYY-MM-DD.
#' @param save_in_directory the directory in which you want to save the output
#'   file.
#' @param full_extract Defaults to `FALSE`. If set to `TRUE` it will include all
#'   14 CSV files from the XML extract instead of the usual 9.
#' @param .FY OPTIONAL: the fiscal year of the data standards. Defaults to
#'   current.
#'
#' @return saves a file called "hmisData_**extract_date**.rda" with 9 `tibble`s:
#'   **client**, **services**, **entry**, **exit**,
#'   **project**, **healthanddv**,
#'   **incomebenefits**, **organization**, and **disabilities**.
#' @export
# save_hmis_extract <- function(extract_path, extract_date, save_in_directory, ..., full_extract = FALSE, .FY = 24)
save_hmis_extract <- function(extract_path, save_in_directory, ..., full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("here", quietly = TRUE)
  requireNamespace("encryptr", quietly = TRUE)
  # requireNamespace("readr", quietly = TRUE)
  
  # extractDate <- as.Date(extract_date)
  
  # extractDate <- as.Date(readr::read_csv(paste0(extract_path, "/Export.csv"), show_col_types = FALSE)$ExportEndDate[1])
  FY <- .FY
  
  if (FY == 24)
  {
    if(!full_extract)
    {
      hmisData <- import_hmis(extract_path,
                              # extract_date,
                              include_disabilities = TRUE,
                              runningStandalone = FALSE,
                              .FY = 24)
      
      # extractDate <- hmisData$extractDate
      # exportStart <- hmisData$exportStart
      # exportEnd <- hmisData$exportEnd
      exportInterval <- hmisData$exportInterval
      client <- hmisData$client
      entry <- hmisData$entry
      exit <- hmisData$exit
      services <- hmisData$services
      project <- hmisData$project
      healthanddv <- hmisData$healthanddv
      incomebenefits <- hmisData$incomebenefits
      organization <- hmisData$organization
      disabilities <- hmisData$disabilities
      funder <- hmisData$funder
      
      ghost_folder <- tempdir()
      
      # pre_encryption_rda_file <- file.path(ghost_folder, paste0("hmisData_", extract_date, ".rda"))
      pre_encryption_rda_file <- file.path(ghost_folder, paste0("hmisData_", extractDate, ".rda"))
      
      save(exportInterval,
           # extractDate,
           # exportStart,
           # exportEnd,
           FY,
           client,
           disabilities,
           entry,
           exit,
           funder,
           healthanddv,
           incomebenefits,
           organization,
           project,
           services,
           compress = "xz",
           # file = here::here(save_in_directory, "/", paste0("hmisData_", extract_date, ".rda"))
           file = pre_encryption_rda_file)
      
      # pre_encryption_rda_file <- file.path(ghost_folder, paste0("hmisData_", export_date, ".rda"))
      # pre_encryption_rda_file <- file.path(ghost_folder, paste0("hmisData_", extractDate, ".rda"))
      
      # encryptr::encrypt_file(pre_encryption_rda_file,
      #                        crypt_file_name = here::here(save_in_directory, "/", paste0("hmisData_", extract_date, ".rda.encryptr.bin")),
      #                        public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_f.pub"))
      
      encryptr::encrypt_file(pre_encryption_rda_file,
                             crypt_file_name = here::here(save_in_directory, "/", paste0("hmisData_", extractDate, ".rda.encryptr.bin")),
                             public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_f.pub"))
      
      file.remove(pre_encryption_rda_file)
      
      rm(pre_encryption_rda_file)
    }
    else
    {
      requireNamespace("lubridate", quietly = TRUE)
      
      hmisData <- import_hmis(extract_path,
                              # extract_date,
                              all_files = TRUE,
                              runningStandalone = FALSE,
                              .FY = 24)
      
      ExportInterval <- hmisData$export_interval
      ExportDate <- lubridate::int_end(ExportInterval)
      # ExportDate <- hmisData$export_date
      # exportStart <- hmisData$exportStart
      # exportEnd <- hmisData$exportEnd
      FiscalYear <- .FY
      Affiliation.csv <- hmisData$affiliation
      Assessment.csv <- hmisData$assessment
      AssessmentQuestions.csv <- hmisData$assessment_questions
      AssessmentResults.csv <- hmisData$assessment_results
      CEParticipation.csv <- hmisData$ce_participation
      Client.csv <- hmisData$client
      CurrentLivingSituation.csv <- hmisData$current_living_situation
      Disabilities.csv <- hmisData$disabilities
      EmploymentEducation.csv <- hmisData$employment_education
      Enrollment.csv <- hmisData$enrollment
      Event.csv <- hmisData$event
      Exit.csv <- hmisData$exit
      Export.csv <- hmisData$export
      Funder.csv <- hmisData$funder
      HealthAndDV.csv <- hmisData$health_and_dv
      HMISParticipation.csv <- hmisData$hmis_participation
      IncomeBenefits.csv <- hmisData$income_benefits
      Inventory.csv <- hmisData$inventory
      Organization.csv <- hmisData$organization
      Project.csv <- hmisData$project
      ProjectCoC.csv <- hmisData$project_coc
      Services.csv <- hmisData$services
      User.csv <- hmisData$user
      YouthEducationStatus.csv <- hmisData$youth_education_status
      
      ghost_folder <- tempdir()
      
      pre_encryption_rda_file <- file.path(ghost_folder, paste0("hmisData_FULL_", ExportDate, ".rda"))
      
      save(ExportInterval,
           # ExportDate,
           # exportStart,
           # exportEnd,
           FiscalYear,
           Affiliation.csv,
           Assessment.csv,
           AssessmentQuestions.csv,
           AssessmentResults.csv,
           CEParticipation.csv,
           Client.csv,
           CurrentLivingSituation.csv,
           Disabilities.csv,
           EmploymentEducation.csv,
           Enrollment.csv,
           Event.csv,
           Exit.csv,
           Export.csv,
           Funder.csv,
           HealthAndDV.csv,
           HMISParticipation.csv,
           IncomeBenefits.csv,
           Inventory.csv,
           Organization.csv,
           Project.csv,
           ProjectCoC.csv,
           Services.csv,
           User.csv,
           YouthEducationStatus.csv,
           compress = "xz",
           file = pre_encryption_rda_file)
      
      encryptr::encrypt_file(pre_encryption_rda_file,
                             crypt_file_name = here::here(save_in_directory, "/", paste0("hmisData_FULL_", ExportDate, ".rda.encryptr.bin")),
                             public_key_path = shortcut("re dropbox", "Code", "files", "id_rsa_f.pub"))
      
      file.remove(pre_encryption_rda_file)
      
      rm(pre_encryption_rda_file)
    }
  }
  
  if (FY == 22)
  {
    if(!full_extract)
    {
      hmisData <- import_hmis(extract_path, extract_date, include_disabilities = TRUE, runningStandalone = FALSE, .FY = 22)
      
      client <- hmisData$client
      entry <- hmisData$entry
      exit <- hmisData$exit
      services <- hmisData$services
      project <- hmisData$project
      healthanddv <- hmisData$healthanddv
      incomebenefits <- hmisData$incomebenefits
      organization <- hmisData$organization
      disabilities <- hmisData$disabilities
      funder <- hmisData$funder
      
      return(save(extractDate,
                  FY,
                  client,
                  disabilities,
                  entry,
                  exit,
                  funder,
                  healthanddv,
                  incomebenefits,
                  organization,
                  project,
                  services,
                  compress = "xz",
                  file = here::here(save_in_directory, "/", paste0("hmisData_", extract_date, ".rda"))))
    }
    else
    {
      requireNamespace("readr", quietly = TRUE)
      
      client <- readr::read_csv(paste0(extract_path, "/Client.csv"))
      current_living_situation <- readr::read_csv(paste0(extract_path, "/CurrentLivingSituation.csv"))
      disabilities <- readr::read_csv(paste0(extract_path, "/Disabilities.csv"))
      enrollment <- readr::read_csv(paste0(extract_path, "/Enrollment.csv"))
      enrollment_CoC <- readr::read_csv(paste0(extract_path, "/EnrollmentCoC.csv"))
      exit <- readr::read_csv(paste0(extract_path, "/Exit.csv"))
      funder <- readr::read_csv(paste0(extract_path, "/Funder.csv"))
      health_and_dv <- readr::read_csv(paste0(extract_path, "/HealthAndDV.csv"))
      income_benefits <- readr::read_csv(paste0(extract_path, "/IncomeBenefits.csv"))
      inventory <- readr::read_csv(paste0(extract_path, "/Inventory.csv"))
      organization <- readr::read_csv(paste0(extract_path, "/Organization.csv"))
      project <- readr::read_csv(paste0(extract_path, "/Project.csv"))
      project_CoC <- readr::read_csv(paste0(extract_path, "/ProjectCoC.csv"))
      services <- readr::read_csv(paste0(extract_path, "/Services.csv"))
      
      return(save(extractDate,
                  FY,
                  # extract_date,
                  client,
                  current_living_situation,
                  disabilities,
                  enrollment,
                  enrollment_CoC,
                  exit,
                  funder,
                  health_and_dv,
                  income_benefits,
                  inventory,
                  organization,
                  project,
                  project_CoC,
                  services,
                  compress = "xz",
                  # file = here(save_in_directory, "/", str_c("CSV_Extract_", extract_date, ".rda"))))
                  file = here::here(save_in_directory, "/", paste0("CSV_Extract_", extract_date, ".rda"))))
    }
  }
}

# save_hmis_extract_2023-11-09 <- function(extract_path, extract_date, save_in_directory, ..., full_extract = FALSE, .FY = 24)
# {
#   rlang::check_dots_empty()
#   
#   requireNamespace("here", quietly = TRUE)
#   # requireNamespace("stringr", quietly = TRUE)
#   
#    extractDate <- as.Date(extract_date)
#    FY <- .FY
#    
#    if (FY == 24)
#    {
#      if(!full_extract)
#      {
#        hmisData <- import_hmis_fy24(extract_path, extract_date, include_disabilities = TRUE)
#        
#        client <- hmisData$client
#        entry <- hmisData$entry
#        exit <- hmisData$exit
#        services <- hmisData$services
#        project <- hmisData$project
#        healthanddv <- hmisData$healthanddv
#        incomebenefits <- hmisData$incomebenefits
#        organization <- hmisData$organization
#        disabilities <- hmisData$disabilities
#        funder <- hmisData$funder
#        
#        return(save(extractDate,
#                    FY,
#                    client,
#                    disabilities,
#                    entry,
#                    exit,
#                    funder,
#                    healthanddv,
#                    incomebenefits,
#                    organization,
#                    project,
#                    services,
#                    compress = "xz",
#                    # file = here::here(save_in_directory, "/", stringr::str_c("hmisData_", extract_date, ".rda"))))
#                    file = here::here(save_in_directory, "/", paste0("hmisData_", extract_date, ".rda"))))
#      }
#      else
#      {
#        requireNamespace("readr", quietly = TRUE)
#        
#        # client <- readr::read_csv(stringr::str_c(extract_path, "Client.csv"))
#        # current_living_situation <- readr::read_csv(stringr::str_c(extract_path, "CurrentLivingSituation.csv"))
#        # disabilities <- readr::read_csv(stringr::str_c(extract_path, "Disabilities.csv"))
#        # enrollment <- readr::read_csv(stringr::str_c(extract_path, "Enrollment.csv"))
#        # enrollment_CoC <- readr::read_csv(stringr::str_c(extract_path, "EnrollmentCoC.csv"))
#        # exit <- readr::read_csv(stringr::str_c(extract_path, "Exit.csv"))
#        # funder <- readr::read_csv(stringr::str_c(extract_path, "Funder.csv"))
#        # health_and_dv <- readr::read_csv(stringr::str_c(extract_path, "HealthAndDV.csv"))
#        # income_benefits <- readr::read_csv(stringr::str_c(extract_path, "IncomeBenefits.csv"))
#        # inventory <- readr::read_csv(stringr::str_c(extract_path, "Inventory.csv"))
#        # organization <- readr::read_csv(stringr::str_c(extract_path, "Organization.csv"))
#        # project <- readr::read_csv(stringr::str_c(extract_path, "Project.csv"))
#        # project_CoC <- readr::read_csv(stringr::str_c(extract_path, "ProjectCoC.csv"))
#        # services <- readr::read_csv(stringr::str_c(extract_path, "Services.csv"))
#        
#        client <- readr::read_csv(paste0(extract_path, "/Client.csv"))
#        current_living_situation <- readr::read_csv(paste0(extract_path, "/CurrentLivingSituation.csv"))
#        disabilities <- readr::read_csv(paste0(extract_path, "/Disabilities.csv"))
#        enrollment <- readr::read_csv(paste0(extract_path, "/Enrollment.csv"))
#        enrollment_CoC <- readr::read_csv(paste0(extract_path, "/EnrollmentCoC.csv"))
#        exit <- readr::read_csv(paste0(extract_path, "/Exit.csv"))
#        funder <- readr::read_csv(paste0(extract_path, "/Funder.csv"))
#        health_and_dv <- readr::read_csv(paste0(extract_path, "/HealthAndDV.csv"))
#        income_benefits <- readr::read_csv(paste0(extract_path, "/IncomeBenefits.csv"))
#        inventory <- readr::read_csv(paste0(extract_path, "/Inventory.csv"))
#        organization <- readr::read_csv(paste0(extract_path, "/Organization.csv"))
#        project <- readr::read_csv(paste0(extract_path, "/Project.csv"))
#        project_CoC <- readr::read_csv(paste0(extract_path, "/ProjectCoC.csv"))
#        services <- readr::read_csv(paste0(extract_path, "/Services.csv"))
#        
#        return(save(extractDate,
#                    FY,
#                    client,
#                    current_living_situation,
#                    disabilities,
#                    enrollment,
#                    enrollment_CoC,
#                    exit,
#                    funder,
#                    health_and_dv,
#                    income_benefits,
#                    inventory,
#                    organization,
#                    project,
#                    project_CoC,
#                    services,
#                    compress = "xz",
#                    # file = here(save_in_directory, "/", str_c("CSV_Extract_", extract_date, ".rda"))))
#                    file = here::here(save_in_directory, "/", paste0("CSV_Extract_", extract_date, ".rda"))))
#      }
#    }
#    
#    if (FY == 22)
#    {
#      if(!full_extract)
#      {
#        hmisData <- import_hmis(extract_path, extract_date, include_disabilities = TRUE)
#        
#        client <- hmisData$client
#        entry <- hmisData$entry
#        exit <- hmisData$exit
#        services <- hmisData$services
#        project <- hmisData$project
#        healthanddv <- hmisData$healthanddv
#        incomebenefits <- hmisData$incomebenefits
#        organization <- hmisData$organization
#        disabilities <- hmisData$disabilities
#        funder <- hmisData$funder
#        
#        return(save(extractDate,
#                    FY,
#                    # extract_date,
#                    client,
#                    disabilities,
#                    entry,
#                    exit,
#                    funder,
#                    healthanddv,
#                    incomebenefits,
#                    organization,
#                    project,
#                    services,
#                    compress = "xz",
#                    # file = here::here(save_in_directory, "/", stringr::str_c("hmisData_", extract_date, ".rda"))))
#                    file = here::here(save_in_directory, "/", paste0("hmisData_", extract_date, ".rda"))))
#      }
#      else
#      {
#        requireNamespace("readr", quietly = TRUE)
#        
#        # client <- readr::read_csv(stringr::str_c(extract_path, "Client.csv"))
#        # current_living_situation <- readr::read_csv(stringr::str_c(extract_path, "CurrentLivingSituation.csv"))
#        # disabilities <- readr::read_csv(stringr::str_c(extract_path, "Disabilities.csv"))
#        # enrollment <- readr::read_csv(stringr::str_c(extract_path, "Enrollment.csv"))
#        # enrollment_CoC <- readr::read_csv(stringr::str_c(extract_path, "EnrollmentCoC.csv"))
#        # exit <- readr::read_csv(stringr::str_c(extract_path, "Exit.csv"))
#        # funder <- readr::read_csv(stringr::str_c(extract_path, "Funder.csv"))
#        # health_and_dv <- readr::read_csv(stringr::str_c(extract_path, "HealthAndDV.csv"))
#        # income_benefits <- readr::read_csv(stringr::str_c(extract_path, "IncomeBenefits.csv"))
#        # inventory <- readr::read_csv(stringr::str_c(extract_path, "Inventory.csv"))
#        # organization <- readr::read_csv(stringr::str_c(extract_path, "Organization.csv"))
#        # project <- readr::read_csv(stringr::str_c(extract_path, "Project.csv"))
#        # project_CoC <- readr::read_csv(stringr::str_c(extract_path, "ProjectCoC.csv"))
#        # services <- readr::read_csv(stringr::str_c(extract_path, "Services.csv"))
#        
#        client <- readr::read_csv(paste0(extract_path, "/Client.csv"))
#        current_living_situation <- readr::read_csv(paste0(extract_path, "/CurrentLivingSituation.csv"))
#        disabilities <- readr::read_csv(paste0(extract_path, "/Disabilities.csv"))
#        enrollment <- readr::read_csv(paste0(extract_path, "/Enrollment.csv"))
#        enrollment_CoC <- readr::read_csv(paste0(extract_path, "/EnrollmentCoC.csv"))
#        exit <- readr::read_csv(paste0(extract_path, "/Exit.csv"))
#        funder <- readr::read_csv(paste0(extract_path, "/Funder.csv"))
#        health_and_dv <- readr::read_csv(paste0(extract_path, "/HealthAndDV.csv"))
#        income_benefits <- readr::read_csv(paste0(extract_path, "/IncomeBenefits.csv"))
#        inventory <- readr::read_csv(paste0(extract_path, "/Inventory.csv"))
#        organization <- readr::read_csv(paste0(extract_path, "/Organization.csv"))
#        project <- readr::read_csv(paste0(extract_path, "/Project.csv"))
#        project_CoC <- readr::read_csv(paste0(extract_path, "/ProjectCoC.csv"))
#        services <- readr::read_csv(paste0(extract_path, "/Services.csv"))
#        
#        return(save(extractDate,
#                    FY,
#                    # extract_date,
#                    client,
#                    current_living_situation,
#                    disabilities,
#                    enrollment,
#                    enrollment_CoC,
#                    exit,
#                    funder,
#                    health_and_dv,
#                    income_benefits,
#                    inventory,
#                    organization,
#                    project,
#                    project_CoC,
#                    services,
#                    compress = "xz",
#                    # file = here(save_in_directory, "/", str_c("CSV_Extract_", extract_date, ".rda"))))
#                    file = here::here(save_in_directory, "/", paste0("CSV_Extract_", extract_date, ".rda"))))
#      }
#    }
# }

# save_hmis_extract_original <- function(extract_path, extract_date, save_in_directory)
# {
#     require(here)
#     require(stringr)
#     
#     hmisData <- import_hmis(extract_path, extract_date, include_disabilities = TRUE)
#     
#     client <- hmisData$client
#     entry <- hmisData$entry
#     exit <- hmisData$exit
#     services <- hmisData$services
#     project <- hmisData$project
#     healthanddv <- hmisData$healthanddv
#     incomebenefits <- hmisData$incomebenefits
#     organization <- hmisData$organization
#     disabilities <- hmisData$disabilities
#     
#     return(save(extract_date,
#                 client,
#                 entry,
#                 exit,
#                 services,
#                 project,
#                 healthanddv,
#                 incomebenefits,
#                 organization,
#                 disabilities,
#                 compress = "xz",
#                 file = here(save_in_directory, "/", str_c("hmisData_", extract_date, ".rda"))))
# }
