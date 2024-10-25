#' Loads an HMIS CSV/XML Export from .rda file format into the Environment.
#'
#' Returns the list of tibbles saved in the selected hmisData.rda file.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param which_extract the file path to the hmisData.rda file you want to load,
#'   the date (`"YYYY-MM-DD"`) of the extract you want to load, -or- type
#'   `"newest"` to load the most recent HMIS extract.
#' @param .backward_compatability OPTIONAL: if loading an extract from before
#'   03-30-2023, switch this to `TRUE` to avoid an error due to previous
#'   standard practice not involving loading the Funder.csv file as part of the
#'   extract data. Defaults to `FALSE`.
#' @param .use_full_extract OPTIONAL: if loading the full extract (the read-in
#'   .csv files with no preliminary data cleanup), switch this to `TRUE`.
#'   Defaults to `FALSE`.)
#' @param .FY OPTIONAL: the fiscal year of the HMIS data standards for the
#'   extract you want to load. This deafults to the current standards (`24`).
#'   You can specify a different fiscal year with `.FY = `XX.
#'
#' @return returns a list of 9 tibbles: client, services, entry, exit, project,
#'   healthanddv, incomebenefits, organization, and disabilities.
#' @export
load_hmis <- function(which_extract, ..., .backward_compatability = FALSE, .use_full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("encryptr", quietly = TRUE)
  requireNamespace("here", quietly = TRUE)
  
  if (which_extract == "newest")
  {
    hmis_rda <- ifelse(.use_full_extract,
                       paste0(shortcut("extracts"),
                              "/",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 22, nchar(shortcut("newest extract")) - 14),
                              "FULL_",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 13, nchar(shortcut("newest extract"))),
                              ".encryptr.bin"),
                       paste0(shortcut("newest extract"), ".encryptr.bin"))
    
    extract_date <- as.Date(substr(hmis_rda, nchar(hmis_rda) - 26, nchar(hmis_rda) - 17))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  else if (!file.exists(which_extract))
  {
    extract_date <- as.Date(which_extract)
    
    hmis_rda <- ifelse(.use_full_extract,
                       shortcut("extracts", paste0("hmisData_FULL_", extract_date, ".rda.encryptr.bin")),
                       shortcut("extracts", paste0("hmisData_", extract_date, ".rda.encryptr.bin")))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  else
  {
    hmis_rda <- which_extract
    
    extract_date <- as.Date(substr(hmis_rda, nchar(hmis_rda) - 26, nchar(hmis_rda) - 17))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  
  # applyHmisClass <- function(x)
  # {
  #   requireNamespace("lubridate", quietly = TRUE)
  #   
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
  
  user_key_folder <- paste0(paste(strsplit(here::here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  ghost_folder <- tempdir()
  
  filename_no_path <- strsplit(hmis_rda, "/")[[1]][length(strsplit(hmis_rda, "/")[[1]])]
  
  temp_extract_file <- file.path(ghost_folder, substr(filename_no_path, 1, nchar(filename_no_path) - 13))
  
  invisible(capture.output(encryptr::decrypt_file(hmis_rda,
                                                  temp_extract_file,
                                                  private_key_path = paste0(user_key_folder, "id_rsa_f"))))
  
  rm(user_key_folder)
  
  load(temp_extract_file)
  
  extract_file_names <- unlist(load(temp_extract_file))
  
  file.remove(temp_extract_file)
  
  if (!.use_full_extract)
  {
    if (.FY >= 24)
    {
      if (extract_date >= as.Date("2024-05-30")) # Post 6/17/24 there will be an export interval instead of an export date
      {
        hmisExtract <- list(exportInterval = exportInterval,
                            FY = FY,
                            client = client,
                            disabilities = disabilities,
                            entry = entry,
                            exit = exit,
                            funder = funder,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            project = project,
                            services = services)
      }
      else # Pre 6/17/24 there was an export date instead of an export interval
      {
        hmisExtract <- list(extractDate = extractDate,
                            FY = FY,
                            client = client,
                            disabilities = disabilities,
                            entry = entry,
                            exit = exit,
                            funder = funder,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            project = project,
                            services = services)
      }
      
      hmisExtract <- lapply(hmisExtract, applyHmisClass)
      
      class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
      
      return(hmisExtract)
    }
    else # FY22
    {
      if (!.backward_compatability) # Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            # exportStart = exportStart,
                            # exportEnd = exportEnd,
                            client = client,
                            entry = entry,
                            exit = exit,
                            funder = funder,
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
      else # DON'T Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            # exportStart = exportStart,
                            # exportEnd = exportEnd,
                            client = client,
                            entry = entry,
                            exit = exit,
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
    }
  }
  else
  {
    if (extract_date >= as.Date("2024-05-30")) # Post 6/17/24 there will be an export interval instead of an export date
    {
      full_hmis_extract = list(ExportInterval = ExportInterval,
                               FiscalYear = FiscalYear,
                               Affiliation.csv = Affiliation.csv,
                               Assessment.csv = Assessment.csv,
                               AssessmentQuestions.csv = AssessmentQuestions.csv,
                               AssessmentResults.csv = AssessmentResults.csv,
                               CEParticipation.csv = CEParticipation.csv,
                               Client.csv = Client.csv,
                               CurrentLivingSituation.csv = CurrentLivingSituation.csv,
                               Disabilities.csv = Disabilities.csv,
                               EmploymentEducation.csv = EmploymentEducation.csv,
                               Enrollment.csv = Enrollment.csv,
                               Event.csv = Event.csv,
                               Exit.csv = Exit.csv,
                               Export.csv = Export.csv,
                               Funder.csv = Funder.csv,
                               HealthAndDV.csv = HealthAndDV.csv,
                               HMISParticipation.csv = HMISParticipation.csv,
                               IncomeBenefits.csv = IncomeBenefits.csv,
                               Inventory.csv = Inventory.csv,
                               Organization.csv = Organization.csv,
                               Project.csv = Project.csv,
                               ProjectCoC.csv = ProjectCoC.csv,
                               Services.csv = Services.csv,
                               User.csv = User.csv,
                               YouthEducationStatus.csv = YouthEducationStatus.csv)
    }
    else # Pre 6/17/24 there was an export date instead of an export interval
    {
      full_hmis_extract = list(ExportDate = ExportDate,
                               FiscalYear = FiscalYear,
                               Affiliation.csv = Affiliation.csv,
                               Assessment.csv = Assessment.csv,
                               AssessmentQuestions.csv = AssessmentQuestions.csv,
                               AssessmentResults.csv = AssessmentResults.csv,
                               CEParticipation.csv = CEParticipation.csv,
                               Client.csv = Client.csv,
                               CurrentLivingSituation.csv = CurrentLivingSituation.csv,
                               Disabilities.csv = Disabilities.csv,
                               EmploymentEducation.csv = EmploymentEducation.csv,
                               Enrollment.csv = Enrollment.csv,
                               Event.csv = Event.csv,
                               Exit.csv = Exit.csv,
                               Export.csv = Export.csv,
                               Funder.csv = Funder.csv,
                               HealthAndDV.csv = HealthAndDV.csv,
                               HMISParticipation.csv = HMISParticipation.csv,
                               IncomeBenefits.csv = IncomeBenefits.csv,
                               Inventory.csv = Inventory.csv,
                               Organization.csv = Organization.csv,
                               Project.csv = Project.csv,
                               ProjectCoC.csv = ProjectCoC.csv,
                               Services.csv = Services.csv,
                               User.csv = User.csv,
                               YouthEducationStatus.csv = YouthEducationStatus.csv)
    }
    
    full_hmis_extract <- lapply(full_hmis_extract, applyHmisClass)
    
    class(full_hmis_extract) <- c("HMIS Extract", class(full_hmis_extract))
    
    return(full_hmis_extract)
  }
}

#' @export
# Was load_hmis0()
upload_hmis <- function(which_extract, ..., .backward_compatability = FALSE, .use_full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  if(which_extract == "newest")
  {
    filename_no_path <- strsplit(shortcut("newest extract"), "/")[[1]][length(strsplit(shortcut("newest extract"), "/")[[1]])]
    
    hmis_rda <- ifelse(.use_full_extract,
                       paste0(shortcut("downloads"),
                              "/",
                              substr(filename_no_path, 1, 9),
                              "FULL_",
                              substr(filename_no_path, 10, nchar(filename_no_path))),
                       shortcut("downloads", filename_no_path))
    
    extract_date <- as.Date(substr(hmis_rda, nchar(hmis_rda) - 13, nchar(hmis_rda) - 3))
    
    # hmisData <- load(shortcut("downloads", filename_no_path))
  }
  else if (!file.exists(shortcut("downloads",which_extract)))
  {
    extract_date <- as.Date(which_extract)
    
    hmis_rda <- ifelse(.use_full_extract,
                       shortcut("downloads", paste0("hmisData_FULL_", extract_date, ".rda")),
                       shortcut("downloads", paste0("hmisData_", extract_date, ".rda")))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  else
  {
    hmis_rda <- which_extract
    
    extract_date <- as.Date(substr(hmis_rda, nchar(hmis_rda) - 13, nchar(hmis_rda) - 3))
  }
  
  load(hmis_rda)
  
  # applyHmisClass <- function(x)
  # {
  #   if ("Date" %in% class(x))
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
  
  if (.use_full_extract)
  {
    if (.FY >= 24)
    {
      if (extract_date >= as.Date("2024-05-30")) # Post 6/17/24 there will be an export interval instead of an export date
      {
        full_hmis_extract = list(ExportInterval = ExportInterval,
                                 FiscalYear = FiscalYear,
                                 Affiliation.csv = Affiliation.csv,
                                 Assessment.csv = Assessment.csv,
                                 AssessmentQuestions.csv = AssessmentQuestions.csv,
                                 AssessmentResults.csv = AssessmentResults.csv,
                                 CEParticipation.csv = CEParticipation.csv,
                                 Client.csv = Client.csv,
                                 CurrentLivingSituation.csv = CurrentLivingSituation.csv,
                                 Disabilities.csv = Disabilities.csv,
                                 EmploymentEducation.csv = EmploymentEducation.csv,
                                 Enrollment.csv = Enrollment.csv,
                                 Event.csv = Event.csv,
                                 Exit.csv = Exit.csv,
                                 Export.csv = Export.csv,
                                 Funder.csv = Funder.csv,
                                 HealthAndDV.csv = HealthAndDV.csv,
                                 HMISParticipation.csv = HMISParticipation.csv,
                                 IncomeBenefits.csv = IncomeBenefits.csv,
                                 Inventory.csv = Inventory.csv,
                                 Organization.csv = Organization.csv,
                                 Project.csv = Project.csv,
                                 ProjectCoC.csv = ProjectCoC.csv,
                                 Services.csv = Services.csv,
                                 User.csv = User.csv,
                                 YouthEducationStatus.csv = YouthEducationStatus.csv)
      }
      
      else # Pre 6/17/24 there was an export date instead of an export interval
      {
        full_hmis_extract = list(ExportDate = ExportDate,
                                 FiscalYear = FiscalYear,
                                 Affiliation.csv = Affiliation.csv,
                                 Assessment.csv = Assessment.csv,
                                 AssessmentQuestions.csv = AssessmentQuestions.csv,
                                 AssessmentResults.csv = AssessmentResults.csv,
                                 CEParticipation.csv = CEParticipation.csv,
                                 Client.csv = Client.csv,
                                 CurrentLivingSituation.csv = CurrentLivingSituation.csv,
                                 Disabilities.csv = Disabilities.csv,
                                 EmploymentEducation.csv = EmploymentEducation.csv,
                                 Enrollment.csv = Enrollment.csv,
                                 Event.csv = Event.csv,
                                 Exit.csv = Exit.csv,
                                 Export.csv = Export.csv,
                                 Funder.csv = Funder.csv,
                                 HealthAndDV.csv = HealthAndDV.csv,
                                 HMISParticipation.csv = HMISParticipation.csv,
                                 IncomeBenefits.csv = IncomeBenefits.csv,
                                 Inventory.csv = Inventory.csv,
                                 Organization.csv = Organization.csv,
                                 Project.csv = Project.csv,
                                 ProjectCoC.csv = ProjectCoC.csv,
                                 Services.csv = Services.csv,
                                 User.csv = User.csv,
                                 YouthEducationStatus.csv = YouthEducationStatus.csv)
      }
      
      full_hmis_extract <- lapply(full_hmis_extract, applyHmisClass)
      
      class(full_hmis_extract) <- c("HMIS Extract", class(full_hmis_extract))
      
      return(full_hmis_extract)
    }
    else
    {
      fullHmisExtract <- list(extractDate = extractDate,
                              # exportStart = exportStart,
                              # exportEnd = exportEnd,
                              client = client,
                              current_living_situation = current_living_situation,
                              disabilities = disabilities,
                              enrollment = enrollment,
                              enrollment_CoC = enrollment_CoC,
                              exit = exit,
                              funder = funder,
                              health_and_dv = health_and_dv,
                              income_benefits = income_benefits,
                              inventory = inventory,
                              organization = organization,
                              project = project,
                              project_CoC = project_CoC,
                              services = services)
      
      fullHmisExtract <- lapply(fullHmisExtract, applyHmisClass)
      
      class(fullHmisExtract) <- c("HMIS Extract", class(fullHmisExtract))
      
      return(fullHmisExtract)
    }
  }
  else
  {
    if (.FY >= 24)
    {
      if (extract_date >= as.Date("2024-05-30")) # Post 6/17/24 there will be an export interval instead of an export date
      {
        hmisExtract <- list(exportInterval = exportInterval,
                            FY = FY,
                            client = client,
                            disabilities = disabilities,
                            entry = entry,
                            exit = exit,
                            funder = funder, # Added 11-17-23 ----
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            project = project,
                            services = services)
      }
      else # Pre 6/17/24 there was an export date instead of an export interval
      {
        hmisExtract <- list(extractDate = extractDate,
                            FY = FY,
                            client = client,
                            disabilities = disabilities,
                            entry = entry,
                            exit = exit,
                            funder = funder, # Added 11-17-23 ----
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            project = project,
                            services = services)
      }
      
      hmisExtract <- lapply(hmisExtract, applyHmisClass)
      
      class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
      
      return(hmisExtract)
    }
    else
    {
      if (!.backward_compatability) # Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            # exportStart = exportStart,
                            # exportEnd = exportEnd,
                            client = client,
                            entry = entry,
                            exit = exit,
                            funder = funder, # Added 11-17-23 ----
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
      else # DON'T Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            # exportStart = exportStart,
                            # exportEnd = exportEnd,
                            client = client,
                            entry = entry,
                            exit = exit,
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
    }
  }
}

#' @export
download_hmis <- function(which_extract, ..., .backward_compatability = FALSE, .use_full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("encryptr", quietly = TRUE)
  requireNamespace("here", quietly = TRUE)
  
  if (which_extract == "newest")
  {
    hmis_rda <- ifelse(.use_full_extract,
                       paste0(shortcut("extracts"),
                              "/",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 22, nchar(shortcut("newest extract")) - 14),
                              "FULL_",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 13, nchar(shortcut("newest extract"))),
                              ".encryptr.bin"),
                       paste0(shortcut("newest extract"), ".encryptr.bin"))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  else if (!file.exists(which_extract))
  {
    extract_date <- as.Date(which_extract)
    
    hmis_rda <- ifelse(.use_full_extract,
                       shortcut("extracts", paste0("hmisData_FULL_", extract_date, ".rda.encryptr.bin")),
                       shortcut("extracts", paste0("hmisData_", extract_date, ".rda.encryptr.bin")))
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  else
  {
    hmis_rda <- which_extract
    
    # message(paste0("Using: \"", hmis_rda, "\" ", "as the HMIS Export to load."))
  }
  
  # applyHmisClass <- function(x)
  # {
  #   if ("Date" %in% class(x))
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
  
  user_key_folder <- paste0(paste(strsplit(here::here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  downloads_folder <- shortcut("downloads")
  
  filename_no_path <- strsplit(hmis_rda, "/")[[1]][length(strsplit(hmis_rda, "/")[[1]])]
  
  downloaded_extract_file <- file.path(downloads_folder, substr(filename_no_path, 1, nchar(filename_no_path) - 13))
  
  invisible(capture.output(encryptr::decrypt_file(hmis_rda,
                                                  downloaded_extract_file,
                                                  private_key_path = paste0(user_key_folder, "id_rsa_f"))))
  
  rm(user_key_folder)
  
  return(cli::cli_inform("The selected HMIS extract has been downloaded to your {.path {shortcut('downloads')}} folder."))
}
