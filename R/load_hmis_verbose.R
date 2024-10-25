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
load_hmis_verbose <- function(which_extract, ..., .backward_compatability = FALSE, .use_full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("encryptr", quietly = TRUE)
  requireNamespace("here", quietly = TRUE)
  
  if (which_extract == "newest")
  {
    # hmis_rda <- paste0(shortcut("newest extract"), ".encryptr.bin")
    
    hmis_rda <- ifelse(.use_full_extract,
                       paste0(shortcut("extracts"),
                              "/",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 22, nchar(shortcut("newest extract")) - 14),
                              "FULL_",
                              substr(shortcut("newest extract"), nchar(shortcut("newest extract")) - 13, nchar(shortcut("newest extract"))),
                              ".encryptr.bin"),
                       paste0(shortcut("newest extract"), ".encryptr.bin"))
    
    message(paste0("Using: ", hmis_rda, " as the HMIS Export to load."))
  }
  else if (!file.exists(which_extract))
  {
    extract_date <- as.Date(which_extract)
    
    # hmis_rda <- shortcut("extracts",
    #                      paste0(substr("hmisData_.rda.encryptr.bin", 1, 9),
    #                             extract_date,
    #                             substr("hmisData_.rda.encryptr.bin", 10, 26)))
    
    hmis_rda <- ifelse(.use_full_extract,
                       shortcut("extracts", paste0("hmisData_FULL_", extract_date, ".rda.encryptr.bin")),
                       shortcut("extracts", paste0("hmisData_", extract_date, ".rda.encryptr.bin")))
    
    message(paste0("Using: ", hmis_rda, " as the HMIS Export to load."))
  }
  else
  {
    hmis_rda <- which_extract
    
    message(paste0("Using: ", hmis_rda, " as the HMIS Export to load."))
  }
  
  applyHmisClass <- function(x)
  {
    if ("Date" %in% class(x))
    {
      return(x)
    }
    else if ("numeric" %in% class(x) & !"Date" %in% class(x))
    {
      class(x) <- c("HMIS Data Standards Fiscal Year", class(x))
      
      return(x)
    }
    else
    {
      class(x) <- c("HMIS Data File", class(x))
      
      return(x)
    }
  }
  
  user_key_folder <- paste0(paste(strsplit(here::here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  cli::cli_alert_info("{.var user_key_folder} = {.path {user_key_folder}}")
  
  ghost_folder <- tempdir()
  
  cli::cli_alert_info("{.var ghost_folder} = {.path {ghost_folder}}")
  
  filename_no_path <- strsplit(hmis_rda, "/")[[1]][length(strsplit(hmis_rda, "/")[[1]])]
  
  cli::cli_alert_info("{.var filename_no_path} = {.val {filename_no_path}}")
  
  temp_extract_file <- file.path(ghost_folder, substr(filename_no_path, 1, nchar(filename_no_path) - 13))
  
  cli::cli_alert_info("{.var temp_extract_file} = {.path {temp_extract_file}}")
  
  cli::cli_alert_info("Running decrypt_file({.val {hmis_rda}}, {.val {temp_extract_file}})")
  invisible(capture.output(encryptr::decrypt_file(hmis_rda,
                                                  temp_extract_file,
                                                  private_key_path = paste0(user_key_folder, "id_rsa_f"))))
  
  rm(user_key_folder)
  
  cli::cli_alert_info("Loading {.val {temp_extract_file}}.")
  
  load(temp_extract_file)
  
  # extract_file_names <- unlist(load(temp_extract_file))
  
  file.remove(temp_extract_file)
  
  if (!.use_full_extract)
  {
    if (.FY >= 24)
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
      
      # hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
      hmisExtract <- lapply(hmisExtract, applyHmisClass)
      
      class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
      
      return(hmisExtract)
    }
    else
    {
      if (!.backward_compatability) # Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
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
        
        # hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
      else # DON'T Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            client = client,
                            entry = entry,
                            exit = exit,
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        # hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
        hmisExtract <- lapply(hmisExtract, applyHmisClass)
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
    }
  }
  else
  {
    # full_hmis_extract <- list(export_date = as.Date(export$ExportEndDate[1]),
    #                           affiliation = affiliation,
    #                           assessment = assessment,
    #                           assessment_questions = assessment_questions,
    #                           assessment_results = assessment_results,
    #                           ce_participation = ce_participation,
    #                           client = client,
    #                           current_living_situation = current_living_situation,
    #                           disabilities = disabilities,
    #                           employment_education = employment_education,
    #                           enrollment = enrollment,
    #                           event = event,
    #                           exit = exit,
    #                           export = export,
    #                           funder = funder,
    #                           health_and_dv = health_and_dv,
    #                           hmis_participation = hmis_participation,
    #                           income_benefits = income_benefits,
    #                           inventory = inventory,
    #                           organization = organization,
    #                           project = project,
    #                           project_coc = project_coc,
    #                           services = services,
    #                           user = user,
    #                           youth_education_status = youth_education_status)
    
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
    
    # if ("roi" %in% extract_file_names)
    # {
    #   full_hmis_extract <- full_hmis_extract |> append(list(roi), after = 22)
    #   
    #   names(full_hmis_extract)[23] <- "roi"
    # }
    
    full_hmis_extract <- lapply(full_hmis_extract, applyHmisClass)
    
    class(full_hmis_extract) <- c("HMIS Extract", class(full_hmis_extract))
    
    return(full_hmis_extract)
  }
  # else
  # {
  #   if (.FY >= 24)
  #   {
  #     fullHmisExtract <- list(extractDate = export_date,
  #                             FY = .FY,
  #                             client = client,
  #                             current_living_situation = current_living_situation,
  #                             disabilities = disabilities,
  #                             enrollment = enrollment,
  #                             enrollment_CoC = enrollment_CoC,
  #                             exit = exit,
  #                             funder = funder,
  #                             health_and_dv = health_and_dv,
  #                             income_benefits = income_benefits,
  #                             inventory = inventory,
  #                             organization = organization,
  #                             project = project,
  #                             project_CoC = project_CoC,
  #                             services = services)
  #     
  #     # fullHmisExtract <- lapply(fullHmisExtract, \(x) applyHmisClass(x))
  #     fullHmisExtract <- lapply(fullHmisExtract, applyHmisClass)
  #     
  #     class(fullHmisExtract) <- c("HMIS Extract", class(fullHmisExtract))
  #     
  #     return(fullHmisExtract)
  #   }
  #   else
  #   {
  #     fullHmisExtract <- list(extractDate = extractDate,
  #                             affiliation = affiliation,
  #                             assessment = assessment,
  #                             assessment_questions = assessment_questions,
  #                             assessment_results = assessment_results,
  #                             ce_participation = ce_participation,
  #                             client = client,
  #                             current_living_situation = current_living_situation,
  #                             disabilities = disabilities,
  #                             employment_education = employment_education,
  #                             entry = enrollment,
  #                             event = event,
  #                             exit = exit,
  #                             export = export,
  #                             funder = funder,
  #                             health_and_dv = health_and_dv,
  #                             hmis_participation = hmis_participation,
  #                             income_benefits = income_benefits,
  #                             inventory = inventory,
  #                             organization = organization,
  #                             project = project,
  #                             project_coc = project_coc,
  #                             services = services,
  #                             user = user,
  #                             youth_education_status = youth_education_status)
  #     
  #     # fullHmisExtract <- lapply(fullHmisExtract, \(x) applyHmisClass(x))
  #     fullHmisExtract <- lapply(fullHmisExtract, applyHmisClass)
  #     
  #     class(fullHmisExtract) <- c("HMIS Extract", class(fullHmisExtract))
  #     
  #     return(fullHmisExtract)
  #   }
  # }
}

load_hmis0 <- function(hmis_rda, ..., .backward_compatability = FALSE, .use_full_extract = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  if(hmis_rda == "newest")
  {
    hmisData <- load(shortcut("newest extract"))
  }
  else
  {
    hmisData <- load(hmis_rda)
  }
  
  applyHmisClass <- function(x)
  {
    if ("Date" %in% class(x))
    {
      return(x)
    }
    else if ("numeric" %in% class(x) & !"Date" %in% class(x))
    {
      class(x) <- c("HMIS Data Standards Fiscal Year", class(x))
      
      return(x)
    }
    else
    {
      class(x) <- c("HMIS Data File", class(x))
      
      return(x)
    }
  }
  
  if (.use_full_extract)
  {
    if (.FY >= 24)
    {
      fullHmisExtract <- list(extractDate = extractDate,
                              FY = FY,
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
      
      fullHmisExtract <- lapply(fullHmisExtract, \(x) applyHmisClass(x))
      
      class(fullHmisExtract) <- c("HMIS Extract", class(fullHmisExtract))
      
      return(fullHmisExtract)
    }
    else
    {
      fullHmisExtract <- list(extractDate = extractDate,
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
      
      fullHmisExtract <- lapply(fullHmisExtract, \(x) applyHmisClass(x))
      
      class(fullHmisExtract) <- c("HMIS Extract", class(fullHmisExtract))
      
      return(fullHmisExtract)
    }
  }
  else
  {
    if (.FY >= 24)
    {
      # if (!.backward_compatability)
      # {
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
      
      hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
      
      class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
      
      return(hmisExtract)
      # }
      # else
      # {
      #   hmisExtract <- list(extractDate = extractDate,
      #                       client = client,
      #                       entry = entry,
      #                       exit = exit,
      #                       funder = funder, # Added 11-17-23 ----
      #                       services = services,
      #                       project = project,
      #                       healthanddv = healthanddv,
      #                       incomebenefits = incomebenefits,
      #                       organization = organization,
      #                       disabilities = disabilities)
      #
      #   hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
      #
      #   class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
      #
      #   return(hmisExtract)
      # }
    }
    else
    {
      if (!.backward_compatability) # Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
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
        
        hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
      else # DON'T Use Funder File
      {
        hmisExtract <- list(extractDate = extractDate,
                            client = client,
                            entry = entry,
                            exit = exit,
                            services = services,
                            project = project,
                            healthanddv = healthanddv,
                            incomebenefits = incomebenefits,
                            organization = organization,
                            disabilities = disabilities)
        
        hmisExtract <- lapply(hmisExtract, \(x) applyHmisClass(x))
        
        class(hmisExtract) <- c("HMIS Extract", class(hmisExtract))
        
        return(hmisExtract)
      }
    }
  }
}
