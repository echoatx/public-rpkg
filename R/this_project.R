#' Check the Type/Name/ID of a Project
#'
#' Returns the **type**, **name**, or **ID** of the project, based on the input
#' into the function.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param flag Either `"name"`, `"group"`, or `"type"`, depending on your
#'   desired output.
#' @param project_input Depending on the **flag**:
#' * For the __`"name"`__ flag, the input should be the `ProjectID` number.
#' * For the __`"group"`__ flag, the input should be:
#'    - the group of projects for which you want the project IDs (as a string, i.e. `"coc"`), or
#'    - `"ls"` if you need the list of valid group strings.
#' * For the __`"type"`__ flag:
#'    - the `ProjectType` label as a string (i.e., `"rrh"`);
#'    - the HMIS `ProjectType` CSV Code as an integer (i.e., `3` for _Permanent Supportive Housing (PSH)_);
#'    - A `ProjectID` number to get that project's project type; or
#'    - `"ls"` to get the full list of Project Types.
#' @param full_label If set to `TRUE` when using the `"type"` flag, the function
#'   will return the full title of the project type instead of the shorthand
#'   label (i.e., "Emergency Shelter" instead of "es" ... et cetera).
#'   _**Defaults to `FALSE`.**_
#' @param hmis_extract The full `CSV Extract` List. This will Automatically use
#'   `hmis` if it is in the Global Environment. Otherwise, it will search the
#'   global environment and use the first HMIS extract it finds, or it will call
#'   [load_hmis("newest")][load_hmis()] and use that if there is no HMIS extract
#'   in the environment to use.
#' @param .FY The HMIS Data Standards Fiscal Year (entered numerically with two
#'   digits, i.e., `22` for FY22). _**Defaults to FY24.**_ Can be set backwards
#'   up to FY22 for backwards compatibility with older HMIS Extracts.
#'
#' @return the project *type*, *name*, or *id*.
#' @export
this_project <- function(flag, project_input, ..., full_label = FALSE, hmis_extract = NULL, .FY = 24)
{
  rlang::check_dots_empty()
  
  validFlags <- c("type", "name", "group")
  
  pType <- switch (as.character(.FY),
                   "22" = pType_FY22,
                   "24" = pType_FY24)
  
  #### CONDITIONS ####
  if (!flag %in% validFlags)
  {
    cli::cli_abort(c("x" = "{.strong {.val {flag}} is not a valid input for the {.arg flag} argument.}",
                     "i" = "{.emph The input for {.arg flag} must be: {.or {.val {validFlags}}}.}"))
  }
  
  if (flag == "name")
  {
    requireNamespace("dplyr", quietly = TRUE)
    
    # if (is.null(hmis_extract))
    # {
    #   ifelse("hmis" %in% ls(envir = .GlobalEnv),
    #          hmis_extract <- get("hmis", envir = .GlobalEnv),
    #          cli::cli_abort(c("x" = "{.storng The {.arg hmis_extract} argument is missing.}",
    #                           "i" = "If the HMIS extract is loaded in the environment as {.envvar hmis} then this issue will autoresolve. If the extract is called something else you will have to manually assign the extract to the {.arg hmis_extract} argument.")))
    # }
    
    if (is.null(hmis_extract))
    {
      if ("hmis" %in% ls(envir = .GlobalEnv))
      {
        hmis_extract <- get("hmis", envir = .GlobalEnv)
      }
      else
      {
        requireNamespace("purrr", quietly = TRUE)
        
        is.hmis <- function(obj)
        {
          return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
        }
        
        gl_env_list <- as.list(globalenv()) |>
          purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
        
        if (!purrr::is_empty(gl_env_list))
        {
          other_hmis_extract <- names(gl_env_list)[1]
          
          hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
          
          cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
        }
        else
        {
          hmis_extract <- load_hmis("newest")
          
          cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn this_project} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn this_project} is: {.val {hmis_extract$extractDate}}.}"))
        }
      }
      # else
      # {
      #   is.hmis <- function(obj)
      #   {
      #     return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
      #   }
      #   
      #   gl_env_list <- as.list(globalenv()) |>
      #     purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
      #   
      #   other_hmis_extract <- ifelse(!purrr::is_empty(gl_env_list),
      #                                names(gl_env_list)[1],
      #                                cli::cli_abort(c("x" = "{.strong The {.arg hmis_extract} argument is missing.}",
      #                                                 "i" = "If the HMIS extract is loaded in the environment as {.envvar hmis} then this issue will autoresolve. If the extract is called something else you will have to manually assign the extract to the {.arg hmis_extract} argument.")))
      #   
      #   hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
      #   
      #   cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
      #                   "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      # }
    }
  }
  
  #### FUNCTION ####
  
  # Group Flag ----
  if (flag == "group")
  {
    requireNamespace("dplyr", quietly = TRUE)
    requireNamespace("readr", quietly = TRUE)
    
    if (is.null(hmis_extract))
    {
      if ("hmis" %in% ls(envir = .GlobalEnv))
      {
        hmis_extract <- get("hmis", envir = .GlobalEnv)
      }
      else
      {
        requireNamespace("purrr", quietly = TRUE)
        
        is.hmis <- function(obj)
        {
          return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
        }
        
        gl_env_list <- as.list(globalenv()) |>
          purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
        
        if (!purrr::is_empty(gl_env_list))
        {
          other_hmis_extract <- names(gl_env_list)[1]
          
          hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
          
          cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
        }
        else
        {
          hmis_extract <- load_hmis("newest")
          
          cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn this_project} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn this_project} is: {.val {hmis_extract$extractDate}}.}"))
        }
      }
      # else
      # {
      #   is.hmis <- function(obj)
      #   {
      #     return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
      #   }
      #   
      #   gl_env_list <- as.list(globalenv()) |>
      #     purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
      #   
      #   other_hmis_extract <- ifelse(!purrr::is_empty(gl_env_list),
      #                                names(gl_env_list)[1],
      #                                cli::cli_abort(c("x" = "{.strong The {.arg hmis_extract} argument is missing.}",
      #                                                 "i" = "If the HMIS extract is loaded in the environment as {.envvar hmis} then this issue will autoresolve. If the extract is called something else you will have to manually assign the extract to the {.arg hmis_extract} argument.")))
      #   
      #   hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
      #   
      #   cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
      #                   "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      # }
    }
    
    validGroups <- c("coc", "yhdp", "coc and yhdp", "consolidated")
    
    if (all(project_input %in% validGroups))
    {
      output <- "group"
      pList = switch (project_input,
                      # "coc" = cocProjects,
                      "coc" = dplyr::pull(dplyr::distinct(dplyr::arrange(dplyr::filter(dplyr::left_join(hmis_extract$project, hmis_extract$funder, by = c("ProjectID")), Funder %in% c(1:7, 44)), -dplyr::desc(ProjectID), dplyr::desc(StartDate)), ProjectID, .keep_all = TRUE), ProjectID),
                      # "coc" = dplyr::pull(dplyr::distinct(dplyr::arrange(dplyr::filter(dplyr::left_join(hmis_extract$project, suppressWarnings(readr::read_csv(shortcut("sdr data", paste0("CSVExtract", gsub("-", "", hmis_extract$extractDate)), "Funder.csv"), show_col_types = FALSE)), by = c("ProjectID")), Funder %in% c(1:7, 44)), -dplyr::desc(ProjectID), dplyr::desc(StartDate)), ProjectID, .keep_all = TRUE), ProjectID),
                      # "yhdp" = yhdpProjects,
                      "yhdp" = dplyr::pull(dplyr::distinct(dplyr::arrange(dplyr::filter(dplyr::left_join(hmis_extract$project, hmis_extract$funder, by = c("ProjectID")), Funder %in% c(43)), -dplyr::desc(ProjectID), dplyr::desc(StartDate)), ProjectID, .keep_all = TRUE), ProjectID),
                      # "yhdp" = dplyr::pull(dplyr::distinct(dplyr::arrange(dplyr::filter(dplyr::left_join(hmis_extract$project, suppressWarnings(readr::read_csv(shortcut("sdr data", paste0("CSVExtract", gsub("-", "", hmis_extract$extractDate)), "Funder.csv"), show_col_types = FALSE)), by = c("ProjectID")), Funder %in% c(43)), -dplyr::desc(ProjectID), dplyr::desc(StartDate)), ProjectID, .keep_all = TRUE), ProjectID),
                      # "coc and yhdp" = c(cocProjects, yhdpProjects),
                      "coc and yhdp" = dplyr::pull(dplyr::distinct(dplyr::arrange(dplyr::filter(dplyr::left_join(hmis_extract$project, suppressWarnings(readr::read_csv(shortcut("sdr data", paste0("CSVExtract", gsub("-", "", hmis_extract$extractDate)), "Funder.csv"), show_col_types = FALSE)), by = c("ProjectID")), Funder %in% c(1:7, 43:44)), -dplyr::desc(ProjectID), dplyr::desc(StartDate)), ProjectID, .keep_all = TRUE), ProjectID),
                      "consolidated" = consolidatedProjects)
    }
    else if (project_input == "ls")
    {
      output <- "list"
    }
    else
    {
      ls_arg <- "ls"
      group_flag <- "group"
      
      cli::cli_abort(c("!" = "{.strong The input for {.fn this_project}, if using the {.val {group_flag}} {.arg flag}, must be a valid project group: {.or {.val {validGroups}}}.}",
                       "x" = "You entered: {.val {project_input}}.",
                       "i" = "{.emph You can always use {.strong `this_project({.val {group_flag}}, {.val {ls_arg}})`} to see the list of valid project groups.}"))
    }
  }
  
  # Type Flag ----
  if (flag == "type")
  {
    # Input is "ls"
    if (typeof(project_input) == "character")
    {
      if (project_input %in% pType$Label & project_input != "ls")
      {
        output <- "code"
      }
      else if (project_input == "ls")
      {
        output <- "list"
      }
      else
      {
        ls_arg <- "ls"
        type_flag <- "type"
        p_labels <- c("es", "rrh")
        one <- 0
        five <- 5
        fourteen <- 14
        true_var <- TRUE
        false_var <- FALSE
        
        cli::cli_abort(c("!" = "{.strong The input for this function, if using the {.val {type_flag}} {.arg flag}, must be a Project Type label (i.e., {.or {.val {p_labels}}}), a number ({.val {one}}-{.val {fourteen}}, except for {.val {five}}), or {.val {ls_arg}} (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}). Otherwise, if you were trying to get the project type for a project ID, please ensure you have entered the 3 digit or greater project ID number correctly.}",
                         "x" = "You entered: {.val {project_input}}.",
                         "i" = "{.emph You can always use {.strong `this_project({.val {type_flag}}, {.val {ls_arg}})`} to see the list of valid project types (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}).}"))
      }
    }
    # Input is a CSV Code or Project ID
    else
    {
      if (nchar(project_input) < 3)
      {
        if (project_input %in% pType$Code & !full_label)
        {
          output <- "label"
          convertCodeToRow <- which(pType$Code == project_input)
        }
        else if (nchar(project_input) < 3 & project_input %in% pType$Code & full_label)
        {
          output <- "title"
          convertCodeToRow <- which(pType$Code == project_input)
        }
        else
        {
          ls_arg <- "ls"
          type_flag <- "type"
          p_labels <- c("es", "rrh")
          one <- 0
          five <- 5
          fourteen <- 14
          true_var <- TRUE
          false_var <- FALSE
          
          cli::cli_abort(c("!" = "{.strong The input for this function, if using the {.val {type_flag}} {.arg flag}, must be a Project Type label (i.e., {.or {.val {p_labels}}}), a number ({.val {one}}-{.val {fourteen}}, except for {.val {five}}), or {.val {ls_arg}} (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}). Otherwise, if you were trying to get the project type for a project ID, please ensure you have entered the 3 digit or greater project ID number correctly.}",
                           "x" = "You entered: {.val {project_input}}.",
                           "i" = "{.emph You can always use {.strong `this_project({.val {type_flag}}, {.val {ls_arg}})`} to see the list of valid project types (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}).}"))
        }
      }
      else
      {
        # This seems to be causing an error where you cannont assign another extract name - only "hmis"
        # ifelse("hmis" %in% ls(envir = .GlobalEnv),
        #        hmis_extract <- get("hmis", envir = .GlobalEnv),
        #        stop("\n\n ERROR: hmis_extract is missing.\n\nIf the HMIS extract is loaded in the environment as \"hmis\" this issue will autoresolve.\n\nIf the extract is called something else you will have to manually assign the extract to the \"hmis_extract\" argument.\n\n"))
        
        # if (is.null(hmis_extract))
        # {
        #   ifelse("hmis" %in% ls(envir = .GlobalEnv),
        #          hmis_extract <- get("hmis", envir = .GlobalEnv),
        #          cli::cli_abort(c("x" = "{.storng The {.arg hmis_extract} argument is missing.}",
        #                           "i" = "If the HMIS extract is loaded in the environment as {.envvar hmis} then this issue will autoresolve. If the extract is called something else you will have to manually assign the extract to the {.arg hmis_extract} argument.")))
        # }
        
        if (is.null(hmis_extract))
        {
          if ("hmis" %in% ls(envir = .GlobalEnv))
          {
            hmis_extract <- get("hmis", envir = .GlobalEnv)
          }
          else
          {
            requireNamespace("purrr", quietly = TRUE)
            
            is.hmis <- function(obj)
            {
              return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
            }
            
            gl_env_list <- as.list(globalenv()) |>
              purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
            
            if (!purrr::is_empty(gl_env_list))
            {
              other_hmis_extract <- names(gl_env_list)[1]
              
              hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
              
              cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                              "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
            }
            else
            {
              hmis_extract <- load_hmis("newest")
              
              cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                              "i" = "{.emph {.fn this_project} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn this_project} is: {.val {hmis_extract$extractDate}}.}"))
            }
          }
          # else
          # {
          #   requireNamespace("purrr", quietly = TRUE)
          #   
          #   is.hmis <- function(obj)
          #   {
          #     return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
          #   }
          #   
          #   gl_env_list <- as.list(globalenv()) |>
          #     purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
          #   
          #   other_hmis_extract <- ifelse(!purrr::is_empty(gl_env_list),
          #                                names(gl_env_list)[1],
          #                                cli::cli_abort(c("x" = "{.strong The {.arg hmis_extract} argument is missing.}",
          #                                                 "i" = "If the HMIS extract is loaded in the environment as {.envvar hmis} then this issue will autoresolve. If the extract is called something else you will have to manually assign the extract to the {.arg hmis_extract} argument.")))
          #   
          #   hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
          #   
          #   cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
          #                   "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
          # }
        }
        
        requireNamespace("dplyr", quietly = TRUE)
        
        df <- hmis_extract$project |> 
          dplyr::distinct(ProjectID, .keep_all = TRUE)
        
        prType <- function(x)
        {
          if (!x %in% hmis_extract$project$ProjectID)
          {
            ls_arg <- "ls"
            type_flag <- "type"
            p_labels <- c("es", "rrh")
            one <- 0
            five <- 5
            fourteen <- 14
            true_var <- TRUE
            false_var <- FALSE
            
            cli::cli_abort(c("!" = "{.strong Invalid input for {.arg project_input}. If you were trying to get the project type for a project ID, please ensure you have entered the 3 digit or greater project ID number correctly. Otherwise, The input for this function, if using the {.val {type_flag}} {.arg flag}, must be a Project Type label (i.e., {.or {.val {p_labels}}}), a number ({.val {one}}-{.val {fourteen}}, except for {.val {five}}), or {.val {ls_arg}} (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}).}",
                             "x" = "You entered: {.val {project_input}}.",
                             "i" = "{.emph You can always use {.strong `this_project({.val {type_flag}}, {.val {ls_arg}})`} to see the list of valid project types (with or without the optional {.arg full_label} argument set to {.val {true_var}} ... it defaults to {.val {false_var}}).}"))
          }
          
          # df <- hmis_extract$project %>%
          #   dplyr::distinct(ProjectID, .keep_all = TRUE) %>%
          #   dplyr::filter(ProjectID == x) %>%
          #   dplyr::mutate(dplyr::case_match(ProjectType, # NEW!!! ----
          #                                   NA ~ 5,
          #                                   .default = ProjectType))
          
          df |> 
            dplyr::filter(ProjectID == x) |> 
            dplyr::mutate(dplyr::case_match(ProjectType,
                                            NA ~ 5,
                                            .default = ProjectType)) |> 
            dplyr::pull(ProjectType)
        }
        
        # temp_project_input <- prType(project_input)
        # 
        # project_input <- temp_project_input
        
        project_input <- prType(project_input)
        
        convertCodeToRow <- which(pType$Code == project_input)
        
        output <- ifelse(full_label, "title", "label")
      }
    }
  }
  
  result <- switch(flag,
                   # "name" = dplyr::pull(dplyr::filter(dplyr::select(hmis_extract$project, ProjectID, ProjectName), ProjectID == project_input), ProjectName),
                   "name" = ifelse(project_input %in% hmis_extract$project$ProjectID,
                                   dplyr::pull(dplyr::filter(dplyr::select(hmis_extract$project, ProjectID, ProjectName), ProjectID == project_input), ProjectName),
                                   NA_character_),
                   "group" = switch(output,
                                    "group" = pList,
                                    "list" = validGroups),
                   "type" = switch(output,
                                   "label" = pType$Label[convertCodeToRow],
                                   "title" = pType$Title[convertCodeToRow],
                                   "code" = pType$Code[which(pType$Label == project_input)],
                                   "list" = pType$Label))
  
  return(result)
}
