#' Chronicity Calculator for an Individual Client
#'
#' Estimates whether a single client may be experiencing chronic homelessness
#' per the available information in their most recent entry in the HMIS export.
#'
#' @param hmis_extract The full `CSV Extract` List. This will Automatically use
#'   `hmis` if it is in the Global Environment. It can also be piped in.
#'   Otherwise, it will search the global environment and use the first HMIS
#'   extract it finds, or it will call [load_hmis("newest")][load_hmis()] and
#'   use that if there is no HMIS extract in the environment to use.
#' @param spid the client's SPID: the value in the `PersonalID` column.
#' @param extractdate the date the CSV/XML extract was pulled from HMIS:
#'   YYYY-MM-DD. defaults to `hmis_extract$extractDate`.
#'
#' @return TRUE or FALSE.
#' @export
maybeChronic_spid <- function(hmis_extract = NULL, spid, extract_date = NULL)
{
  requireNamespace("dplyr", quietly = TRUE)  
  requireNamespace("tidyr", quietly = TRUE)
  
  chronicStatus <- NULL
  
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
    extract_date <- hmis_extract$extractDate
  }
  
  # Is this Needed?
  # clientData <- hmis_extract$client %>%
  #   filter(PersonalID == spid)
  # 
  # entryData <- hmis_extract$entry %>%
  #   filter(PersonalID == spid)
  # 
  # mergedData <- clientData %>%
  #   left_join(entryData, by = 'PersonalID') %>%
  #   arrange(desc(EntryDate)) %>%
  #   slice_head(n = 1) %>%
  #   replace_na(list(DisablingCondition = 0)) %>%
  #   replace_na(list(DateToStreetESSH = as.Date(extract_date))) %>%
  #   replace_na(list(TimesHomelessPastThreeYears = 0)) %>%
  #   replace_na(list(MonthsHomelessPastThreeYears = 0))
  
  entryData <- hmis_extract$entry |>
    dplyr::filter(PersonalID == spid) |> 
    tidyr::replace_na(list(DisablingCondition = 0,
                           DateToStreetESSH = extract_date,
                           TimesHomelessPastThreeYears = 0,
                           MonthsHomelessPastThreeYears = 0))
  
  disability <- ifelse(entryData$DisablingCondition == 1, TRUE, FALSE)
  
  timeline <- ifelse(extract_date - as.Date(entryData$DateToStreetESSH) >= 365
                     | ((entryData$TimesHomelessPastThreeYears == 4) & (entryData$MonthsHomelessPastThreeYears %in% c(112, 113))),
                     TRUE,
                     FALSE)
  
  chronicStatus <- ifelse(disability & timeline, TRUE, FALSE)
  
  return(chronicStatus)
  
  # if(mergedData$DisablingCondition == 1)
  # {
  #   if((as.integer(as.Date(extract_date) - as.Date(mergedData$DateToStreetESSH)) >= 365) | ((mergedData$TimesHomelessPastThreeYears >= 4) & (mergedData$MonthsHomelessPastThreeYears >= 12)))
  #   {
  #     chronicStatus = TRUE
  #   }
  #   else
  #   {
  #     chronicStatus = FALSE
  #   }
  # }
  # else
  # {
  #   chronicStatus = FALSE
  # }
  # 
  # return(chronicStatus)
}
