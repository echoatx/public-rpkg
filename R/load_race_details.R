#' Loads or Attaches Client Races/Ethnicity by PersonalID from the HMIS CSV/XML
#' Export.
#'
#' [load_race_details()] returns a data frame containing all race answers per
#' PersonalID in the `Client.csv` file associated with the HMIS Extract you are
#' using.
#'
#' [attach_race_details()] attaches those details to a data frame, using
#' Tidyverse's [dplyr::left_join()] function (by `PersonalID`) a data frame
#' containing all race answers per `PersonalID` in the `Client.csv` file
#' associated with the HMIS Extract you are using.
#'
#' @param .data for `attach_race_details()`, the tibble or data frame you want
#'   the full race/ethnicity data attached to.
#' @param hmis_extract the hmis extract to use for these functions. If none is
#'   supplied, the function will try to find an HMIS extract named `hmis` in
#'   your environment. If it cannot do so, it will then try to find any HMIS
#'   Extract in your environment to use (if there is more than one it will
#'   select the first one it finds). If it cannot find an extract, it will call
#'   [load_hmis("newest")] to use that, and it will inform you that it did so.
#'
#' @return [load_race_details()] returns a data frame with the Race/Ethnicity
#'   answers for each client in the `Client.csv` file associated with the HMIS
#'   Extract used for the `hmis_data` argument. [attach_race_details()] calls
#'   [load_race_details()] and then uses the Tidyverse's [dplyr::left_join()]
#'   function to add the Race/Ethnicity data for each client in `.data` from
#'   their answers in the `Client.csv` file associated with the HMIS Extract
#'   used for the `hmis_data` argument.
#' @export
load_race_details <- function(hmis_extract = NULL, hmis_extract_full = NULL)
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
                        "i" = "{.emph {.fn load_race_details} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      }
      else
      {
        hmis_extract <- load_hmis("newest")
        
        cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                        "i" = "{.emph {.fn load_race_details} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn load_race_details} is: {.val {hmis_extract$extractDate}}.}"))
      }
    }
  }
  
  if (is.null(hmis_extract_full))
  {
    if ("hmis_full" %in% ls(envir = .GlobalEnv))
    {
      hmis_extract_full <- get("hmis", envir = .GlobalEnv)
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
        
        if (length(other_hmis_extract) == 26)
        {
          hmis_extract_full <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
          
          cli::cli_warn(c("!" = "{.strong {.fn load_race_details} could not find an object named {.envvar hmis_full}. (If a {.emph FULL} HMIS extract is loaded in your environment as {.envvar hmis_full} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn load_race_details} found a {.strong full} HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
        }
        else
        {
          hmis_extract_full <- load_hmis("newest", .use_full_extract = TRUE)
          
          cli::cli_warn(c("!" = "{.strong {.fn load_race_details} could not find an object named {.envvar hmis_full}. (If a {.emph FULL} HMIS extract is loaded in your environment as {.envvar hmis_full} then this issue will autoresolve.)}",
                          "i" = "{.emph {.fn load_race_details} could not find another {.strong full} HMIS extract in your environment, so it called `load_hmis({.val newest}, .use_full_extract = {.val {TRUE}})` and used that instead. The date of the extract used by {.fn load_race_details} is: {.val {hmis_extract_full$extractDate}}.}"))
        }
      }
      else
      {
        hmis_extract_full <- load_hmis("newest", .use_full_extract = TRUE)
        
        cli::cli_warn(c("!" = "{.strong {.fn load_race_details} could not find an object named {.envvar hmis_full}. (If a {.emph FULL} HMIS extract is loaded in your environment as {.envvar hmis_full} then this issue will autoresolve.)}",
                        "i" = "{.emph {.fn load_race_details} could not find another {.strong full} HMIS extract in your environment, so it called `load_hmis({.val newest}, .use_full_extract = {.val {TRUE}})` and used that instead. The date of the extract used by {.fn load_race_details} is: {.val {hmis_extract_full$extractDate}}.}"))
      }
    }
  }
  # client_races <- dplyr::`%>%`(suppressWarnings(readr::read_csv(shortcut("sdr data", paste0("CSVExtract", gsub("-", "", hmis_extract$extractDate)), "Client.csv"), show_col_types = FALSE)) |>
  #                                dplyr::select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican, HispanicLatinaeo, MidEastNAfrican, NativeHIPacific, White, RaceNone),
  #                              dplyr::mutate(dplyr::across(AmIndAKNative:White, as.logical),
  #                                            dplyr::across(AmIndAKNative:White, \(x)  dplyr::if_else(!is.na(.$RaceNone), NA, x)))) |> 
  #   dplyr::select(-RaceNone)
  
  client_races <- dplyr::`%>%`(hmis_extract_full$Client.csv |>
                                 dplyr::select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican, HispanicLatinaeo, MidEastNAfrican, NativeHIPacific, White, RaceNone),
                               dplyr::mutate(dplyr::across(AmIndAKNative:White, as.logical),
                                             dplyr::across(AmIndAKNative:White, \(x)  dplyr::if_else(!is.na(.$RaceNone), NA, x)))) |> 
    dplyr::select(-RaceNone)
}
#' @rdname load_race_details
#' @export
attach_race_details <- function(.data, hmis_extract = NULL, hmis_extract_full = NULL)
{
  requireNamespace("dplyr", quietly = TRUE)
  
  .hmis_extract <- hmis_extract
  
  .hmis_extract_full <- hmis_extract_full
  
  .data <- .data |> 
    dplyr::left_join(load_race_details(.hmis_extract, .hmis_extract_full), by = c("PersonalID"))
  
  return(.data)
}
