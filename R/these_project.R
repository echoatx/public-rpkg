#' Vectorized version of [this_project()]
#'
#' Returns the **types**, **names**, or **IDs** of the projects, based on the
#' input into the function.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param flag Either `"names"`, `"groups"`, or `"types"`, depending on your
#'   desired output.
#' @param project_input Depending on the **flag**:
#' * For the __`"names"`__ flag, the input should be the `ProjectID` numbers for which you want the project names.
#' * For the __`"groups"`__ flag, the input should be:
#'    - the groups of projects (as a character vector) for which you want the project IDs (i.e. `c("coc", "yhdp")`).
#'    - _(NOTE: You can use_ [this_project("group", "ls")][this_project()] _if you need the list of valid group strings.)_
#' * For the __"types"__ flag, the input should be:
#'    - the HMIS `ProjectType` CSV Codes to be converted to project type labels/titles;
#'    - the `ProjectID`s for which you want the project types returned; or
#'    - the project type labels/titles as strings (i.e. `"rrh"`/`"Rapid Rehousing"`) to get the corresponding CSV Codes; To get the full list of Project Type labels you may run [this_project("type", "ls")][this_project()].
#' @param full_label If set to `TRUE` when using the `"types"` flag, the
#'   function will return the full tile of the project type instead of the
#'   shorthand label ("Emergency Shelter" instead of "es" ... et cetera).
#'   _**Defaults to `FALSE`.**_
#' @param active_only If set to `TRUE`, this will filter the results to only
#'   return `ProjectID`s that are *active* in HMIS (inactive projects are
#'   recorded with a `ZZZ` prefix). _**Defaults to `FALSE`.**_
#' @param hmis_extract The full `CSV Extract` List. This will Automatically use
#'   `hmis` if it is in the Global Environment. Otherwise, it will search the
#'   global environment and use the first HMIS extract it finds, or it will call
#'   [load_hmis("newest")][load_hmis()]  and use that if there is no HMIS
#'   extract in the environment to use.
#' @param .FY The HMIS Data Standards Fiscal Year (entered numerically with two
#'   digits, i.e., `22` for FY22). _**Defaults to FY24.**_ Can be set backwards
#'   up to FY22 for backwards compatibility with older HMIS Extracts.
#'
#' @return the project **types**, **names**, or **IDs**.
#' @export
#' @examples
#' \dontrun{
#' # This will return `"es-nbn"` and `"rrh"`:
#' these_project("types", c(1, 13))
#'
#' # This will return `"Emergency Shelter - Night-by-Night"` and `"PH - Rapid Re-Housing"`:
#' these_project("types", c(1, 13), full_label = TRUE)
#'
#' # This will return `1` and `13`:
#' these_project("types", c("es-nbn", "rrh"))
#'
#' # This will return all ACTIVE CoC & YHDP ProjectIDs:
#' these_project("groups", c("coc", "yhdp"), active_only = TRUE)
#'
#' # This will add a `Project.Name` column to the data frame named entry:
#' hmis <- load_hmis("newest")
#' entry <- hmis$entry
#' entry <- entry |>
#'   dplyr::mutate(Project.Name = these_project("names", ProjectID))
#' }
these_project <- function(flag, project_input, ..., full_label = FALSE, active_only = FALSE, hmis_extract = NULL, .FY = 24)
{
  rlang::check_dots_empty()
  
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  
  validPluralFlags <- c("types", "names", "groups")
  
  if (!flag %in% validPluralFlags)
  {
    cli::cli_abort(c("!" = "{.strong The {.arg flag} argument for {.fn these_project} must be: {.or {.val {validPluralFlags}}}.}",
                     "x" = "You entered: {.val {flag}}."))
  }
  
  fy <- .FY
  
  singularFlag <- switch (flag,
                          "types" = "type",
                          "names" = "name",
                          "groups" = "group")
  
  these_full_labels <- full_label
  
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
  
  these_data <- hmis_extract
  
  project_input <- project_input |> tidyr::replace_na(5)
  
  combine_groups <- function(.groups)
  {
    result <- NULL
    
    for (i in 1:length(.groups))
    {
      result <- c(result, this_project("group", .groups[i]))
      
      rm(i)
    }
    
    return(result)
  }
  
  mapType <- switch(singularFlag,
                    "name"  = "chr",
                    "group" = "na",
                    "type"  = switch(typeof(project_input),
                                     "character" = "dbl",
                                     "double" = "chr"))
  
  result <- switch(mapType,
                   "chr" = purrr::map_chr(tidyr::replace_na(project_input, 0666), \(x) this_project(singularFlag, x, full_label = these_full_labels, hmis_extract = these_data, .FY = fy), .progress = paste0("project ", flag, ":")),
                   "dbl" = purrr::map_dbl(project_input, \(x) this_project(singularFlag, x, full_label = these_full_labels, hmis_extract = these_data, .FY = fy), .progress = paste0("project ", flag, ":")),
                   "na"  = unique(unlist(purrr::map(project_input, \(x) this_project(singularFlag, x, hmis_extract = these_data, .FY = fy)))))
  
  if (active_only)
  {
    if (!mapType %in% c("dbl", "na"))
    {
      cli::cli_warn(c("!" = "The {.arg active_only} argument can only be used if the {.fn these_project} function is returning numerical project IDs. This function is disregarding your supplied argument of {.strong `active_only = {.val {active_only}}`}."))
    }
    else
    {
      result <- dplyr::select(hmis_extract$project, ProjectID, ActiveProject) |>
        dplyr::filter(ProjectID %in% result, ActiveProject) |>
        dplyr::pull(ProjectID)
    }
  }
  
  return(result)
}
