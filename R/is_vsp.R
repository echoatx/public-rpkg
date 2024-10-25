#' Do Any Supplied Project IDs Belong to a VSP?
#'
#' Determines whether the Project IDs belong to a VSP, and optionally whether
#' that VSP uses an alternate database instead of HMIS.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param project_id A single Project ID or multiple Project IDs (this function
#'   is vectorized).
#' @param alternate_database_users_only Defaults to `FALSE`. If set to `TRUE`
#'   this will only return `TRUE` for VSPs that use an alternate database
#'   instead of HMIS.
#' @param hmis_extract The full CSV Extract List. This will Automatically use
#'   "hmis" if it is in the Global Environment, but otherwise you must specify
#'   the hmis_extract variable.
#'
#' @return `TRUE` or `FALSE` for each supplied Project ID, depending on whether
#'   it belongs to a VSP organization. (If `alternate_database_users_only` is
#'   set to `TRUE`, then any VSPs that use HMIS and not an alternate database
#'   instead will aslo return `FALSE`.)
#' @export
is_vsp <- function(project_id, ..., alternate_database_users_only = FALSE, hmis_extract = NULL)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr", quietly = TRUE)
  
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
                        "i" = "{.emph {.fn is_vsp} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      }
      else
      {
        hmis_extract <- load_hmis("newest")
        
        cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                        "i" = "{.emph {.fn is_vsp} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn is_vsp} is: {.val {hmis_extract$extractDate}}.}"))
      }
    }
  }
  
  vsp_ids <- hmis_extract$organization |>
    dplyr::filter(OrganizationName %in% c("SAFE Alliance")) |>
    dplyr::pull(ProjectID)
  
  is.vsp <- function(id)
  {
    id %in% vsp_ids
  }
  
  return(vapply(project_id, is.vsp, FUN.VALUE = logical(1)))
}
