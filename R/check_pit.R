#' Check Enrollments on a Specific Day
#'
#' Returns two tibbles showing total enrollments and deduplicated client PIT
#' counts on a specific day per any optional arguments.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract the full HMIS Extract List.
#' @param pit the date for the PIT count.
#' @param project_type OPTIONAL: runs a PIT count only on the specified project
#'   type.
#' @param project_id OPTIONAL: runs a PIT count only on the specified project
#'   per its `ProjectID` number.
#' @param dedup_only OPTIONAL: if set to `TRUE` returns only the deduplicated
#'   data frame (`tibble`) instead of the list of the total PIT count and the
#'   deduplicated version. Defaults to `FALSE`.
#'
#' @return a list of 2 `tibble`s: entryExitData & deduplicatedPeople (or, if
#'   `dedup_only = TRUE`, then just the deduplicated PIT count tibble).
#' @export
check_pit <- function(hmis_extract, pit, ...,
                      project_type = NULL,
                      project_id = NULL,
                      dedup_only = FALSE)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr",  quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  scope <- NULL
  
  if (is.null(project_type) & is.null(project_id)) 
  { 
    scope = "global"
    
    globalPIT <- function()
    {
      entryData <- hmis_extract$entry |>
        dplyr::left_join(dplyr::distinct(hmis_extract$project, ProjectID, .keep_all = TRUE), by = "ProjectID")
      
      entryExitData <- dplyr::left_join(entryData, hmis_extract$exit, by = c("PersonalID", "EnrollmentID")) |>
        dplyr::filter(EntryDate <= (pit) 
                      & (ExitDate >= (pit) | is.na(ExitDate)))
      
      dd_people <- entryExitData |>
        dplyr::distinct(PersonalID, .keep_all = TRUE)
      
      return(list(entryExitData = tibble::as_tibble(entryExitData),
                  deduplicatedPeople = tibble::as_tibble(dd_people)))
    }
  }
  else if (!is.null(project_type) & is.null(project_id)) 
  { 
    scope = "by-type"
    
    typePIT <- function()
    {
      entryData <- hmis_extract$entry |>
        dplyr::left_join(dplyr::distinct(hmis_extract$project, ProjectID, .keep_all = TRUE), by = "ProjectID") |>
        dplyr::filter(ProjectType %in% project_type)
      
      entryExitData <- dplyr::left_join(entryData, hmis_extract$exit, by = c("PersonalID", "EnrollmentID")) |>
        dplyr::filter((EntryDate <= (pit)) 
                      & (ExitDate >= (pit) | is.na(ExitDate)))
      
      dd_people <- entryExitData |>
        dplyr::distinct(PersonalID, .keep_all = TRUE)
      
      return(list(entryExitData = tibble::as_tibble(entryExitData),
                  deduplicatedPeople = tibble::as_tibble(dd_people)))
    }
  }
  else if (is.null(project_type) & !is.null(project_id))
  {
    scope = "by-id"
    
    idPIT <- function()
    {
      entryData <- hmis_extract$entry |>
        dplyr::filter(ProjectID %in% project_id)
      
      entryExitData <- dplyr::left_join(entryData, hmis_extract$exit, by = c("PersonalID", "EnrollmentID")) |>
        dplyr::filter((EntryDate <= (pit)) 
                      & (ExitDate >= (pit) | is.na(ExitDate)))
      
      dd_people <- entryExitData |>
        dplyr::distinct(PersonalID, .keep_all = TRUE)
      
      return(list(entryExitData = tibble::as_tibble(entryExitData),
                  deduplicatedPeople = tibble::as_tibble(dd_people)))
    }
  }
  else
  {
    cli::cli_abort(c("x" = "{.strong Invalid input for the optional arguments ({.arg project_type} / {.arg project_id}):}",
                     "!" = "The optional input for this function must be one or the other.",
                     "i" = "You may specify {.emph {.arg project_type} = x} or {.emph {.arg project_id} = y}, but not both."))
  }
  
  result <- switch(scope,
                   "global"  = globalPIT(),
                   "by-type" = typePIT(),
                   "by-id"   = idPIT())
  
  ifelse(!dedup_only,
         return(result),
         return(result$deduplicatedPeople))
}
