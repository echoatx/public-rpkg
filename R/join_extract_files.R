#' Quickly Join The Entry/Exit/Client/Project Files
#'
#' This will use [dplyr::left_join()] to join the Entry, Exit, Client, and
#' Project files of the HMIS Extract, optionally filtering for entry dates
#' within a specific reporting period.
#'
#' @inheritParams rlang::args_dots_empty
#' @importFrom rlang .data
#'
#' @param .data the HMIS CSV/XML extract list of tibbles.
#' @param reporting_period If set to a [lubridate::interval()],
#'   `join_extract_files()` will use [dplyr::filter()] to filter for
#'   `EntryDate`s within the given interval.
#'
#' @return As a `tibble`, the joined Entry/Exit/Client/Project files.
#' @export
join_extract_files <- function(.data, ..., reporting_period = NULL)
{
  rlang::check_dots_empty()
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  
  # if (is.null(.data))
  # {
  #   if ("hmis" %in% ls(envir = .GlobalEnv))
  #   {
  #     .data <- get("hmis", envir = .GlobalEnv)
  #   }
  #   else
  #   {
  #     requireNamespace("purrr", quietly = TRUE)
  #     
  #     is.hmis <- function(obj)
  #     {
  #       return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
  #     }
  #     
  #     gl_env_list <- as.list(globalenv()) |>
  #       purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
  #     
  #     if (!purrr::is_empty(gl_env_list))
  #     {
  #       other_.data <- names(gl_env_list)[1]
  #       
  #       .data <- get(paste0(other_.data), envir = .GlobalEnv)
  #       
  #       cli::cli_warn(c("!" = "{.strong The {.arg .data} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
  #                       "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_.data}}} in your environment, and used that instead.}"))
  #     }
  #     else
  #     {
  #       .data <- load_hmis("newest")
  #       
  #       cli::cli_warn(c("!" = "{.strong The {.arg .data} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
  #                       "i" = "{.emph {.fn this_project} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn this_project} is: {.val {.data$extractDate}}.}"))
  #     }
  #   }
  # }
  
  .data <- .data$entry |> 
    dplyr::left_join(.data$exit, by = c("EnrollmentID", "PersonalID")) |> 
    dplyr::left_join(.data$client, by = "PersonalID") |> 
    dplyr::left_join(.data$project, by = "ProjectID")
  
  if(is.null(reporting_period))
  {
    # .data <- joined_files
    
    return(.data)
  }
  
  requireNamespace("lubridate", quietly = TRUE)
  
  # .data <- joined_files
  
  return(.data |> dplyr::filter(EntryDate <= lubridate::int_end(reporting_period)
                                & (lubridate::`%within%`(ExitDate, reporting_period) | is.na(ExitDate))))
}

# join_extract_files_ORIG <- function(hmis_extract = NULL, ..., reporting_period = NULL)
# {
#   rlang::check_dots_empty()
#   
#   requireNamespace("dplyr", quietly = TRUE)
#   
#   if (is.null(hmis_extract))
#   {
#     if ("hmis" %in% ls(envir = .GlobalEnv))
#     {
#       hmis_extract <- get("hmis", envir = .GlobalEnv)
#     }
#     else
#     {
#       requireNamespace("purrr", quietly = TRUE)
#       
#       is.hmis <- function(obj)
#       {
#         return(ifelse("HMIS Extract" %in% class(obj), TRUE, FALSE))
#       }
#       
#       gl_env_list <- as.list(globalenv()) |>
#         purrr::keep(\(x) ifelse(is.hmis(x), TRUE, FALSE))
#       
#       if (!purrr::is_empty(gl_env_list))
#       {
#         other_hmis_extract <- names(gl_env_list)[1]
#         
#         hmis_extract <- get(paste0(other_hmis_extract), envir = .GlobalEnv)
#         
#         cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
#                         "i" = "{.emph {.fn this_project} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
#       }
#       else
#       {
#         hmis_extract <- load_hmis("newest")
#         
#         cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
#                         "i" = "{.emph {.fn this_project} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn this_project} is: {.val {hmis_extract$extractDate}}.}"))
#       }
#     }
#   }
#   
#   joined_files <- hmis_extract$entry |> 
#     dplyr::left_join(hmis_extract$exit, by = c("EnrollmentID", "PersonalID")) |> 
#     dplyr::left_join(hmis_extract$client, by = "PersonalID") |> 
#     dplyr::left_join(hmis_extract$project, by = "ProjectID")
#   
#   if(is.null(reporting_period))
#   {
#     return(joined_files)
#   }
#   
#   requireNamespace("lubridate", quietly = TRUE)
#   
#   return(joined_files |> dplyr::filter(lubridate::`%within%`(EntryDate, reporting_period)))
# }
