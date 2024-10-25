#' Calculate HUD System Performance Measures
#'
#' Use an HMIS data export to calculate one or more HUD System Performance
#' Measures for a given federal fiscal year.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param hmis_extract The HMIS Extract to use (may be piped in, or if left
#'   null, this function will call [load_hmis("newest")]).
#' @param spm As a character (in "quotes" if there is a letter, i.e., "2a"),
#'   which SPM (1-7b). You may select more than one, i.e., `c(1, "2b", 5, "7a")`
#' @param fiscal_year The federal fiscal year for which to run the SPMs.
#'
#' @return One or more SPMs.
#' @export
calculate_spm_details <- function(hmis_extract = NULL, spm, ..., fiscal_year)
{
  rlang::check_dots_empty()
  
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  if (nchar(fiscal_year) != 4)
  {
    cli::cli_abort(c("!" = "Invalid input for {.arg fiscal_year} in {.fn calculate_spm}",
                     "i" = "The argument {.arg fiscal_year} must be a 4-digit year (e.g., {.or {.val {c(1996, 2017, 2024)}}})",
                     "x" = "You entered: {.val {fiscal_year}}."))
  }
  
  sysPM <- c(1, "2a", "2b", 3, 4, 5, 6, "6a", "6b", "6c", "7a", "7b")
  
  if (!any(spm %in% sysPM))
  {
    cli::cli_abort(c("!" = "Invalid input for {.arg spm} in {.fn calculate_spm}",
                     "i" = "The argument {.arg spm} must be: {.or {.val {sysPM}}})",
                     "x" = "You entered: {.val {spm}}."))
  }
  
  if (is.null(hmis_extract))
  {
    if ("hmis" %in% ls(envir = .GlobalEnv))
    {
      hmis_extract <- get("hmis", envir = .GlobalEnv)
    }
    else
    {
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
                        "i" = "{.emph {.fn calculate_spm} found an HMIS extract called {.envvar {.val {other_hmis_extract}}} in your environment, and used that instead.}"))
      }
      else
      {
        hmis_extract <- load_hmis("newest")
        
        cli::cli_warn(c("!" = "{.strong The {.arg hmis_extract} argument is missing. (If an HMIS extract is loaded in your environment as {.envvar hmis} then this issue will autoresolve.)}",
                        "i" = "{.emph {.fn calculate_spm} could not find another HMIS extract in your environment, so it called `load_hmis(\"newest\")` and used that instead. The date of the extract used by {.fn calculate_spm} is: {.val {hmis_extract$extractDate}}.}"))
      }
    }
  }
  
  reporting_period <- lubridate::interval(paste0(fiscal_year - 1, "-10-01"),
                                          paste0(fiscal_year, "-09-30"))
  
  run_spm_1 <- function(.data, rp)
  {
    return ("spm-1")
  }
  
  run_spm_2a <- function(.data, rp)
  {
    return ("spm-2a")
  }
  
  run_spm_2b <- function(.data, rp)
  {
    return ("spm-2b")
  }
  
  run_spm_3 <- function(.data, rp)
  {
    return ("spm-3")
  }
  
  run_spm_4 <- function(.data, rp)
  {
    return ("spm-4")
  }
  
  run_spm_5 <- function(.data, rp)
  {
    return ("spm-5")
  }
  
  run_spm_6 <- function(.data, rp)
  {
    return ("spm-6")
  }
  
  run_spm_6a <- function(.data, rp)
  {
    return ("spm-6a")
  }
  
  run_spm_6b <- function(.data, rp)
  {
    return ("spm-6b")
  }
  
  run_spm_6c <- function(.data, rp)
  {
    return ("spm-6c")
  }
  
  run_spm_7a <- function(.data, rp)
  {
    return ("spm-7a")
  }
  
  run_spm_7b <- function(.data, rp)
  {
    return ("spm-7b")
  }
  
  run_spm <- function(.spm)
  {
    switch (as.character(.spm),
            "1"  = run_spm_1(hmis_extract,  reporting_period),
            "2a" = run_spm_2a(hmis_extract, reporting_period),
            "2b" = run_spm_2b(hmis_extract, reporting_period),
            "3"  = run_spm_3(hmis_extract,  reporting_period),
            "4"  = run_spm_4(hmis_extract,  reporting_period),
            "5"  = run_spm_5(hmis_extract,  reporting_period),
            "6"  = run_spm_6(hmis_extract,  reporting_period),
            "6a" = run_spm_6a(hmis_extract, reporting_period),
            "6b" = run_spm_6b(hmis_extract, reporting_period),
            "6c" = run_spm_6c(hmis_extract, reporting_period),
            "7a" = run_spm_7a(hmis_extract, reporting_period),
            "7b" = run_spm_7b(hmis_extract, reporting_period))
  }
  
  result <- switch (as.character(length(spm)),
                    "1" = { return (run_spm(spm)) },
                    purrr::map(spm, \(x) run_spm(x)))
  
  for (i in 1:length(spm))
  {
    names(result)[i] <- paste0("SPM-", spm[i])
  }
  
  return (result)
}
