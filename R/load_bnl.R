#' Loads a BNL from .rda file format into the Environment.
#'
#' Returns the list of tibbles saved in the selected BNL ".rda" file.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param which_bnl the path to the BNL `.rda` file you want to load, or the
#'   date of the BNL you want to load (`"YYYY-MM-DD"`), -or- type `"newest"` to
#'   load the most recent BNL.
#' @param as.full If using the `"newest"`, parameter, set `as.full` to `TRUE` if
#'   you need to load the full BNL, not the autocleaned version. This defaults
#'   to `FALSE` and loads the autocleaned version.
#' @param as.dedup automatically deduplicates the BNL if set to `TRUE`. Defaults
#'   to `FALSE`. This keeps one instance of each PersonalID based on the most
#'   recent CADate ("API Assessment Date," if available and "ID Date" if not).
#'
#' @return loads the selected CE BNL `.rda` file.
#' @export
load_bnl <- function(which_bnl, ...,
                     as.full = FALSE,
                     as.dedup = FALSE)
{
  rlang::check_dots_empty()
  
  requireNamespace("encryptr", quietly = TRUE)
  
  if (which_bnl == "newest")
  {
    ifelse(as.full, 
           bnl_rda <- paste0(substr(shortcut("newest bnl"), 1, nchar(shortcut("newest bnl")) - 14),
                             "FULL",
                             substr(shortcut("newest bnl"), nchar(shortcut("newest bnl")) - 14, nchar(shortcut("newest bnl"))),
                             ".encryptr.bin"),
           bnl_rda <- paste0(shortcut("newest bnl"), ".encryptr.bin"))
  }
  else if (!file.exists(which_bnl))
  {
    extract_date <- as.Date(which_bnl)
    
    bnl_rda <- ifelse(as.full,
                      shortcut("bnl", paste0("bnl_FULL_", extract_date, ".rda.encryptr.bin")),
                      shortcut("bnl", paste0("bnl_", extract_date, ".rda.encryptr.bin")))
  }
  else
  {
    bnl_rda <- which_bnl
  }
  
  user_key_folder <- paste0(paste(strsplit(here::here(), "/")[[1]][1:3], collapse = "/"), "/files/")
  
  ghost_folder <- tempdir()
  
  filename_no_path <- strsplit(bnl_rda, "/")[[1]][length(strsplit(bnl_rda, "/")[[1]])]
  
  temp_bnl_file <- file.path(ghost_folder, substr(filename_no_path, 1, nchar(filename_no_path) - 13))
  
  invisible(capture.output(encryptr::decrypt_file(bnl_rda,
                                                  temp_bnl_file,
                                                  private_key_path = paste0(user_key_folder, "id_rsa_f"))))
  
  rm(user_key_folder)
  
  load(temp_bnl_file)
  
  file.remove(temp_bnl_file)
  
  # load(bnl_rda)
  
  applyBnlClass <- function(x)
  {
    if ("Date" %in% class(x))
    {
      return(x)
    }
    else
    {
      class(x) <- c("BNL Data File", class(x))
      
      return(x)
    }
  }
  
  if (as.dedup)
  {
    requireNamespace("dplyr", quietly = TRUE)
    
    if (as.full)
    {
      pshPriority <- pshPriority %>%
        dplyr::group_by(`Client Uid`) %>%
        dplyr::arrange(dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) %>%
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) %>%
        (ungroup)
      
      pshStaffing <- pshStaffing %>%
        dplyr::group_by(`Client Uid`) %>%
        dplyr::arrange(dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) %>%
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) %>%
        (ungroup)
      
      rrhPriority <- rrhPriority %>%
        dplyr::group_by(`Client Uid`) %>%
        dplyr::arrange(dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) %>%
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) %>%
        (ungroup)
      
      rrhStaffing <- rrhStaffing %>%
        dplyr::group_by(`Client Uid`) %>%
        dplyr::arrange(dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) %>%
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) %>%
        (ungroup)
      
      ceConsolidated <- ceConsolidated %>%
        dplyr::group_by(`Client Uid`) %>%
        dplyr::arrange(dplyr::desc(dplyr::if_else(!is.na(`API Assessment Date`),
                                                  `API Assessment Date`,
                                                  `ID Date`))) %>%
        dplyr::distinct(`Client Uid`, .keep_all = TRUE) %>%
        (ungroup)
    }
    else
    {
      pshPriority <- pshPriority %>%
        dplyr::group_by(`PersonalID`) %>%
        dplyr::arrange(dplyr::desc(`CADate`)) %>%
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) %>%
        (ungroup)
      
      pshStaffing <- pshStaffing %>%
        dplyr::group_by(`PersonalID`) %>%
        dplyr::arrange(dplyr::desc(`CADate`)) %>%
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) %>%
        (ungroup)
      
      rrhPriority <- rrhPriority %>%
        dplyr::group_by(`PersonalID`) %>%
        dplyr::arrange(dplyr::desc(`CADate`)) %>%
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) %>%
        (ungroup)
      
      rrhStaffing <- rrhStaffing %>%
        dplyr::group_by(`PersonalID`) %>%
        dplyr::arrange(dplyr::desc(`CADate`)) %>%
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) %>%
        (ungroup)
      
      ceConsolidated <- ceConsolidated %>%
        dplyr::group_by(`PersonalID`) %>%
        dplyr::arrange(dplyr::desc(`CADate`)) %>%
        dplyr::distinct(`PersonalID`, .keep_all = TRUE) %>%
        (ungroup)
    }
  }
  
  bnl <- list(bnlDate = bnlDate,
              pshPriority = pshPriority,
              pshStaffing = pshStaffing,
              rrhPriority = rrhPriority,
              rrhStaffing = rrhStaffing,
              ceConsolidated = ceConsolidated)
  
  bnl <- lapply(bnl, \(x) applyBnlClass(x))
  
  class(bnl) <- c("CE BNL", class(bnl))
  
  return(bnl)
}
