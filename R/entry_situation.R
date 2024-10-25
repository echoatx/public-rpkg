#' Entry Situation HMIS Codes, Labels and Data Frame
#'
#' If the input is an Entry Situation, returns the HMIS CSV Code for that entry
#' situation. If the input is an HMIS CSV Code, returns the Entry Situation
#' Label. If the input is "ls" returns the list of Entry Situation labels. If
#' the input is "load" returns the full Entry Situations data frame.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param input The "Entry Situation Label" as a string; The HMIS CSV Code as an
#'   integer; "ls" to get the full list of Entry Situation Labels; or "load" to
#'   get the full Entry Situation data frame.
#' @param flag OPTIONAL: For the FY24 and later versions of this function,
#'   adding the flag \"category\" will return the category of the input value
#'   (i.e. homeless situation, permanent housing destination, etc.)
#' @param .FY The HMIS data standards fiscal year (defaults to current).
#'
#' @return Depending on the input: The "Entry Situation" label; The HMIS CSV
#'   Code; the list of Entry Situation labels; or the Entry Situation Labels
#'   data frame.
#' @export
entry_situation <- function(input, flag = NULL, ..., .FY = 24)
{
  rlang::check_dots_empty()
  
  fys <- c(22, 24)
  
  fy22_version <- function(.input)
  {
    if (!is.null(flag))
    {
      cli::cli_warn(c("!" = "{.strong The FY22 version of {.fn ph_destination} does not support the {.arg flag} argument.}",
                      "i" = "{.emph {.fn ph_destination} will run disregarding the {.arg flag} argument.}"))
    }
    
    input = .input
    
    output <- NULL
    
    entry_sit_table <- entry_sit_fy22_table
    
    if(is.na(input))
    {
      output <- switch(typeof(input),
                       "character" = NA_real_,
                       "double" = NA_character_,
                       NA)
      
      return(output)
    }
    
    if(input == "load")
    {
      output = "full_table"
    }
    else if(input == "ls")
    {
      output = "list"
      # print_list = print(entry_sit_table$Response)
      
      list_to_print = paste0(entry_sit_table$Value[1], ": ", entry_sit_table$Response[1])
      
      for (n in 2:length(entry_sit_table$Value))
      {
        list_to_print = c(list_to_print, paste0(entry_sit_table$Value[n], ": ", entry_sit_table$Response[n]))
        rm(n)
      }
      
      print_list = print(list_to_print)
    }
    else if((typeof(input) == "character") & (input %in% entry_sit_table$Response))
    {
      output = "csv_code"
      code_row = entry_sit_table$Value[which(entry_sit_table$Response == input)]
    }
    else if(input %in% c(1:37, 99)) # entry_sit_table$Value
    {
      output = "destination_label"
      label_row = entry_sit_table$Response[which(entry_sit_table$Value == input)]
    }
    else
    {
      stop("\n\nERROR: The input for this function must be a CSV Code (1-37 or 99) or the corresponding HMIS Entry Situation Label (e.g. \"Rental by client, with other ongoing housing subsidy,\" (to get one of those values from the other).\n\nOR, \"load\" (to load the full data frame into a variable).\n\n")
    }
    
    result <- switch(output,
                     "full_table" = entry_sit_table,
                     "csv_code" = code_row,
                     "destination_label" = label_row)
    
    return(result)
  }
  
  fy24_version <- function(.input)
  {
    input = .input
    
    output <- NULL
    
    if(is.na(input))
    {
      output <- switch(typeof(input),
                       "character" = NA_real_,
                       "double" = NA_character_,
                       NA)
      
      return(output)
    }
    
    if(input == "load")
    {
      output = "full_table"
    }
    else if(input == "ls")
    {
      output = "list"
      # print_list = print(FY24_LivingSituations_Destinations_SubsidyTypes$Description)
      
      list_to_print = paste0(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value[1], ": ", HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description[1])
      
      for (n in 2:length(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value))
      {
        list_to_print = c(list_to_print, paste0(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value[n], ": ", HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description[n]))
        rm(n)
      }
      
      print_list = print(list_to_print)
    }
    else if((typeof(input) == "character") & (input %in% HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description))
    {
      if (is.null(flag))
      {
        output = "csv_code"
        code_row = HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value[which(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description == input)]
      }
      else if (flag == "category")
      {
        output = "category"
        category_row = HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Classification[which(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description == input)]
      }
      else
      {
        valid_flag <- "category"
        
        cli::cli_abort(c("x" = "{.strong {.val {flag}} is an invalid input for the {.arg flag} argument.}",
                         "i" = "{.emph currently, the only option for flag is: {.val {valid_flag}}}"))
      }
    }
    else if(input %in% HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value) # FY24_LivingSituations_Destinations_SubsidyTypes$Value
    {
      if (is.null(flag))
      {
        output = "destination_label"
        label_row = HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Description[which(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value == input)]
      }
      else if (flag == "category")
      {
        output = "category"
        category_row = HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Classification[which(HUD_LivingSituations_Destinations_SubsidyTypes_FY24$Value == input)]
      }
      else
      {
        valid_flag <- "category"
        
        cli::cli_abort(c("x" = "{.strong {.val {flag}} is an invalid input for the {.arg flag} argument.}",
                         "i" = "{.emph currently, the only option for flag is: {.val {valid_flag}}}"))
      }
    }
    else
    {
      ls_msg <- "ls"
      
      # stop("\n\nERROR: The input for this function must be a CSV Code (1-37 or 99) or the corresponding HMIS Entry Situation Label (e.g. \"Rental by client, with other ongoing housing subsidy,\" (to get one of those values from the other).\n\nOR, \"load\" (to load the full data frame into a variable).\n\n")
      
      cli::cli_abort(c("x" = "{.strong {.val {input}} is an invalid input.}",
                       "!" = "The input for this function must be a valid CSV Code or the corresponding HMIS Entry Situation Label (e.g. {.strong {.emph \"Rental by client, with other ongoing housing subsidy,\"}} (to get one of those values from the other) OR {.strong {.emph \"load\"}} (to load the full data frame into a variable).",
                       "i" = "{.emph You can type {.val {ls_msg}} as the argument for {.fn entry_situation} to see the full list of valid CSV Codes and their text descriptions.}"))
    }
    
    result <- switch(output,
                     "full_table" = HUD_LivingSituations_Destinations_SubsidyTypes_FY24,
                     "csv_code" = code_row,
                     "destination_label" = label_row,
                     "category" = category_row)
    
    return(result)
  }
  
  error_message <- function()
  {
    cli::cli_abort(c("!" = "{.strong The {.arg .FY} argument is invalid.}",
                     "i" = "{.emph {.arg .FY} must be the fiscal year of the data standards: {.or {.val {fys}}.}}",
                     "x" = "You entered {.val {(.FY)}}"))
  }
  
  if (length(input) == 1)
  {
    final_output <- switch(as.character(.FY),
                           "22" = fy22_version(input),
                           "24" = fy24_version(input),
                           error_message())
  }
  else
  {
    final_output <- switch(as.character(.FY),
                           "22" = unlist(purrr::map(input, fy22_version)),
                           "24" = unlist(purrr::map(input, fy24_version)),
                           error_message())
  }
  
  return(final_output)
}
