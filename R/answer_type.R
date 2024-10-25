#' Answer Type Codes and Labels for HMIS Field Responses
#'
#' If the input is an Answer Type, returns the HMIS CSV Code for that Answer
#' Type. If the input is an HMIS CSV Code, returns the name of the Answer Type.
#' If the input is `"ls"` returns the full list of Answer Type labels.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param answer_code
#' * The "Answer Type" as a string;
#'    - This includes `"unknown"` which will return the unknown answer types: `c(8, 9, 99)` _("Client doesn’t know", "Client prefers not to answer", and "Data not collected".)_.
#' * The HMIS CSV Code as an integer; or
#' * `"ls"` to get the full list of Answer Types.
#' @param full_label If set to `TRUE`, `answerType()` will return the full text
#'   of the answer type rather than the shorthand label (i.e., `"Client prefers
#'   not to answer"` instead of `"noAnswer"`). __Defaults to `FALSE`.__
#' @param .FY The HMIS Data Standards Fiscal Year (entered numerically with two
#'   digits, i.e., `22` for FY22). _**Defaults to FY24.**_ Can be set backwards
#'   up to FY22 for backwards compatibility with older HMIS Extracts.
#'
#' @return Depending on the input: The "Answer Type" label; The HMIS CSV Code;
#'   or the full list of Answer Types.
#' @export
answer_type <- function(answer_code, ..., full_label = FALSE, .FY = 24)
{
  rlang::check_dots_empty()
  
  qAnswer <- switch (as.character(.FY),
                     "22" = qAnswer_FY22,
                     "24" = qAnswer_FY24)
  
  output <- NULL
  
  if (typeof(answer_code) == "double" 
      & answer_code %in% qAnswer$Code 
      & !full_label) 
  { 
    output = "label"
    convertCodeToRow <- which(qAnswer$Code == answer_code)
  } 
  else if (typeof(answer_code) == "double" 
           & answer_code %in% qAnswer$Code 
           & full_label) 
  { 
    output = "title"
    convertCodeToRow <- which(qAnswer$Code == answer_code)
  } 
  else if (typeof(answer_code) == "character" 
           & answer_code %in% qAnswer$Label 
           & (answer_code != "ls" & answer_code != "unknown")) 
  { 
    output = "code"
  } 
  else if (typeof(answer_code) == "character" & answer_code == "ls") 
  { 
    output = "list"
  } 
  else if (typeof(answer_code) == "character" & answer_code == "unknown") 
  { 
    output = "all-codes"
  } 
  else 
  { 
    # stop("ERROR: The input for this function must be 8/9/99, 
    #      clientDoesNotKnow/clientRefused/dataNotCollected, 
    #      \"unknown\", or 
    #      \"ls\" (with or without full_label set to TRUE ... 
    #      it defaults to FALSE).")
    
    # error_msg <- function()
    # {
    #   cli::cli_alert_danger("{.strong ERROR: Invalid Input.}")
    #   cli:: cli_text("The input for this function must be:")
    #   cli::cli_ul()
    #   cli::cli_li("{.field 8}, {.field 9}, {.field 99},")
    #   cli::cli_li("{.field clientDoesNotKnow}, {.field clientRefused}, {.field dataNotCollected},")
    #   cli::cli_li("{.field unknown}, or")
    #   cli::cli_li("{.field ls}.")
    #   cli::cli_text("(with or without {.envvar full_label} set to {.field TRUE} ... it defaults to {.field FALSE}.)\n")
    # }
    
    # cli::cli_abort(message = error_msg())
    
    cli::cli_abort(c("!" = "The input for this function must be:",
                     "*" = "{.val {8}}, {.val {9}}, {.val {99}},",
                     "*" = "{.val clientDoesNotKnow}, {.val clientRefused}, {.val dataNotCollected},",
                     "*" = "{.val unknown}, or",
                     "*" = "{.val ls}.",
                     # "i" = "{.emph (The above is with or without {.arg full_label} set to {.val {TRUE}} ... it defaults to {.val {FALSE}}.)}",
                     "x" = "You entered: {.val {answer_code}}"))
  }
  
  result <- switch(output,
                   "label" = qAnswer$Label[convertCodeToRow],
                   "title" = qAnswer$Title[convertCodeToRow],
                   "code" = qAnswer$Code[which(qAnswer$Label == answer_code)],
                   "list" = qAnswer$Label,
                   "all-codes" = unknown
  )
  
  return(result)
}
