#' Open the ECHO Package Help Website
#'
#' This function opens the help website for the ECHO Package.
#'
#' @return Depending on the input: The "Answer Type" label; The HMIS CSV Code;
#'   or the full list of Answer Types.
#' @export
echo_help <- function(function_name = NULL)
{
  requireNamespace("fs", quietly = TRUE)
  
  if (is.null(function_name))
  {
    fs::file_show(ECHO::shortcut("re dropbox",
                                 "Code",
                                 "Packages",
                                 "ECHO Package Help Site",
                                 ".site_directory",
                                 "index.html"))
  }
  else
  {
    
    
    fs::file_show(ECHO::shortcut("re dropbox",
                                 "Code",
                                 "Packages",
                                 "ECHO Package Help Site",
                                 ".site_directory",
                                 "reference",
                                 paste0(function_name, ".html")))
  }
}
