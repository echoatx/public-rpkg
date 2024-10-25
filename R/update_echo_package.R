#' Update the ECHO Package
#'
#' This function updates the ECHO package. If you have already run
#' `library(ECHO)`, you will have to do so again after using this function
#' because it must unload the ECHO package in order to update it.
#'
#' @return Updates the ECHO package.
#' @export
update_echo_package <- function()
{
  requireNamespace("here", quietly = TRUE)
  # requireNamespace("stringr", quietly = TRUE)
  
  wd <- here::here()
  
  # splitwd <- stringr::str_split(wd, "/")
  splitwd <- strsplit(wd, "/")
  
  # reconstwd <- stringr::str_c(splitwd[[1]][1],
  #                             splitwd[[1]][2],
  #                             splitwd[[1]][3],
  #                             sep = "/")
  
  reconstwd <- paste(splitwd[[1]][1],
                     splitwd[[1]][2],
                     splitwd[[1]][3],
                     sep = "/")
  
  # pkgpath <- stringr::str_c(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Packages/ECHO_0.1.0.tar.gz")
  
  pkgpath <- paste0(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Packages/ECHO_0.1.0.tar.gz")
  
  unloadNamespace("ECHO")
  
  return(install.packages(pkgpath, source = TRUE, repos=NULL))
}
