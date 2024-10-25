#' Install or Update the Custom ECHO RStudio Themes
#'
#' This function installs/updates the custom ECHO themes for the editor in
#' RStudio
#'
#' @param theme Type `"light"` or `"dark"` to install the light theme or the
#'   dark theme. You can install both, but they must be installed separately so
#'   you'll have to run this function once for each.
#'
#' @return Installs or updates the selected custom ECHO RStudio theme (`"light"`
#'   or `"dark"`).
#' @export
install_echo_themes <- function(theme)
{
  requireNamespace("here", quietly = TRUE)
  requireNamespace("rstudioapi", quietly = TRUE)
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
  
  whichTheme <- switch(theme,
                       # "light" = stringr::str_c(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Themes/echolight.rstheme"),
                       "light" = paste0(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Themes/echolight.rstheme"),
                       # "dark" = stringr::str_c(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Themes/echodark.rstheme"))
                       "dark" = paste0(reconstwd, "/Dropbox (ECHO)/Research-and-Evaluation-Team/Code/Themes/echodark.rstheme"),
                       cli::cli_abort(c("!" = "Invalid input for the {.arg theme} argument.",
                                        "i" = "{.arg theme} must be {.or {.val {c('light', 'dark')}}}.",
                                        "x" = "You entered: {.val {theme}}")))
  
  return(rstudioapi::addTheme(whichTheme, force = TRUE, apply = TRUE))
}
