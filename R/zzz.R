echochamber <- function()
{
  cli::cli_div()
  cli::cli_h1("{cli::symbol$pointer} Welcome to the ECHO Chamber...")
  cli::cli_text("\n")
  cli::cli_alert_success("{.strong The {.pkg ECHO} package has been loaded & attached.}")
  cli::cli_alert_info("{.emph This also loads & attaches the {.pkg tidyverse} (core), {.pkg magrittr}, {.pkg here}, {.pkg readxl}, and {.pkg encryptr} packages.}", wrap = TRUE)
  cli::cli_h1("")
  cli::cli_text("\n")
  cli::cli_end()
}

.onAttach <- function(libname, pkgname)
{
  suppressPackageStartupMessages(library(here, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE))
  suppressPackageStartupMessages(library(magrittr, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE))
  suppressPackageStartupMessages(library(readxl, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE))
  suppressPackageStartupMessages(library(tidyverse, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE))
  suppressPackageStartupMessages(library(encryptr, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE))
  
  echochamber()
}

.onLoad <- function(libname, pkgname)
{
  rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli())
# rlang::on_load(echochamber())
