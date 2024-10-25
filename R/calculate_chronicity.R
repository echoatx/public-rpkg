#' Add a `Chronic_Homelessness_Status_At_Entry` Column to a Data Frame
#'
#' Adds a .csv `Chronic_Homelessness_Status_At_Entry` column to a tibble or data
#' frame using [dplyr::mutate()].
#'
#' @param .data A tibble or data frame that is, corresponds to, or has all the
#'   data from the `client` tibble in the `HMIS Data Export`.
#'
#' @return the tibble used as the function input, with a new column called
#'   `Chronic_Homelessness_Status_At_Entry`.
#' @export
calculate_chronicity_at_entry <- function(.data)
{
  requireNamespace("dplyr", quietly = TRUE)
  
  which_HH_column <- ifelse("HOH" %in% names(.data), "HOH", "RelationshipToHoH")
  
  # deselect_dob_at_end <- FALSE
  # 
  # if (!"DOB" %in% names(.data))
  # {
  #   deselect_dob_at_end <- TRUE
  #   
  #   # if (try(get("client", pos = parent.frame())) != "try-error")
  #   if ("client" %in% names(parent.frame()))
  #   {
  #     .data <- .data |> 
  #       dplyr::left_join(dplyr::select(client, PersonalID, DOB))
  #   }
  #   else
  #   {
  #     .data <- .data |> 
  #       dplyr::left_join(dplyr::select(load_hmis("newest")$client, PersonalID, DOB))
  #   }
  # }
  
  .data |> 
    dplyr::mutate(Chronic_Homelessness_Status_At_Entry = factor(dplyr::if_else({{which_HH_column}} == 1 | floor(as.double(EntryDate - DOB) / 365) >= 18,
                                                                               dplyr::case_match(DisablingCondition,
                                                                                                 1 ~ dplyr::if_else(ProjectType %in% c(0, 1, 4, 8),
                                                                                                                    dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                     TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                         MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                         MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                         MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                         .default = "missing"),
                                                                                                                                     TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                     TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                     TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                     .default = "missing"),
                                                                                                                    dplyr::case_when(LivingSituationEntry %in% c(100:199) ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                                             TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                                                 MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                                                 MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                 MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                 .default = "missing"),
                                                                                                                                                                                             TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                                             TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                             TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                             .default = "missing"),
                                                                                                                                     LivingSituationEntry %in% c(200:299) ~ dplyr::case_when(LOSUnderThreshold == 1 ~ dplyr::case_when(PreviousStreetESSH == 1 ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                                                                                                                                  TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                                                                                                                                      MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                                                                                                                                      MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                                                                      MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                                                                      .default = "missing"),
                                                                                                                                                                                                                                                                                  TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                                                                                                                                  TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                  TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                  .default = "missing"),
                                                                                                                                                                                                                                       PreviousStreetESSH == 0 ~ "NO",
                                                                                                                                                                                                                                       .default = "missing"),
                                                                                                                                                                                             LOSUnderThreshold == 0 ~ "NO",
                                                                                                                                                                                             .default = "missing"),
                                                                                                                                     LivingSituationEntry %in% c(0:99, 300:499) ~ dplyr::case_when(LOSUnderThreshold == 1 ~ dplyr::case_when(PreviousStreetESSH == 1 ~ dplyr::case_when(EntryDate - DateToStreetESSH >= 365 ~ "YES",
                                                                                                                                                                                                                                                                                        TimesHomelessPastThreeYears == 4 ~ dplyr::case_when(MonthsHomelessPastThreeYears %in% c(112:113) ~ "YES",
                                                                                                                                                                                                                                                                                                                                            MonthsHomelessPastThreeYears %in% c(101:111) ~ "NO",
                                                                                                                                                                                                                                                                                                                                            MonthsHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                                                                            MonthsHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                                                                            .default = "missing"),
                                                                                                                                                                                                                                                                                        TimesHomelessPastThreeYears %in% c(1:3) ~ "NO",
                                                                                                                                                                                                                                                                                        TimesHomelessPastThreeYears %in% c(8:9) ~ "DK/PNTA",
                                                                                                                                                                                                                                                                                        TimesHomelessPastThreeYears == 99 ~ "missing",
                                                                                                                                                                                                                                                                                        .default = "missing"),
                                                                                                                                                                                                                                             PreviousStreetESSH == 0 ~ "NO",
                                                                                                                                                                                                                                             .default = "missing"),
                                                                                                                                                                                                   LOSUnderThreshold == 0 ~ "NO",
                                                                                                                                                                                                   .default = "missing"),
                                                                                                                                     is.na(LivingSituationEntry) ~ "missing",
                                                                                                                                     .default = NA)),
                                                                                                 0 ~ "NO",
                                                                                                 8 ~ "DK/PNTA",
                                                                                                 9 ~ "DK/PNTA",
                                                                                                 99 ~ "missing",
                                                                                                 .default = "missing"),
                                                                               NA),
                                                                levels = c("YES", "NO", "DK/PNTA", "missing")))
  # if (deselect_dob_at_end)
  # {
  #   .data <- .data |> 
  #     dplyr::select(-DOB)
  # }
  # 
  # return(.data)
}
