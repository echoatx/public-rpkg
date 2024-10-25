# library(here)
# library(tidyverse)
# 
# consolidatedProjects <- c(9211, 9311, 9244)
# # cocProjects <- c(2621, 9166, 9583, 3995, 3994, 9379, 9477, 9451)
# # yhdpProjects <- c(9499, 9504, 9502, 9503, 9500)
# 
# unknown <- c(8, 9, 99)
# 
# pType0 <- data.frame(Code  = c(1:4, 6:14),
#                      Label = c("es", "th", "psh", "outreach", "sso", "other", "safeHaven", "phHousingOnlyProject", "phHousingWServicesNoDisability", "dayShelter", "hp", "rrh", "ce"),
#                      Title = c("Emergency Shelter",
#                                "Transitional Housing",
#                                "Permanent Supportive Housing",
#                                "Street Outreach",
#                                "Supportive Services Only",
#                                "Other",
#                                "Safe Haven",
#                                "PH - Housing Only",
#                                "PH - Housing W/Services (No Disability)",
#                                "Day Shelter",
#                                "Homelessness Prevention",
#                                "Rapid Re-Housing",
#                                "Coordinated Entry"))
# 
# pType_FY22 <- data.frame(Code  = c(1:14),
#                          Label = c("es", "th", "psh", "outreach", "UNKNOWN", "sso", "other", "safeHaven", "phHousingOnlyProject", "phHousingWServicesNoDisability", "dayShelter", "hp", "rrh", "ce"),
#                          Title = c("Emergency Shelter",
#                                    "Transitional Housing",
#                                    "Permanent Supportive Housing",
#                                    "Street Outreach",
#                                    "UNKNOWN",
#                                    "Supportive Services Only",
#                                    "Other",
#                                    "Safe Haven",
#                                    "PH - Housing Only",
#                                    "PH - Housing W/Services (No Disability)",
#                                    "Day Shelter",
#                                    "Homelessness Prevention",
#                                    "Rapid Re-Housing",
#                                    "Coordinated Entry"))
# 
# pType_FY24 <- data.frame(Code  = c(0:14),
#                          Label = c("es-ee", "es-nbn", "th", "psh", "outreach", "UNKNOWN", "sso", "other", "safeHaven", "phHousingOnlyProject", "phHousingWServicesNoDisability", "dayShelter", "hp", "rrh", "ce"),
#                          Title = c("Emergency Shelter – Entry Exit",
#                                    "Emergency Shelter - Night-by-Night",
#                                    "Transitional Housing",
#                                    "PH - Permanent Supportive Housing",
#                                    "Street Outreach",
#                                    NA_character_,
#                                    "Services Only",
#                                    "Other",
#                                    "Safe Haven",
#                                    "PH - Housing Only",
#                                    "PH - Housing with Services (no disability required for entry)",
#                                    "Day Shelter",
#                                    "Homelessness Prevention",
#                                    "PH - Rapid Re-Housing",
#                                    "Coordinated Entry"))
# 
# rrhType_FY24 <- data.frame(Code  = c(1:2),
#                            Label = c("sso", "housing"),
#                            Title = c("RRH: Services Only", "RRH: Housing with or without services"))
# 
# qAnswer_FY22 <- data.frame(Code  = unknown,
#                            Label = c("clientDoesNotKnow", "noAnswer", "nullValue"),
#                            Title = c("Client doesn’t know", "Client Refused", "Data not collected"))
# 
# qAnswer_FY24 <- data.frame(Code  = unknown,
#                            Label = c("clientDoesNotKnow", "noAnswer", "nullValue"),
#                            Title = c("Client doesn’t know", "Client prefers not to answer", "Data not collected"))
# 
# deepRed            <- "#7C0D0E"
# deepBlue           <- "#072A6C"
# deepGreen          <- "#034B03"
# brightYellow       <- "#FFEB2A"
# brightGreen        <- "#66FF00"
# brightRed          <- "#FF160C"
# brightMagenta      <- "#FF00CD"
# deepOrange         <- "#CD3700"
# deepPurple         <- "#540062"
# boldViolet         <- "#710193"
# boldPurple         <- "#710193"
# mediumOrange       <- "#FC6A03"
# lightGrey          <- "#9897A9"
# mediumGrey         <- "#696880"
# deepGrey           <- "#3F3F4E"
# lightBrown         <- "#80471C"
# boldBrown          <- "#4A2511"
# mediumBrown        <- "#371D10"
# deepBrown          <- "#2E1503"
# boldGreen          <- "#028A0F"
# mediumGreen        <- "#32612D"
# brightCyan         <- "#00FFFF"
# lightBlue          <- "#0492C2"
# boldBlue           <- "#0000FF"
# mediumBlue         <- "#1034A6"
# BLACK              <- "#000000"
# WHITE              <- "#FFFFFF"
# echoBlue           <- "#1c75bc"
# echoGrey           <- "#6d6e71"
# echoYellow         <- "#f4b407"
# mediumYellow       <- "#e6cc00"
# mediumRed          <- "#b30000"
# mediumPurple       <- "#734f96"
# mediumYellowOrange <- "#e69b00"
# deepYellowOrange   <- "#e47200"
# 
# # my_palette_OLD <- data.frame(Hex = c(deepRed, ----
# #                                  deepBlue,
# #                                  deepGreen,
# #                                  brightYellow,
# #                                  brightGreen,
# #                                  brightRed,
# #                                  brightMagenta,
# #                                  deepOrange,
# #                                  deepPurple,
# #                                  boldViolet,
# #                                  mediumOrange,
# #                                  lightGrey,
# #                                  mediumGrey,
# #                                  deepGrey,
# #                                  lightBrown,
# #                                  boldBrown,
# #                                  mediumBrown,
# #                                  deepBrown,
# #                                  boldGreen,
# #                                  mediumGreen,
# #                                  brightCyan,
# #                                  lightBlue,
# #                                  boldBlue,
# #                                  mediumBlue,
# #                                  BLACK,
# #                                  WHITE),
# #                          Color = c("deepRed",
# #                                    "deepBlue",
# #                                    "deepGreen",
# #                                    "brightYellow",
# #                                    "brightGreen",
# #                                    "brightRed",
# #                                    "brightMagenta",
# #                                    "deepOrange",
# #                                    "deepPurple",
# #                                    "boldViolet",
# #                                    "mediumOrange",
# #                                    "lightGrey",
# #                                    "mediumGrey",
# #                                    "deepGrey",
# #                                    "lightBrown",
# #                                    "boldBrown",
# #                                    "mediumBrown",
# #                                    "deepBrown",
# #                                    "brightCyan",
# #                                    "boldGreen",
# #                                    "mediumGreen",
# #                                    "lightBlue",
# #                                    "boldBlue",
# #                                    "mediumBlue",
# #                                    "BLACK",
# #                                    "WHITE"))
# 
# # # Color Options Data Frame ----
# # my_palette <- data.frame(Hex = c(deepRed,
# #                                  deepBlue,
# #                                  deepGreen,
# #                                  brightYellow,
# #                                  brightGreen,
# #                                  brightRed,
# #                                  brightMagenta,
# #                                  deepOrange,
# #                                  deepPurple,
# #                                  boldViolet,
# #                                  mediumOrange,
# #                                  lightGrey,
# #                                  mediumGrey,
# #                                  deepGrey,
# #                                  lightBrown,
# #                                  boldBrown,
# #                                  mediumBrown,
# #                                  deepBrown,
# #                                  boldGreen,
# #                                  mediumGreen,
# #                                  brightCyan,
# #                                  lightBlue,
# #                                  boldBlue,
# #                                  mediumBlue,
# #                                  BLACK,
# #                                  WHITE,
# #                                  echoBlue,
# #                                  echoGrey,
# #                                  echoYellow),
# #                          Color = c("deepRed",
# #                                    "deepBlue",
# #                                    "deepGreen",
# #                                    "brightYellow",
# #                                    "brightGreen",
# #                                    "brightRed",
# #                                    "brightMagenta",
# #                                    "deepOrange",
# #                                    "deepPurple",
# #                                    "boldViolet",
# #                                    "mediumOrange",
# #                                    "lightGrey",
# #                                    "mediumGrey",
# #                                    "deepGrey",
# #                                    "lightBrown",
# #                                    "boldBrown",
# #                                    "mediumBrown",
# #                                    "deepBrown",
# #                                    "boldGreen",
# #                                    "mediumGreen",
# #                                    "brightCyan",
# #                                    "lightBlue",
# #                                    "boldBlue",
# #                                    "mediumBlue",
# #                                    "BLACK",
# #                                    "WHITE",
# #                                    "echoBlue",
# #                                    "echoGrey",
# #                                    "echoYellow"))
# 
# # Color Options Data Frame ----
# my_palette <- data.frame(Hex = c(BLACK,
#                                  boldBlue,
#                                  boldBrown,
#                                  boldGreen,
#                                  boldPurple,
#                                  brightCyan,
#                                  brightGreen,
#                                  brightMagenta,
#                                  brightRed,
#                                  brightYellow,
#                                  deepBlue,
#                                  deepBrown,
#                                  deepGreen,
#                                  deepGrey,
#                                  deepOrange,
#                                  deepPurple,
#                                  deepRed,
#                                  deepYellowOrange,
#                                  echoBlue,
#                                  echoGrey,
#                                  echoYellow,
#                                  lightBlue,
#                                  lightBrown,
#                                  lightGrey,
#                                  mediumBlue,
#                                  mediumBrown,
#                                  mediumGreen,
#                                  mediumGrey,
#                                  mediumOrange,
#                                  mediumPurple,
#                                  mediumRed,
#                                  mediumYellow,
#                                  mediumYellowOrange,
#                                  WHITE),
#                          Color = c("BLACK",
#                                    "boldBlue",
#                                    "boldBrown",
#                                    "boldGreen",
#                                    "boldPurple",
#                                    "brightCyan",
#                                    "brightGreen",
#                                    "brightMagenta",
#                                    "brightRed",
#                                    "brightYellow",
#                                    "deepBlue",
#                                    "deepBrown",
#                                    "deepGreen",
#                                    "deepGrey",
#                                    "deepOrange",
#                                    "deepPurple",
#                                    "deepRed",
#                                    "deepYellowOrange",
#                                    "echoBlue",
#                                    "echoGrey",
#                                    "echoYellow",
#                                    "lightBlue",
#                                    "lightBrown",
#                                    "lightGrey",
#                                    "mediumBlue",
#                                    "mediumBrown",
#                                    "mediumGreen",
#                                    "mediumGrey",
#                                    "mediumOrange",
#                                    "mediumPurple",
#                                    "mediumRed",
#                                    "mediumYellow",
#                                    "mediumYellowOrange",
#                                    "WHITE"))
# 
# ph_dest_fy22   <- here("../", "../", "Data", "ph_destinations_hud.csv")
# entry_sit_fy22 <- here("../", "../", "Data", "entry_livingsituation_classification.csv")
# 
# # This is saved in data/FY24_LivingSituations_Destinations_SubsidyTypes.rda
# # FY24_LivingSituations_Destinations_SubsidyTypes <- read_csv("C:/Users/Christopher Murray/Downloads/FY24_LivingSituations-Destinations-SubsidyTypes.csv")
# 
# HUD_CoC_Program_Specific_Data_Elements_Table_FY24 <- tibble::tibble("COC Intervention"              = c("HP", "PSH",  "RRH", "SSO-CE", "SSO (Street Outreach)", "SSO (All Others)", "TH"),
#                                                                 "Income and Sources"            = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Non-Cash Benefits"             = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Health Insurance"              = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Physical Disability"           = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Developmental Disability"      = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Chronic Health Condition"      = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "HIV/AIDS"                      = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Mental Health Disorder"        = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Substance Use Disorder"        = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Domestic Violence"             = c(TRUE,  TRUE,  TRUE,  FALSE,    TRUE,                    TRUE,               TRUE),
#                                                                 "Current Living Situation"      = c(FALSE, FALSE, FALSE, TRUE,     TRUE,                    FALSE,              FALSE),
#                                                                 "Date of Engagement"            = c(FALSE, FALSE, FALSE, FALSE,    TRUE,                    FALSE,              FALSE),
#                                                                 "Coordinated Entry Assessment"  = c(FALSE, FALSE, FALSE, TRUE,     FALSE,                   FALSE,              FALSE),
#                                                                 "Coordinated Entry Event"       = c(FALSE, FALSE, FALSE, TRUE,     FALSE,                   FALSE,              FALSE),
#                                                                 "Moving On Assistance Provided" = c(FALSE, TRUE,  FALSE, FALSE,    FALSE,                   FALSE,              FALSE),
#                                                                 "Translation Assistance Needed" = c(TRUE,  TRUE,  TRUE,  TRUE,     TRUE,                    TRUE,               TRUE),
#                                                                 "Sexual Orientation"            = c(FALSE, TRUE,  FALSE, FALSE,    FALSE,                   FALSE,              FALSE),
#                                                                 "Housing Assessment at Exit"    = c(TRUE,  FALSE, FALSE, FALSE,    FALSE,                   FALSE,              FALSE))
# 
# save(HUD_CoC_Program_Specific_Data_Elements_Table_FY24,
#      file = "data/HUD_CoC_Program_Specific_Data_Elements_Table_FY24.rda")
# 
# entry_udes <- tibble::tibble("UDE" = c("DisablingCondition",
#                                        "EntryDate",
#                                        "RelationshipToHoH",
#                                        "HOH",
#                                        "EnrollmentCoC",
#                                        "LivingSituation",
#                                        "LivingSituationEntry",
#                                        "LengthOfStay",
#                                        "DateToStreetESSH",
#                                        "TimesHomelessPastThreeYears",
#                                        "MonthsHomelessPastThreeYears",
#                                        "MoveInDate"),
#                              "HMIS Extract" = c(rep("entry", times = 2), NA_character_, "entry", NA_character_, NA_character_, rep("entry", times = 6)),
#                              "CSV File" = c(rep("Enrollment.csv", times = 3), NA_character_, rep("Enrollment.csv", times = 2), NA_character_, rep("Enrollment.csv", times = 5)))
# 
# exit_udes <- tibble::tibble("UDE" = c("ExitDate", "Destination"),
#                             "HMIS Extract" = rep("exit", times = 2),
#                             "CSV File" = rep("Exit.csv", times = 2))
# 
# client_udes <- tibble::tibble("UDE" = c("FirstName",
#                                         "LastName",
#                                         "NameDataQuality",
#                                         "SSN",
#                                         "SSNDataQuality",
#                                         "DOB",
#                                         "DOBDataQuality",
#                                         "Race_Ethnicity",
#                                         # "AmIndAKNative",
#                                         # "Asian",
#                                         # "BlackAfAmerican",
#                                         # "HispanicLatinaeo",
#                                         # "MidEastNAfrican",
#                                         # "NativeHIPacific",
#                                         # "White",
#                                         "RaceNone",
#                                         "Gender",
#                                         # "Woman",
#                                         # "Man",
#                                         # "NonBinary",
#                                         # "CulturallySpecific",
#                                         # "Transgender",
#                                         # "Questioning",
#                                         # "DifferentIdentity",
#                                         "GenderNone",
#                                         "VeteranStatus",
#                                         "Veteran_Status"),
#                               "HMIS Extract" = c(rep("client", times = 2), rep(NA_character_, times = 3), "client", NA_character_, "client", NA_character_, "client", NA_character_, NA_character_, "client"),
#                               "CSV File" = c(rep("Client.csv", times = 7), NA_character_, "Client.csv", NA_character_, "Client.csv", "Client.csv", NA_character_))
# 
# HUD_HMIS_Universal_Data_Elements_Table_FY24 <- dplyr::bind_rows(entry_udes, exit_udes, client_udes)
# 
# save(HUD_HMIS_Universal_Data_Elements_Table_FY24,
#      file = "data/HUD_HMIS_Universal_Data_Elements_Table_FY24.rda")
# 
# entry_sit_fy22_table <- read_csv(entry_sit_fy22)
# ph_dest_fy22_table   <- read_csv(ph_dest_fy22)
# 
# save(consolidatedProjects,
#      unknown,
#      pType_FY22,
#      pType_FY24,
#      qAnswer_FY22,
#      qAnswer_FY24,
#      my_palette,
#      # cocProjects,
#      # yhdpProjects,
#      entry_sit_fy22_table,
#      ph_dest_fy22_table,
#      file = "R/sysdata.rda")
