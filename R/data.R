#' FY24 HUD HMIS Living Situation & Destination Option List with Rental Subsidy Types
#'
#' This provides the CSV codes and text labels for the living situations/ exit destinations and rental subsidy types for the
#' FY24 HMIS data standards ...
#'
#' @format ## `HUD_LivingSituations_Destinations_SubsidyTypes_FY24` A data frame with 43 rows and 3 columns:
#' \describe{
#'   \item{Value}{HMIS CSV code}
#'   \item{Description}{Text explanation of the CSV code}
#'   \item{Classification}{HUD's classification of the value's category (permanent housing, homelessness, etc.)}
#'   ...
#' }
#' @source <https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf>
"HUD_LivingSituations_Destinations_SubsidyTypes_FY24"

#' CoC Program Specific Data Elements Table (FY24)
#'
#' This table identifies which PSDEs are required for each CoC Component ...
#'
#' @format ## `HUD_CoC_Program_Specific_Data_Elements_Table_FY24` A data frame with 7 rows and 19 columns:
#' \describe{
#'   \item{CoC Intervention}{String: The Project Type (and sub-category, if applicable).}
#'   \item{Income and Sources}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Non-Cash Benefits}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Health Insurance}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Physical Disability}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Developmental Disability}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Chronic Health Condition}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{HIV/AIDS}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Mental Health Disorder}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Substance Use Disorder}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Domestic Violence}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Current Living Situation}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Date of Engagement}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Coordinated Entry Assessment}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Coordinated Entry Event}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Moving On Assistance Provided}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Translation Assistance Needed}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Sexual Orientation}{Boolean: is this element required for the CoC Intervention in this row?}
#'   \item{Housing Assessment at Exit}{Boolean: is this element required for the CoC Intervention in this row?}
#'   ...
#' }
#' @source <https://files.hudexchange.info/resources/documents/CoC-Program-HMIS-Manual-2024.pdf>
"HUD_CoC_Program_Specific_Data_Elements_Table_FY24"

#' FY24 HUD HMIS Universal Data Elements Table
#'
#' This table lists the items that are considered UDEs and identifies where they can be found in the HMIS Extract and/or the
#' HMIS data export .csv files ...
#'
#' @format ## `HUD_HMIS_Universal_Data_Elements_Table_FY24` A data frame with 27 rows and 3 columns:
#' \describe{
#'   \item{UDE}{The Name of the UDE}
#'   \item{HMIS Extract}{Where in the HMIS Extract does this UDE appear as a column?}
#'   \item{CSV File}{In which .csv file in the HMIS data export does this UDE appear as a column?}
#'   ...
#' }
#' @source <https://files.hudexchange.info/resources/documents/HMIS-Data-Dictionary-2024.pdf>
"HUD_HMIS_Universal_Data_Elements_Table_FY24"
