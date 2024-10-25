#' View Client Information by SPID
#'
#' Search for entries, exits, and client demographics by ServicePoint ID number.
#'
#' @param csvextract the list returned by the [import_hmis()] function.
#' @param uid the client's SPID.
#'
#' @return the `entry`, `client`, and `exit` data frames filtered by SPID.
#' @export
view_spid <- function(csvextract, uid)
{
    require(glue)

    csvextract$entry %>%
        filter(PersonalID == uid) %>%
        view(glue("Entries_SPID={uid}"))

    csvextract$client %>%
        filter(PersonalID == uid) %>%
        view(glue("Client_SPID={uid}"))

    csvextract$exit %>%
        filter(PersonalID == uid) %>%
        view(glue("Exits_SPID={uid}"))
}
