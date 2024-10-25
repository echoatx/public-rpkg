# ECHO 0.1.0

------------------------------------------------------------------------

## ECHO Package Updates (05/31/2024):

### Updated Functions / Primary Changes

-   There is a new `tablify()` function which is like `base::table()`
    except that it formats the output as a tibble and sorts descending
    by the greatest count (it also has `table()`'s `useNA = "ifany"` set
    by default, which can be overridden). The count of `NA`s comes last
    by default, but it can be set to fall in order based on the count of
    the NA category. This function uses tidyverse-style piping and the
    `.data` pronoun, so if you pipe the tibble/dataframe you want to
    table a variable for into this function, it should prompt you to
    auto-fill non-quoted variable names based on the input.
    -   Example usage: `hmis$client %>% tablify(Race_Ethnicity)` is
        equivalent to the following:

        ``` r
        as_tibble(as.data.frame(table(hmis$client$Race_Ethnicity, useNA = "ifany"))) %>%
          rename(`Count` = `Freq`) %>% 
          group_by(is.na(Race_Ethnicity)) %>%
          arrange(desc(Count), .by_group = TRUE) %>%
          ungroup() %>%
          select(-3)) # Gets rid of the column added by the
                      # conditional group_by() above
        ```

    -   `NA` will be last in the table above regardless of the `Count`
        of that category, but to make `NA` fall in descending order with
        everything else you can use:
        `hmis$client %>% tablify(Race_Ethnicity, na_last = FALSE)`.

    -   If you want to leave `NA`s out completely you can use:
        `hmis$client %>% tablify(Race_Ethnicity, ignore_na = TRUE)`.

------------------------------------------------------------------------

## ECHO Package Updates (05/31/2024):

### Updated Functions / Primary Changes

-   The `shortcut()` function now has options to take you to the
    **Systems Advancment** Dropbox folder, and several locations inside
    that folder. These all have the prefix `"sa ..."` Options include:
    -   `shortcut("sa dropbox")` Goes to the *Systems Advancement* (or
        *Housing for Health* – it will auto-detect what it's named on
        your system) Dropbox folder.
    -   `shortcut("sa data")` Goes to the *Systems Advancement/**Data***
        subfolder.
        -   `shortcut("sa capds")` goes to the *"/Data/**CAPDS**"*
            subfolder.
        -   `shortcut("sa aging")` goes to the *"/Data/**Commission on
            Aging**"* subfolder.
        -   `shortcut("sa connxus")` goes to the *"/Data/**Connxus**"*
            subfolder.
        -   `shortcut("sa cuc")` goes to the *"/Data/**CuC - Medical
            Complexity and CA**"* subfolder.
        -   `shortcut("sa hiv")` goes to the *"/Data/**HIV-AIDS Pop**"*
            subfolder.
        -   `shortcut("sa mortality")` goes to the
            *"/Data/**Mortality**"* subfolder.

------------------------------------------------------------------------

## ECHO Package Updates (03/06/2024):

### Updated Functions / Primary Changes

-   The `load_hmis()` and `load_bnl()` functions now load *encrypted*
    `.rda` files. You must install the **encryptr** R package to use
    them (run: `install.packages("encryptr")`). Separate setup of a
    private key file as well as knowledge of the password is also
    required. This ensures the security of confidential information so
    that it is never saved unencrypted.
    -   For both of the above functions, if loading a previous HMIS
        extract or BNL, it is no longer necessary to give a full
        filepath, whether by typing it, using `here()` or `shortcut()`.
        Instead, you only need to type the dat in quotes, e.g.
        `load_hmis("2023-12-28")` would load the December 2023 HMIS
        extrcat, and `load_hmis("2023-03-30", .FY = 22)` would load the
        March 2023 HMIS extract.
    -   Note that we currently need to specify `.FY = 22` as an
        additional argument in `load_hmis()` when using ***pre
        10/01/23*** HMIS extracts.

### New Functions

-   The new `%unlock%` function will unlock any columns of data that
    have been encrypted in a dataset. Example usage:
    `hmis_full$client %unlock% c("FirstName", "LastName")`.

-   The new `join_extract_files()` function will quickly join the
    `entry`, `exit`, `client`, and `project` files of the given HMIS
    extract. If run by itself it will default to calling
    load_hmis("newest"), and using that extract. Otherwise, you can
    specify which HMIS extract to use. Example usage:

    -   `entry_exit_client_project <- join_extract_files()`

        *-Or-*

        ``` r
        hmis_dec2023 <- load_hmis("2023-12-28")

        # These all do the same thing:

        entry_exit_client_project <- join_extract_files(hmis_dec2023)

        entry_exit_client_project <- hmis_dec2023 %>%
          join_extract_files()

          entry_exit_client_project <- hmis_dec2023 |>
          join_extract_files()
        ```

------------------------------------------------------------------------

## First version of ECHO Package Website (12/06/2023).

-   added the `echo_help()` function, which will open the ECHO package
    help website. You can also add a function name (as a "string") to
    open that specific help page: i.e., `echo_help("make_universe")`
    will open the help page for the `make_universe()` function.

------------------------------------------------------------------------

## ECHO Package Updates (12/01/2023):

-   The **checkPIT()** function is now called `check_pit()`. It works
    the same, but has been renamed to follow standard naming
    conventions.

-   The original **make_enrollmentUniverse()** function that made the
    enrollment universe (the result of which we used to make the client
    universe) has been replaced with `make_universe()`. You can tell it:
    make_universe(universe_type = “enrollment”),
    make_universe(universe_type = “client”), or
    make_universe(universe_type = “both”).

-   `is_vsp()` will return true if the entered ProjectID is a VSP. You
    can further specify is_vsp(ProjectID, alternate_database_users_only
    = TRUE) in which case it will only return true for VSPs who use an
    alternate database instead of HMIS (like SAFE).

-   The system overview functions used for the dashboard have been added
    to the ECHO package and no longer need to be sourced in from
    wherever they’re floating in Dropbox. They are prefixed with
    **“hrs\_...”** so there’s `hrs_returns_demographics()`,
    `hrs_unsheltered_snapshot()`, `hrs_sheltered_snapshot()`, and
    `hrs_enrollments()`.

-   Most of the ECHO package functions that use the HMIS extract and
    would autoload it if it was in your environment and called “hmis”
    have been updated to:

    -   Still do that, BUT, if the HMIS Extract is not in your
        environment and called “hmis” then the functions will search
        your environment for any HMIS Extract (regardless of what it’s
        named) and use that (it will inform you in the console that it
        did this, and which extract it used), OR if there is no HMIS
        Extract in your environment, it will automatically/invisibly
        call load_hmis(“newest”) in the background and use that to do
        it’s work (again, it will inform you in the console that it did
        this and tell you the date of the “newest” extract it used).

-   The saved HMIS extracts starting 10/26/2023 and going forward now
    have the following new features:

    -   In the “client” file:
        -   Race_Ethnicity & Gender are now factors.
        -   VeteranStatus has been split into is_Veteran (True/False),
            and Veteran_Status (a factor)
        -   Age_Group has been added back in (as a factor).
    -   In the “entry” file:
        -   Age_at_Entry has been added.
    -   In the “project” file:
        -   RRH_Subtype has been added as a factor. It is NA for any non
            RRH project. For RRH Projects it will be “Housing”, “SSO”,
            or “Unspecified (Missing Data)”.
        -   Target_Population has been added as a factor. It will be
            “DV”, “HIV”, “None” or “NA”.
        -   HMIS_Participant has been added as a TRUE/FALSE based on
            whether a project is set as a current HMIS participant.
        -   HMIS_Status has been added to expand upon HMIS_Participant.
            It will classify whether a project’s HMIS participation
            status is “Current”, “Former”, “Never”, or “Unknown”.
        -   CE_Participant has been added as a TRUE/FALSE based on
            whether a project is set as currently receiving CE referrals
            (for some reason all projects got reset to non participating
            after 10/26, but I’ve spoken to Joseph and he’s looking into
            it).
        -   CE_Status has been added to expand upon CE_Participant. It
            will classify whether a project’s CE participation status is
            “Current”, “Former”, “Never”, or “Unknown”.
