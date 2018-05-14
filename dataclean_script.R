#' ---
#' title: "R + REDCap Example Data Cleaning Workflow"
#' author: "Jennifer Thompson, MPH"
#' date: "June 4, 2018"
#' ---
#'
#' This script is intended as a full example script, to be used as a template
#' for your own study. Happy data cleaning!
#'
#' All code for data export assumes that the user has rights to use the REDCap
#' API for data export, and that a working API token is stored in the
#' `.Renviron` file in the working directory, in the format
#'
#'  RCTOKEN=manylettersandnumbers
#'
#' If you don't want to use the `.Renviron` file, you can replace
#' `Sys.getenv("RCTOKEN")` with `"manylettersandnumbers"`. If you do this, do
#' not share your code - it is very important to keep your API token secret.
#'
#' The code will use several helper functions, sourced from
#' `dataclean_helpers.R`, assumed to be in the same working directory.
#'
#' This script intentionally uses base R (with the exception of the `httr`
#' package), to maximize adoption and minimize dependencies. Should you care to
#' refactor with the [tidyverse](tidyverse.org) or other packages, alternate
#' versions are welcome!
#'
#' Please see github.com/jenniferthompson/DataCleanExample for further
#' information and a full tutorial.
#'
#' This example uses a sample REDCap database for a three-month longitudinal
#' study of adult patients taking a dietary supplement and measuring creatinine,
#' HDL and LDL cholesterol, and weight over time. (Sample database is adapted
#' with thanks from REDCap's project templates.)
#'
#' # Step 1: Use REDCap API to Export Raw Data
#'
#' You must have appropriate user rights for your database in order to request
#' an API token. Once you have the correct user rights, log into the REDCap
#' project. On the lefthand side under `Applications`, you will see a line for
#' `API and API Playground`. Click here, then on the button titled
#' `Generate API token`.
#'
#' Once your token is generated, **never share it with anyone**. It gives you
#' permission and ability to access research data, and should be kept protected
#' at all times.
#'
#' For more information on the REDCap API, please see `Project Setup -> Other
#' Functionality` within an existing REDCap project. REDCap's API Playground can
#' be useful in figuring out which options to include in the `body` argument of
#' `httr::POST`. (Do note that as of the time of this writing, the example R
#' code from the Playground uses `RCurl`; `httr` is currently more commonly used
#' and thus it is easier to find documentation and assistance for it.)
#'
#' More information on the `httr` package, for working with APIs, can be found
#' in the documentation and vignettes, linked from
#' [CRAN](https://cran.r-project.org/package=httr). (Everything else in
#' this example script is intentionally in base R to avoid dependencies.)
#'
## ----httr_setup----------------------------------------------------------
library(httr)

## Source helper functions (dataclean_helpers.R should be stored in this working
## directory)
## Function post_to_df(), which is useful in API code, is stored here, along
## with other helper functions we'll use later
source("dataclean_helpers.R")

## Set URL for REDCap instance (yours may be different)
rc_url <- "https://redcap.vanderbilt.edu/api/"

## ----baseline_data------------------------------------------------------------
## Data from baseline visit only: Demographics and Baseline Data forms
baseline_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN"), ## API token gives you permission
    content = "record",            ## export *records*
    format = "csv",                ## export as *CSV*
    forms = "demographics,baseline_data", ## forms
    fields = c("study_id"),               ## additional fields
    events = "baseline_visit_arm_1",      ## baseline visit event only
    rawOrLabel = "label",      ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE ## export ckbox *labels* vs Unchecked/Checked
  )
)

## baseline_post has class "response"; read it as a CSV to create a data.frame
baseline_df <- post_to_df(baseline_post)

## Double-check if you like!
## baseline_df

#' The `rawOrLabel = "label"` and `exportCheckboxLabel = TRUE` elements in the
#' `body` argument of `POST` are personal preference. I set these to export
#' labels because I usually find that it is more clear - ie, it is easier to
#' figure out what `sex == Male` is doing than `sex == 1`. Depending on your
#' database and your purposes, you may want to change these to use the raw
#' numeric codes - for example, if you have fields with very long labels.
#'
## ----monthly_data-------------------------------------------------------------
## Data from all monthly visits
monthly_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN"),           ## API token gives you permission
    content = "record",                      ## export *records*
    format = "csv",                          ## export as *CSV*
    forms = "monthly_data",                  ## forms
    fields = c("study_id"),                  ## additional fields
    events = paste(sprintf("month_%s_arm_1", 1:3), collapse = ","),
      ## all 3 monthly visit events
    rawOrLabel = "label",      ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE ## export ckbox *labels* vs Unchecked/Checked
  )
)
monthly_df <- post_to_df(monthly_post)

## Double-check if you like!
## monthly_df

## ----completion_data----------------------------------------------------------
## Data from study completion visits
completion_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN"),     ## API token gives you permission
    content = "record",                ## export *records*
    format = "csv",                    ## export as *CSV*
    forms = "completion_data",         ## form
    fields = c("study_id"),            ## additional fields
    events = "study_completion_arm_1", ## study completion event
    rawOrLabel = "label",      ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE ## export ckbox *labels* vs Unchecked/Checked
  )
)
completion_df <- post_to_df(completion_post)

## Double-check if you like!
## completion_df

#' Note: The data dictionary is read in (to object `datadict`) as part of
#' dataclean_helpers.R.
#'
#' # Step 2: Create a data.frame of All Issues
#'
#' This sounds deceptively straightforward, but is the most involved part of this
#' process. Our goal is to create a single data frame of all potential problems in
#' our REDCap project as of the date we run the script, which can then be imported
#' into our documentation database(s) (more on that later) so that the issues can
#' be resolved.
#'
#' Typically, I work form by form. This keeps the code in manageable chunks and
#' makes it easier to both write initially and debug and maintain as the study
#' progresses.
#'
#' Remember: REDCap has many data validation capabilities built in -
#' **use them!** The added value for this data cleaning script comes from the
#' more complex data checks that are possible here and not within REDCap itself.
#'
## ----datamgmt-----------------------------------------------------------------
## -- Helper functions; we're doing the same thing a lot -----------------------
## Another reason to make sure all your dates have the same formatting!
date_ymd <- function(x){ as.Date(x, format = "%Y-%m-%d") }
diff_days <- function(x, y){ as.numeric(difftime(x, y, units = "days")) }

## Baseline data
baseline_df$dob <- date_ymd(baseline_df$dob)
baseline_df$date_enrolled <- date_ymd(baseline_df$date_enrolled)

## Dates out of characters
baseline_df$date_visit_b <- date_ymd(baseline_df$date_visit_b)
baseline_df$date_supplement_dispensed <-
  date_ymd(baseline_df$date_supplement_dispensed)

## Calculate days between baseline visit and other milestones
## Creating these variables will make our code later much more readable
baseline_df$days_visit_consent <- with(baseline_df, {
  diff_days(date_visit_b, date_enrolled) })
baseline_df$days_visit_supp <- with(baseline_df, {
  diff_days(date_supplement_dispensed, date_visit_b) })

## Monthly data
monthly_df$date_visit_m <- date_ymd(monthly_df$date_visit_m)

## Study completion data
completion_df$completed_date <- date_ymd(completion_df$completed_date)
completion_df$studywd_date <- date_ymd(completion_df$studywd_date)
completion_df$death_date <- date_ymd(completion_df$death_date)

## ----dummy_data----------------------------------------------------------
## Create "dummy" datasets that have a record for every patient + time
## This helps us check things like "pt X is missing Monthly Visit 2"
## Knowing the study design is key!

## Lists of all unique IDs, events
## In our case, every patient should have a record for every month, unless they
##  have died or withdrawn, along with baseline data & a study completion form.
##  So we create a dummy dataset with all five events for each patient. Later,
##  we'll also create an indicator variable for whether the patient has died or
##  withdrawn at that point (and therefore should *not* have data entered).
all_ids <- sort(unique(c(
  baseline_df$study_id, monthly_df$study_id, completion_df$study_id
)))
all_events <- c("Baseline Visit", paste("Month", 1:3), "Study Completion")

dummy_df <- data.frame(
  study_id = rep(all_ids, each = length(all_events)),
  redcap_event_name = rep(all_events, length(all_ids))
)

## Merge on relevant dates for each event: enrollment, completion, death,
##  withdrawal - we will use these to help clean things in each form
dates_everyone <- merge(
  subset(baseline_df, select = c(study_id, date_enrolled)),
  subset(completion_df, select = c(study_id, completed_date, death_date, studywd_date)),
  by = "study_id", all = TRUE
)

dummy_df <- merge(
  dummy_df,
  dates_everyone,
  by = "study_id", all.x = TRUE
)
dummy_df <- merge(
  dummy_df,
  subset(monthly_df, select = c(study_id, redcap_event_name, date_visit_m)),
  by = c("study_id", "redcap_event_name"), all.x = TRUE
)

## Merge subsets of dummy data onto individual datasets, to make sure every
## dataset has all the patients/records it's supposed to
## This step also adds dates from baseline and completion forms onto forms where
##  they're needed (eg, adds date of study withdrawal onto monthly form, so we
##  can tell whether they *should* have a monthly visit or not)
baseline_df <- merge(
  subset(
    dummy_df,
    redcap_event_name == "Baseline Visit",
    select = c(study_id, redcap_event_name)
  ),
  baseline_df,
  by = c("study_id", "redcap_event_name"), all.x = TRUE
)

monthly_df <- merge(
  subset(
    dummy_df,
    redcap_event_name %in% paste("Month", 1:3),
    select = -date_visit_m
  ),
  monthly_df,
  by = c("study_id", "redcap_event_name"), all.x = TRUE
)

completion_df <- merge(
  subset(
    dummy_df,
    redcap_event_name == "Study Completion",
    select = c(study_id, redcap_event_name, date_enrolled)
  ),
  completion_df,
  by = c("study_id", "redcap_event_name"), all.x = TRUE
)

## Add an indicator to the monthly form for whether the patient should have a
## form filled out this month. (They should *not* if they have died or withdrawn
## before [month] * 30 days.)
monthly_df$month_num <-
  with(monthly_df, as.numeric(substr(redcap_event_name, 7, 7)))
monthly_df$days_enrolled_actual <- with(monthly_df, {
  diff_days(date_visit_m, date_enrolled) })
monthly_df$date_visit_planned <- with(monthly_df, {
  date_enrolled + (month_num * 30) })
monthly_df$out_of_study <- with(monthly_df, {
  (!is.na(death_date) & death_date < date_visit_planned) |
    (!is.na(studywd_date) & studywd_date < date_visit_planned)
})

## Indicator: Does the patient have any monthly visit data this month?
## Use the data dictionary to save typing!
monthly_vars <- subset(datadict, form_name == "monthly_data")$field_name
## `consent_reminder` is just a reminder field, is not exported
monthly_vars <- setdiff(monthly_vars, "consent_reminder")

monthly_df$any_data <- rowSums(!is.na(monthly_df[, monthly_vars])) > 0

## Create a variable for *last* month's visit, to check hospitalization dates
## Note: There may be better ways to do this in base (maybe split/unsplit?).
## I would usually use dplyr:
## monthly_df %>%
##   group_by(study_id) %>%
##   mutate(last_visit = lag(date_visit_m))
monthly_df$last_visit <- as.Date(
  unlist(
    lapply(
      unique(monthly_df$study_id),
      FUN = function(i){
        df <- subset(monthly_df, study_id == i)
        c(NA, df$date_visit_m[1:(nrow(df) - 1)])
      })
  ),
  origin = "1970-1-1"
)

#' ## Demographics Form
#' For our demographics form, we want to check the following:
#'
#' - Study ID should always be an integer (no letters or special characters)
#' - These fields should always be present:
#'     - Date of consent
#'     - Consent form
#'     - All contact information
#'     - Phone
#'     - Mood
#'     - Statins
#' - Postal code should be properly formatted (this example is from Japan)
#' - Date of birth should be between 18 and 110 years before consent
#' - If the patient is female, whether she has ever given birth should be entered
#' - If the patient has given birth, the number of births should be entered
#' - If no activity questions are marked, study staff should confirm this (it might be OK, but it is unusual and should be checked)
#' - If the patient is marked as being on statins, at least one specific statin
#' should be checked; if the patient is marked as *not* being on statins, *no*
#' statins should be checked
#' - Height and weight should both be present and within soft limits set in the database
#'
#' ### 2. Custom error checking (+ demo of the process)
#'
#' Our next step is to create a `n x 2` data.frame where each row represents one
#' potential problem; the first column is an error code; and the second column
#' is the corresponding error message. For example, our error code might be
#' `id_format`, and the corresponding error message might be `Study ID should be an
#' integer with at most four digits`.
#'
## ----demog_codes---------------------------------------------------------
## -- Create error codes + corresponding messages for all issues *except* ------
## -- fields that are simply missing or should fall within specified limits ----

## Codes: Short, like variable names
## Messages: As clear as possible to the human reader
demog_codes <- data.frame(
  code = c(
    "id_format", "postcode_format", "dob_limits", "birth_yn", "birth_num",
    "no_activity", "which_statin", "no_statins"
  ),
  msg = c(
    "Study ID should be an integer with at most four digits",
    "Postal code should be formatted properly",
    "Date of birth should be within 18 and 110 years prior to consent",
    "If patient is female, whether she has given birth should be marked",
    "If patient has given birth, number of births should be present",
    "This patient has no activities marked; please confirm or correct",
    "Patient is marked as taking statins, but no specific statins checked",
    "Patient is marked as not taking statins, but at least one statin is checked"
  )
)

#' The second step is to create a matrix with # rows =
#' `nrow(data frame we're checking)`, and # columns =
#' `nrow(data frame of error codes)`. Every column is
#' logical, indicating whether or not this problem exists at this row in the
#' data.frame. For many issues, we'll do this manually.
#'
## ----demog_setup---------------------------------------------------------
## Preliminary setup: Create empty matrix to hold all potential issues
## Rows = # rows in baseline_df
## Columns = # potential issues
## Everything starts off FALSE
demog_issues <- matrix(FALSE, ncol = nrow(demog_codes), nrow = nrow(baseline_df))
colnames(demog_issues) <- demog_codes$code
rownames(demog_issues) <- with(baseline_df, {
  paste(study_id, redcap_event_name, sep = '; ') })

## ----demog_issues--------------------------------------------------------
## -- Determine true/false for each potential issue ----------------------------
## Study ID, postal codes are checked using regular expressions; set rows which
## do *not* meet the regex to TRUE
## (For regexes, it's easier to do this by using the first-column notation here,
## because of how grep() works. For other custom checks, however, I find it more
## readable to say `df_issues[, "issue_name"] <- [condition]`.)
demog_issues[
  grep("^\\d{1,4}$", baseline_df$study_id, invert = TRUE), "id_format"
  ] <- TRUE
demog_issues[
  grep("^\\d{3}-\\d{4}$", baseline_df$postal_code, invert = TRUE),
  "postcode_format"
  ] <- TRUE

## Date of birth: Should be within 18 and 110 years of consent date
demog_issues[, "dob_limits"] <- with(baseline_df, {
  !is.na(date_enrolled) & !is.na(dob) &
    (diff_days(date_enrolled, dob) < (18 * 365.25) |
       diff_days(date_enrolled, dob) > (110 * 365.25))
})

## Birth questions
demog_issues[, "birth_yn"] <- with(baseline_df, {
  !is.na(gender) & gender == "Female" & is.na(given_birth) })
demog_issues[, "birth_num"] <- with(baseline_df, {
  !is.na(given_birth) & given_birth == "Yes" & is.na(num_children)
})

## Activity questions: If all are missing, ask site to confirm
demog_issues[, "no_activity"] <- rowSums(
  !is.na(
    baseline_df[,grep("^(gym|aerobics|eat|drink)\\_[0-6]$", names(baseline_df))]
  )
) == 0

## Statins
demog_issues[, "which_statin"] <- baseline_df$any_statins == "Yes" &
    rowSums(!is.na(baseline_df[, paste0("which_statins_", 1:9)])) == 0
demog_issues[, "no_statins"] <- baseline_df$any_statins == "No" &
    rowSums(!is.na(baseline_df[, paste0("which_statins_", 1:9)])) > 0

#' The next step is to take our matrix, look for only the actual errors
#' (`TRUE` values), and combine them into a data.frame with one row per actual
#' error. Because we'll be doing this frequently, there is a helper function,
#' `create_error_df()`, in `dataclean_helpers.R` to do it for us. The function
#' takes as inputs our `error_codes` and `error_matrix`, and returns a
#' data.frame with one row per actual problem.
#'
## ----demog_df------------------------------------------------------------
demog_errors <- create_error_df(
  error_matrix = demog_issues, error_codes = demog_codes
)

demog_errors

#' ### 3. Simple checks: Are these fields present?
#'
#' Because these checks are so common, I have a helper function, `check_missing()`,
#' that lets us do it easily. It follows the same basic steps as we did for our
#' custom checks, but every check is `is.na(df[, x])`, and every error message
#' is "Missing [field label]." It takes as inputs our data.frame, a character
#' string of variables to check, and the data dictionary, from which it gets
#' variable labels to create the full error message.
#'
## ----demog_missing-------------------------------------------------------
demog_missing <- check_missing(
  df = baseline_df,
  variables = c(
    "date_enrolled", "patient_document", "family_name", "given_name",
    "street_address", "city_prefecture", "postal_code", "phone", "email",
    "dob", "gender", "mood", "any_statins", "height", "weight", "bmi"
  ),
  ddict = datadict
)

demog_missing

#' ### 4. Check for Values Outside Limits
#'
#' We have several numeric fields that we want to make sure fall within
#' reasonable limits (or if they do not, that the values are confirmed to be
#' correct). `check_limits_numeric()` function in `dataclean_helpers.R` follows
#' a very similar process to `check_missing()`, but you can specify minimum and
#' maximum allowed values for each variable, using either the limits set in the
#' REDCap data dictionary or a custom data.frame.
#'
#' Since we want to use the default values, using this function looks very
#' similar to `check_missing()`:
#'
## ----demog_limits--------------------------------------------------------
demog_limits <- check_limits_numeric(
  df = baseline_df,
  variables = c("height", "weight"),
  ddict = datadict
)

demog_limits

## ----demog_combine-------------------------------------------------------
demog_final <- do.call(rbind, list(demog_missing, demog_limits, demog_errors))
demog_final$form <- "Demographics"

## demog_final

#' ## Baseline Visit Form
#'
#' We'll be making sure that:
#'
#' - Date of visit is recorded, and is on or within a week of consent
#' - Creatinine present, within 0.3 - 9
#' - HDL cholesterol present, within 20-100
#' - LDL cholesterol present, within 30-300
#' - Weight present, within 35-200
#' - Two plasma, two serum variables answered
#' - Subject Global Assessment answered
#' - Date patient begins supplement answered, after date of baseline visit
#'
#' ### Things aren't missing
#'
#' We actually want to check that *everything* is present. We can save ourselves
#' some typing by taking advantage of the data dictionary.
#'
## ----baseline_missing----------------------------------------------------
## Get all the variables in the Baseline Visit form
baseline_vars <- subset(datadict, form_name == "baseline_data")$field_name

## Now see if they're missing
baseline_missing <- check_missing(
  df = baseline_df,
  variables = baseline_vars
)

## baseline_missing

#' ### Things aren't extremely large or small
#'
#' We have several lab values and patient characteristics measured at this
#' visit. We'll create a data frame with our own limits vs using the data
#' dictionary.
#'
## ----baseline_limits-----------------------------------------------------
baseline_limit_vars <- paste0(c("creat", "hdl", "ldl", "drywt"), "_b")

baseline_limit_df <- data.frame(
  var = baseline_limit_vars,
  min_val = c(0.5, 20, 30, 35),
  max_val = c(10, 100, 300, 200)
)

baseline_limits <- check_limits_numeric(
  ## These arguments are similar to previous example
  df = baseline_df,
  variables = baseline_limit_vars,
  ddict = datadict,
  ## These arguments are how we supply our own limits:
  ##  df_limits = data.frame; cname_min, cname_max = columns in df_limits with
  ##  minimum, maximum limits
  df_limits = baseline_limit_df,
  cname_min = "min_val",
  cname_max = "max_val"
)

## baseline_limits

## ----baseline_issues----------------------------------------------------------
## -- Create error codes + corresponding messages for additional issues --------
baseline_codes <- data.frame(
  code = c("visit_consent", "visit_supp"),
  msg = c(
    "Baseline visit should occur within one week of consent",
    "Supplement start date should be on or within one week of baseline visit"
  )
)

## -- Issues matrix: in this case, two columns ---------------------------------
## Rows = # rows in baseline_df
## Columns = # potential issues
## Everything starts off FALSE
baseline_issues <- matrix(
  FALSE, ncol = nrow(baseline_codes), nrow = nrow(baseline_df)
)
colnames(baseline_issues) <- baseline_codes$code
rownames(baseline_issues) <- with(baseline_df, {
  paste(study_id, redcap_event_name, sep = '; ') })

## -- Determine true/false for each potential issue ----------------------------
baseline_issues[, "visit_consent"] <- with(baseline_df, {
  !is.na(days_visit_consent) &
    (days_visit_consent < 0 | days_visit_consent > 6)
})
baseline_issues[, "visit_supp"] <- with(baseline_df, {
  !is.na(days_visit_supp) & (days_visit_supp < 0 | days_visit_supp > 6)
})

baseline_errors <- create_error_df(baseline_issues, baseline_codes)

## ----baseline_combine----------------------------------------------------
## -- Combine all baseline errors into a final data.frame ----------------------
baseline_final <- do.call(
  rbind, list(baseline_missing, baseline_limits, baseline_errors)
)
baseline_final$form <- "Baseline Data"

#' ## Monthly Visit
#'
#' We'll check at every visit:
#'
#' - At least some data exists for each month, unless patient has withdrawn or died
#' - Date of visit is recorded and is at within X months (+/- one week) of enrollment
#' - Creatinine present, within 0.3 - 9
#' - HDL cholesterol present, within 20-100
#' - LDL cholesterol present, within 30-300
#' - Weight present, within 35-200
#' - Number of treatments missed (out of 8) present
#' - % compliance present
#' - number of treatments should be between 0-8 and “match” % compliance
#' - Hospitalization (y/n) present
#' - If hospitalized, cause, dates, discharge summary questions present
#' - If hospitalized, dates should be between last and current visit
#' - Date of hospital discharge should be after date of admission
#'
## ----monthly_errors------------------------------------------------------
## -- Custom checks ------------------------------------------------------------
## Create error codes + corresponding messages
monthly_codes <- data.frame(
  code = c(
    "incomplete_visit", "extra_visit", "visit_date", "trt_comp",
    "hosp_cause_miss", "hosp_adm_miss", "hosp_dis_miss", "hosp_sum_miss",
    "hosp_lastvisit", "hosp_adm_dis"
  ),
  msg = c(
    "Patient has not died or withdrawn, but has no data for this month",
    "Patient died or withdrew before scheduled monthly visit, but has data",
    "Date of monthly visit not within X months (+/- one week) after enrollment",
    "Number of treatments missed does not match % compliance",
    "Patient hospitalized since last visit, but missing cause of hospitalization",
    "Patient hospitalized since last visit, but missing admission date",
    "Patient hospitalized since last visit, but missing discharge date",
    "Patient hospitalized since last visit, but missing whether summary filed",
    "Hospital admission date is prior to last visit",
    "Hospital discharge date is prior to admission date"
  )
)

monthly_issues <- matrix(
  FALSE, ncol = nrow(monthly_codes), nrow = nrow(monthly_df)
)
colnames(monthly_issues) <- monthly_codes$code
rownames(monthly_issues) <- with(monthly_df, {
  paste(study_id, redcap_event_name, sep = '; ') })

## Determine true/false for each potential issue
monthly_issues[, "incomplete_visit"] <- with(monthly_df, {
  !out_of_study & !any_data })
monthly_issues[, "extra_visit"] <- with(monthly_df, out_of_study & any_data)
monthly_issues[, "visit_date"] <- with(monthly_df, {
  !is.na(days_enrolled_actual) &
    (days_enrolled_actual < ((month_num * 30) - 7) |
       days_enrolled_actual > ((month_num * 30) + 7))
})
monthly_issues[, "trt_comp"] <- with(monthly_df, {
  !is.na(trt_missed) & !is.na(compliance) & (
    (compliance == "99-75 percent" & trt_missed > 2) |
    (compliance == "74-50 percent" & !(trt_missed %in% 3:4)) |
    (compliance == "49-25 percent" & !(trt_missed %in% 5:6)) |
    (compliance == "0-24 percent" & !(trt_missed %in% 7:8))
  )
})
monthly_issues[, "hosp_cause_miss"] <- with(monthly_df, {
  !is.na(hosp_yn) & hosp_yn == "Yes" & is.na(hosp_cause) })
monthly_issues[, "hosp_adm_miss"] <- with(monthly_df, {
  !is.na(hosp_yn) & hosp_yn == "Yes" & is.na(hosp_adm) })
monthly_issues[, "hosp_dis_miss"] <- with(monthly_df, {
  !is.na(hosp_yn) & hosp_yn == "Yes" & is.na(hosp_dis) })
monthly_issues[, "hosp_sum_miss"] <- with(monthly_df, {
  !is.na(hosp_yn) & hosp_yn == "Yes" & is.na(hosp_summary_binder) })
monthly_issues[, "hosp_lastvisit"] <- with(monthly_df, {
  !is.na(hosp_adm) & !is.na(last_visit) & hosp_adm < last_visit })
monthly_issues[, "hosp_adm_dis"] <- with(monthly_df, {
  !is.na(hosp_adm) & !is.na(hosp_dis) & hosp_dis < hosp_adm })

monthly_errors <- create_error_df(monthly_issues, monthly_codes)

## -- Missingness checks -------------------------------------------------------
monthly_vars <- c(
  paste0(c("creat", "hdl", "ldl", "drywt"), "_m"),
  "trt_missed", "compliance", "hosp_yn"
)
monthly_missing <- check_missing(
  df = subset(monthly_df, !out_of_study),
    ## takes out visits that shouldn't be there
  variables = monthly_vars
)

## -- Limit checks -------------------------------------------------------------
monthly_limits_vars <-
  c(paste0(c("creat", "hdl", "ldl", "drywt"), "_m"), "trt_missed")

monthly_limit_df <- data.frame(
  var = monthly_limits_vars,
  min_val = c(0.3, 20, 30, 35, 0),
  max_val = c(9, 100, 300, 200, 8)
)

monthly_limits <- check_limits_numeric(
  df = subset(monthly_df, !out_of_study),
  variables = monthly_limits_vars,
  ddict = datadict,
  df_limits = monthly_limit_df, cname_min = "min_val", cname_max = "max_val"
)

## -- Combine all checks -------------------------------------------------------
monthly_final <- do.call(
  rbind, list(monthly_missing, monthly_limits, monthly_errors)
)
monthly_final$form <- "Monthly Data"

#' ## Study Completion Form
#'
#' Each patient should have a study completion form filled out, at the end of
#' the study or at death or withdrawal. We'll check:
#'
#' - Study completion (yes/no), date are recorded
#' - Whether patient withdrew or died are recorded
#' - If the patient withdrew, whether date and reason are recorded
#' - If the patient died, whether date and cause are recorded
#'
## ----completion_errors---------------------------------------------------
## -- All checks ---------------------------------------------------------------
## Since there's only one variable that should be present for everyone, we'll
## just include it in the main section rather than doing a separate call for
## check_missing().

## Create error codes + corresponding messages
completion_codes <- data.frame(
  code = c(
    "miss_comp", "comp_date", "wd_date", "wd_reason", "died_date", "died_cause"
  ),
  msg = c(
    "Missing Has patient completed study?",
    "Patient completed study, but missing date",
    "Patient withdrew, but missing date",
    "Patient withdrew, but missing reason",
    "Patient died, but missing date",
    "Patient died, but missing cause"
  )
)

completion_issues <- matrix(
  FALSE, ncol = nrow(completion_codes), nrow = nrow(completion_df)
)
colnames(completion_issues) <- completion_codes$code
rownames(completion_issues) <- with(completion_df, {
  paste(study_id, redcap_event_name, sep = '; ') })

## Determine true/false for each potential issue
completion_issues[, "miss_comp"] <- is.na(completion_df$completed_study)
completion_issues[, "comp_date"] <- with(completion_df, {
  !is.na(completed_study) & completed_study == "Yes" & is.na(completed_date) })
completion_issues[, "wd_date"] <- with(completion_df, {
  !is.na(studywd_yn) & studywd_yn == "Yes" & is.na(studywd_date) })
completion_issues[, "wd_reason"] <- with(completion_df, {
  !is.na(studywd_yn) & studywd_yn == "Yes" & is.na(studywd_reason) })
completion_issues[, "died_date"] <- with(completion_df, {
  !is.na(death_yn) & death_yn == "Yes" & is.na(death_date) })
completion_issues[, "died_cause"] <- with(completion_df, {
  !is.na(death_yn) & death_yn == "Yes" & is.na(death_cause) })

completion_final <- create_error_df(completion_issues, completion_codes)
completion_final$form <- "Completion Data"


#' Now we combine all our data issues into one data.frame and prepare it to
#' upload to the documentation database.
#'
## ----combine_issues------------------------------------------------------
all_issues <- do.call(
  rbind, list(demog_final, baseline_final, monthly_final, completion_final)
)

## Separate study_id, redcap_event_name from id column
id_event <- strsplit(as.character(all_issues$id), "; ")
all_issues$study_id <- unlist(lapply(id_event, FUN = function(x){ x[[1]] }))
all_issues$event <-
  unlist(lapply(id_event, FUN = function(x){ x[[2]] }))
all_issues <- all_issues[order(all_issues$study_id), ]

## Create a unique query number for all issues for each patient
all_issues$querynum <- unlist(
  lapply(
    unique(all_issues$study_id),
    FUN = function(i){ 1:sum(all_issues$study_id == i) }
  )
)

## Add today's date, create total query ID (study_id + date + querynum)
all_issues$date_query <- format(Sys.Date(), "%Y-%m-%d")
all_issues$queryid <-
  with(all_issues, paste(study_id, date_query, querynum, sep = "_"))

## -- Final version to upload: Query ID, pt ID, date, form, event, issue -------
all_issues <- subset(
  all_issues,
  select = c(queryid, study_id, date_query, form, event, msg)
)

#' # Step 3: Remove Unfixable Queries
#'
#' Sometimes, issues with the data are known but cannot be resolved. Therefore,
#' an important step in our process is the **documentation** of each issue. This
#' both serves as a record of why the original data changed and enables us to
#' **remove** issues which are unfixable or correct from future data cleans.
#' Note that since the documentation is a separate REDCap project, you will need
#' a separate API token. Mine is saved in my `.Renviron` file as `DOCTOKEN`.
#'
## ----already_checked, eval = FALSE---------------------------------------
## Documentation of queries already checked
doc_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("DOCTOKEN"),
    content = "record",
    format = "csv",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE
  )
)
doc_df <- post_to_df(doc_post)

## Double-check if you like!
## doc_df

#' We now need to merge `all_issues` with our previously documented issues and
#' remove the potential errors which are either unfixable or actually correct.
#' To be removed from future data cleans, queries must be marked as accurate in
#' the field `corrected`, or must be confirmed by the coordinating center as
#' being unfixable in the field `cc_conclusion`.
#'
## ----flag_queries--------------------------------------------------------
doc_df$remove <- with(doc_df, {
  (!is.na(corrected) &
     corrected == "Value confirmed correct (for accuracy queries ONLY)") |
    (!is.na(cc_conclusion) &
       cc_conclusion == "Yes (it is permanently unfixable)")
})

subset(doc_df, remove)

## ----merge_fixed---------------------------------------------------------
all_issues <- merge(
  all_issues, subset(doc_df, select = c(study_id, form, event, msg, remove)),
  by = c("study_id", "form", "event", "msg"),
  all.x = TRUE, all.y = FALSE
)

## ----remove_documented---------------------------------------------------
upload_issues <- subset(all_issues, !(!is.na(remove) & remove == TRUE))
write.csv(
  upload_issues, sprintf("issues_%s.csv", Sys.Date()), row.names = FALSE
)

#' From here, you're ready to upload the new round of queries to your
#' documentation database and begin the process again. Happy cleaning!
