################################################################################
## Read in **corrected data**, do data management to continue tutorial
################################################################################

## -- Same as before, but using API tokens for *corrected* example database ----
source("dataclean_helpers.R")
rc_url <- "https://redcap.med.osaka-cu.ac.jp/redcap/api/"

## ----datamgmt-----------------------------------------------------------------
## -- Helper functions; we're doing the same thing a lot -----------------------
## Another reason to make sure all your dates have the same formatting!
date_ymd <- function(x){ as.Date(x, format = "%Y-%m-%d") }
diff_days <- function(x, y){ as.numeric(difftime(x, y, units = "days")) }

## ----baseline_data------------------------------------------------------------
## Data from baseline visit only: Demographics and Baseline Data forms
baseline_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN_OCU_CORR"), ## API token gives you permission
    content = "record",                     ## export *records*
    format = "csv",                         ## export as *CSV*
    forms = "demographics,baseline_data",   ## forms
    fields = c("study_id"),                 ## additional fields
    events = "baseline_visit_arm_1",        ## baseline visit event only
    rawOrLabel = "label",      ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE ## export ckbox *labels* vs Unchecked/Checked
  )
)

## baseline_post has class "response"; read it as a CSV to create a data.frame
baseline_df <- post_to_df(baseline_post)

## Data from all monthly visits
monthly_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN_OCU_CORR"),  ## API token gives you permission
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

## Data from study completion visits
completion_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN_OCU_CORR"), ## API token gives you permission
    content = "record",                     ## export *records*
    format = "csv",                         ## export as *CSV*
    forms = "completion_data",              ## form
    fields = c("study_id"),                 ## additional fields
    events = "study_completion_arm_1",      ## study completion event
    rawOrLabel = "label",      ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE ## export ckbox *labels* vs Unchecked/Checked
  )
)
completion_df <- post_to_df(completion_post)

## ----datamgmt-----------------------------------------------------------------
## Repeat data management on updated data
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

## Versions if using API to export: options given download checkboxes as
##  `day of the week` or NA
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

# ## Versions if using manually exported CSV "labels" format: exports checkboxes
# ##  as `Checked` or `Unchecked`
# ## Activity
# demog_issues[, "no_activity"] <- rowSums(
#   baseline_df[,grep("^(gym|aerobics|eat|drink)\\_[0-6]$", names(baseline_df))] == "Checked"
# ) == 0
#
# ## Statins
# demog_issues[, "which_statin"] <- baseline_df$any_statins == "Yes" &
#     rowSums(baseline_df[, paste0("which_statins_", 1:9)] == "Checked") == 0
# demog_issues[, "no_statins"] <- baseline_df$any_statins == "No" &
#     rowSums(baseline_df[, paste0("which_statins_", 1:9)] == "Checked") > 0

demog_issues


## ----demog_df------------------------------------------------------------
demog_errors <- create_error_df(
  error_matrix = demog_issues, error_codes = demog_codes
)

demog_errors


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


## ----head_datadict-------------------------------------------------------
subset(
  datadict,
  field_name %in% c("study_id", "dob", "height", "weight"),
  select = c(
    field_name, field_label, field_type,
    text_validation_type_or_show_slider_number,
    text_validation_min, text_validation_max
  )
)


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


## ----baseline_missing----------------------------------------------------
## Get all the variables in the Baseline Visit form
baseline_vars <- subset(datadict, form_name == "baseline_data")$field_name

## Now see if they're missing
baseline_missing <- check_missing(
  df = baseline_df,
  variables = baseline_vars
)

## baseline_missing


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


## ----baseline_issues-----------------------------------------------------
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

## Write out to CSV to more easily look at it - will store this in repo
write.csv(all_issues, "querydata/updated_issues.csv", row.names = FALSE)
