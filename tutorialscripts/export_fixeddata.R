################################################################################
## Export all current data in Data Clean Example database by form, as described
## in dataclean.Rmd; store in fixeddata/
################################################################################

library(httr)
rc_url <- "https://redcap.vanderbilt.edu/api/"

## -- Helper function to create data.frames from `response` objects created ----
## -- by httr::POST ------------------------------------------------------------
post_to_df <- function(post_obj){
  ## Use read.csv to create a data.frame from the response object
  tmp <- read.csv(
    text = as.character(post_obj),
    stringsAsFactors = FALSE,
    na.strings = ""
  )

  ## REDCap exports many underscores in checkbox variable names; cut down to 1
  names(tmp) <- gsub("_+", "_", names(tmp))

  return(tmp)
}

## -- Demographic/baseline data ------------------------------------------------
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

baseline_df <- post_to_df(baseline_post)

## -- Monthly visit data -------------------------------------------------------
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

## -- Study completion data ----------------------------------------------------
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

## -- Send all datasets as CSV files to fixedata/ ------------------------------
export_csv <- function(df_name){
  write.csv(
    get(df_name),
    file = sprintf("fixeddata/%s.csv", df_name),
    row.names = FALSE, na = ""
  )
}

export_csv("baseline_df")
export_csv("monthly_df")
export_csv("completion_df")
