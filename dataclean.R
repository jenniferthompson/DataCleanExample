################################################################################
## Data cleaning script for example REDCap project
################################################################################

## -- 1. Load or read in data --------------------------------------------------
## Load httr library, for interacting with REDCap API
library(httr)

## My API token is saved in my hidden .Renviron file and kept secret, like this:
## RCTOKEN=mylongredcaptoken12345
##
## You can do that to be able to securely share your code without sharing your
##   information. If you'd rather, you can replace "Sys.getenv('RCTOKEN')" below
##   with your actual token, like `token = 'myredcaptoken12345'`.
##
## For more on using the REDCap API, including setting different options than I
## have set below, see the API documentation on your instance of REDCap. It is
## probably located at a URL like: https://redcap.vanderbilt.edu/api/help/

## Set URL for REDCap instance (yours may be different)
rc_url <- "https://redcap.vanderbilt.edu/api/"

## Export raw data from main database
## You can take two approaches here:
## 1. Read everything in all at once, like a giant spreadsheet, and create
##    subsets of the data (eg, baseline visit vs monthly visits) in R
## 2. Read in forms separately, using event and form names to help you.

## Approach 1 is easier if you're not comfortable with using the API, but can
##  get complicated if your REDCap project is very large.
## I'm going to use Approach 2 here, to show examples of using the API, and
##  leave code for Approach 1 in comments.

## For using the REDCap API in more complex ways, the redcapAPI package may be
## of interest.

## 1. Use API + httr::POST to get data directly from REDCap
main_post <- httr::POST(
  url = rc_url,
  body = list(
    token = Sys.getenv("RCTOKEN"), ## API token gives you permission to get data
    content = "record",            ## export *records*
    format = "csv",                ## export as *CSV*
    rawOrLabel = "label",          ## export factor *labels* vs numeric codes
    exportCheckboxLabel = TRUE     ## export ckbox *labels* vs Unchecked/Checked
  )
)

## 2. main_post has class "response"; read it as a CSV to create a data.frame
main_df <- read.csv(
  text = as.character(main_post),
  stringsAsFactors = FALSE,
  na.strings = ""
)

## Create subsets with data collected at various time points
baseline_df <- subset(main_df, redcap_event_name == "Baseline Visit")
monthly_df <- subset(main_df, redcap_event_name %in% paste("Month", 1:3))
completion_df <- subset(main_df, redcap_event_name == "Study Completion")

## -- 2. Additional Setup ------------------------------------------------------
## Source script of helper functions; keeping them separate -> cleaner code that
## is easier to read/debug
source("dataclean_helpers.R")
