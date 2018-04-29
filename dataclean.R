################################################################################
## Data cleaning script for example REDCap project
################################################################################

## -- 1. Load or read in data --------------------------------------------------
## Load RCurl library, for interacting with REDCap API
library(RCurl)

## My API token is saved in my hidden .Renviron file and kept secret, like this:
## RCTOKEN=mylongredcaptoken12345
##
## You can do that to be able to securely share your code without sharing your
##   information. If you'd rather, you can replace "Sys.getenv('RCTOKEN')" below
##   with your actual token, like `token = 'myredcaptoken12345'`.
##
## For more on using the REDCap API, including setting different options than I
## have set below, see the documentation:
##   https://redcap.vanderbilt.edu/api/help/
##   (replace redcap.vanderbilt.edu with your REDCap location)

## Set URL for REDCap instance (yours will be different)
rc_url <- "https://redcap.vanderbilt.edu/api/"

## Export raw data from main database
main_pF <- RCurl::postForm(
  uri = rc_url,                  ## URL for REDCap instance, specified above
  token = Sys.getenv("RCTOKEN"), ## token for specific database
  content = "record",            ## export records
  format = "csv",                ## export as CSV
  rawOrLabel = "label",          ## export factor *labels* vs numeric codes
  exportCheckboxLabel = TRUE,    ## export checkbox labels vs Unchecked/Checked
  exportDataAccessGroups = FALSE ## don't need data access groups
)

## RCurl::postForm() returns a character string. Read it in as a CSV.
main_df <- read.csv(
  file = textConnection(main_pF),
  na.strings = "",
  stringsAsFactors = FALSE
)

## -- 2. Additional Setup ------------------------------------------------------
## Source script of helper functions; keeping them separate -> cleaner code that
## is easier to read/debug
source("dataclean_helpers.R")
