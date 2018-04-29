################################################################################
## Helper functions for dataclean.R
################################################################################

## -- Read in data dictionary; we will use this for variable labels ------------
library(RCurl)

datadict_pF <- postForm(
    "https://redcap.vanderbilt.edu/api/", ## URL for REDCap instance
    token = Sys.getenv("RCTOKEN"),        ## token for specific database
    content = "metadata",                 ## export metadata
    format = "csv"                        ## export as CSV
  )

datadict <- read.csv(
  file = textConnection(datadict_pF),
  na.strings = "",
  stringsAsFactors = FALSE
)

## -- Return a field label given the field name --------------------------------
## Data entry staff may not know what "sga_b" is, but they know what
## "Subject Global Assessment" is. This function expects data dictionaries as
## exported by REDCap defaults.
get_label <- function(
  variable,        ## character; must be in one row of ddict$field_name
  ddict = datadict ## data.frame; expects columns field_name, field_label
){

  ## Checks: ddict must be a data.frame with fields field_name and field_label,
  ## at minimum; field_name must include specified variable
  if(!inherits(ddict, "data.frame")){
    stop("'ddict' must be a data.frame", call. = FALSE)
  }
  if(!all(c("field_name", "field_label") %in% names(ddict))){
    stop("'ddict' must include columns: field_name, field_label", call. = FALSE)
  }
  if(sum(ddict[, "field_name"] == variable, na.rm = TRUE) != 1){
    stop(
      "'variable' must be represented in exactly one row in 'ddict'",
      call. = FALSE
    )
  }

  ## With all those checks out of the way, getting the label is simple:
  return(subset(datadict, field_name == variable)[["field_label"]])
}

## -- Data checking functions --------------------------------------------------
## Each function takes as arguments a data.frame and a set of variable names,
## and returns a data.frame with two columns: ID (study ID + REDCap event) and
## msg (error message)

## -- Helper function for all check_xxxxx() functions: -------------------------
## Takes as arguments:
## - Matrix of 1/NA (problem/not), one column per issue
## - data.frame of issue names (column 1) and error messages (column 2)
## Returns data.frame with one row per error and two columns:
## - id: "record ID;REDCap event name"
## - msg: error message
## If no errors, returns a data.frame w/ 0 rows
create_error_df <- function(
  error_matrix,
  error_codes
){
  ## All values in error_matrix should be 1 or NA
  if(!(all(is.na(error_matrix) | error_matrix == 1))){
    stop("All values in `error_matrix` should be 1 or NA", call. = FALSE)
  }

  ## error_codes should be a matrix or data.frame with two columns; all columns
  ## in error_matrix should be represented in error_codes[, 1]
  if(ncol(error_codes) != 2){
    stop(
      "`error_codes` should have two columns: `variables` and `msgs`",
      call. = FALSE
    )
  }
  if(!all(colnames(error_matrix) %in% error_codes[, 1])){
    stop(
      "All columns in `error_matrix` should be represented by a row in `error_codes`",
      call. = FALSE
    )
  }

  error_data <-
    do.call(
      rbind,
      lapply(
        1:ncol(error_matrix),
        FUN = function(i){
          data.frame(
            id = rownames(error_matrix)[which(!is.na(error_matrix[, i]))],
            msg = rep(
              error_codes[match(colnames(error_matrix)[i], error_codes[, 1]), 2],
              sum(error_matrix[, i], na.rm = TRUE)
            )
          )
        }
      )
    )

  return(error_data)
}

## -- Check for basic missingness ----------------------------------------------
## (when a variable should be present for all records in df)
check_missing <- function(df, variables, ddict = datadict){
  if(nrow(df) > 0){
    ## Create error messages ("Missing" + database label)
    missing_msgs <- unlist(
      lapply(
        variables,
        FUN = function(x){
          paste("Missing", get_label(x, ddict))
        }
      )
    )

    ## Create data frame of column names, error messages
    error_codes <- as.data.frame(cbind(variables, missing_msgs))

    ## Matrix for whether each variable is missing:
    ## column names = variables, row names = ID + REDCap event
    missing_matrix <- do.call(
      cbind,
      lapply(variables, FUN = function(x){ ifelse(is.na(df[,x]), 1, NA) })
    )
    colnames(missing_matrix) <- variables
    rownames(missing_matrix) <-
      paste0(df[,"study_id"], ';', df[,"redcap_event_name"])

    ## Create final data set: One row per column per missing value, with error
    ## message that matches that column name
    ## id = ID + REDCap event; msg = error message ("Missing ...")
    missing_data <- create_error_df(
      error_matrix = missing_matrix, error_codes = error_codes
    )
  } else{
    missing_data <- NULL
  }

  return(missing_data)
}
