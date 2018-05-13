################################################################################
## Helper functions for dataclean.R
################################################################################

library(httr) ## for working with REDCap API

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

## -- Read in data dictionary --------------------------------------------------
## -- We will use this for variable labels, limits -----------------------------
ddict_post <- httr::POST(
  url = "https://redcap.vanderbilt.edu/api/",
  body = list(
    token = Sys.getenv("RCTOKEN"), ## API token gives you permission to get data
    content = "metadata",          ## export *metadata* (data dictionary)
    format = "csv"                 ## export as *CSV*
  )
)

datadict <- post_to_df(ddict_post)

## -- Return a field label given the field name --------------------------------
## Data entry staff may not know what "sga_b" is, but they know what
## "Subject Global Assessment" is.
get_label <- function(
  variable,         ## character; must be in one row of ddict$field_name
  ddict = datadict, ## data.frame
  ## Default column names are based on REDCap data dictionary exports;
  ## you can supply your own data.frame with your desired column names
  cname_vname = "field_name", ## colname that contains variable ("sga_b")
  cname_label = "field_label" ## colname that contains label ("Subj Global Asmt")
){

  ## Checks: ddict must be a data.frame with fields field_name and field_label,
  ## at minimum; field_name must include specified variable
  if(!inherits(ddict, "data.frame")){
    stop("'ddict' must be a data.frame", call. = FALSE)
  }
  if(!all(c(cname_vname, cname_label) %in% names(ddict))){
    stop(
      "Columns of 'ddict' must include `cname_vname`, `cname_label`",
      call. = FALSE
    )
  }
  if(sum(ddict[, cname_vname] == variable, na.rm = TRUE) != 1){
    stop(
      "'variable' must be represented in exactly one row in 'ddict'",
      call. = FALSE
    )
  }

  ## With all those checks out of the way, getting the label is simple:
  return(
    as.character(ddict[ddict[, cname_vname] == variable, cname_label])
  )
}

## -- Data checking functions --------------------------------------------------
## Each function takes as arguments a data.frame and a set of variable names,
## and returns a data.frame with two columns: ID (study ID + REDCap event) and
## msg (error message)

## -- Helper function for all check_xxxxx() functions: -------------------------
## Takes as arguments:
## - Matrix of T/F or 1/0 (problem/not), one column per issue
## - data.frame of issue names (column 1) and error messages (column 2)
## Returns data.frame with one row per error and two columns:
## - id: "record ID;REDCap event name"
## - msg: error message (eg, "Missing age", "Height lower than suggested limit")
## If no errors, returns a data.frame w/ 0 rows
create_error_df <- function(
  error_matrix,
  error_codes
){
  ## All values in error_matrix should be logical or 1/0; none missing
  if(!(is.logical(error_matrix) | all(error_matrix %in% 0:1)) |
     any(is.na(error_matrix))){
    stop(
      "`error_matrix` should be logical or all 0/1 with no missing values",
      call. = FALSE
    )
  }

  ## error_codes should be a matrix or data.frame with two columns; all columns
  ## in error_matrix should be represented in error_codes[, 1]
  if(ncol(error_codes) != 2){
    stop(
      "`error_codes` should have two columns: `code` and `msg`",
      call. = FALSE
    )
  }
  if(!all(colnames(error_matrix) %in% error_codes[, 1])){
    stop(
      "All columns in `error_matrix` must be represented by a row in `error_codes`",
      call. = FALSE
    )
  }

  error_data <-
    do.call(
      rbind,
      lapply(
        ## For every column of error_matrix...
        1:ncol(error_matrix),
        FUN = function(i){
          ## Create a data.frame with all actual error messages within the col
          data.frame(
            id = rownames(error_matrix)[which(error_matrix[, i])],
            msg = rep(
              error_codes[match(colnames(error_matrix)[i], error_codes[, 1]), 2],
              sum(error_matrix[, i])
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
    error_codes <- as.data.frame(cbind(paste0("miss_", variables), missing_msgs))
    names(error_codes) <- c("code", "msg")

    ## Matrix for whether each variable is missing:
    ## column names = variables, row names = ID + REDCap event
    missing_matrix <- do.call(
      cbind,
      lapply(variables, FUN = function(x){ is.na(df[,x]) })
    )
    colnames(missing_matrix) <- paste0("miss_", variables)
    rownames(missing_matrix) <-
      paste0(df[,"study_id"], "; ", df[,"redcap_event_name"])

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

## -- Check whether numeric variables fall within specified limits -------------
## If using a REDCap data dictionary, these can be the text_validation_min/max
## fields; you can also specify your own limits that were not built into the
## REDCap database design.
## df_limits is a data.frame that has, at minimum, one column for the variable
## name (column 1) and one column each for minimum and maximum values.
## Everything in `variables` should be represented by a row in df_limits.

check_limits_numeric <- function(
  df,                                ## data.frame to check
  variables,                         ## character vector of variables to check
  ddict = datadict,                  ## data.frame containing variable labels
  ## These defaults assume you are using a REDCap data dictionary, but you can
  ## pass your own dataset too.
  df_limits = datadict,              ## data.frame containing limits
  cname_min = "text_validation_min", ## Column name for minimum limit
  cname_max = "text_validation_max"  ## Column name for maximum limit
){
  ## Checks
  if(!inherits(df, "data.frame")){
    stop("`df` must be a data.frame", call. = FALSE)
  }
  if(!all(variables %in% names(df))){
    stop("All elements of `variables` must be columns in `df`", call. = FALSE)
  }
  if(!inherits(df_limits, "data.frame") |
     !all(c(cname_min, cname_max) %in% names(df_limits))){
    stop("`df_limits` must be a data.frame with columns `cname_min`, `cname_max`",
         call. = FALSE)
  }
  if(!all(variables %in% df_limits[, 1])){
    stop(
      "All elements of `variables` must be represented by a row in `df_limits`",
      call. = FALSE
    )
  }
  ## Warning if anything listed in `variables` is not a numeric field
  not_numeric <- setdiff(
    variables,
    subset(
      datadict,
      text_validation_type_or_show_slider_number %in% c("number", "integer")
    )$field_name
  )
  variables <- setdiff(variables, not_numeric)

  if(length(not_numeric) > 0){
    warning(
      sprintf(
        "The following `variables` are not numeric: %s",
        paste(not_numeric, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  if(nrow(df) > 0){
    ## Create components for error messages in a data.frame:
    limits_codes <- data.frame(
      variables = variables,
      var_label = unlist(lapply(variables, FUN = get_label)),
      min_value = unlist(lapply(
        variables, FUN = function(x){
          as.numeric(df_limits[df_limits[, 1] == x, cname_min])
        }
      )),
      max_value = unlist(lapply(
        variables, FUN = function(x){
          as.numeric(df_limits[df_limits[, 1] == x, cname_max])
        }
      ))
    )

    ## If both min and max are missing, there are no limits to check
    no_limits <-
      subset(limits_codes, is.na(min_value) & is.na(max_value))$variables
    variables <- setdiff(variables, no_limits)

    if(length(no_limits) > 0){
      warning(
        sprintf(
          "The following `variables` have no limits in `df_limits`: %s",
          paste(no_limits, collapse = "; ")
        ),
        call. = FALSE
      )
    }

    limits_codes <- subset(limits_codes, !(variables %in% no_limits))

    ## Combine pieces to create error messages
    limits_codes$msg <- with(limits_codes, {
      ifelse(
        !is.na(min_value) & !is.na(max_value),
        sprintf(
          "%s is not between recommended limits of %s and %s; please correct or confirm accuracy",
          var_label, min_value, max_value
        ),
      ifelse(
        !is.na(min_value),
        sprintf(
          "%s is lower than recommended limit of %s; please correct or confirm accuracy",
          var_label, min_value
        ),
      ifelse(
        !is.na(max_value),
        sprintf(
          "%s is higher than recommended limit of %s; please correct or confirm accuracy",
          var_label, max_value
        )
      )))
    })

    ## Replace NA min/max with -/+Inf
    limits_codes$min_value <- with(limits_codes, {
      ifelse(is.na(min_value), -Inf, min_value)
    })
    limits_codes$max_value <- with(limits_codes, {
      ifelse(is.na(max_value), Inf, max_value)
    })

    ## Matrix for whether each variable is outside limits:
    ## column names = variables, row names = ID + REDCap event

    ## Function to check values for one variable
    outside_limits <- function(df, variable, min, max){
      ifelse(is.na(df[, variable]), FALSE,
             df[, variable] < min | df[, variable] > max)
    }

    ## Apply that function over all variables
    limits_matrix <- mapply(
      FUN = outside_limits,
      variable = variables,
      min = limits_codes$min_value,
      max = limits_codes$max_value,
      MoreArgs = list(df = df)
    )
    colnames(limits_matrix) <- variables
    rownames(limits_matrix) <-
      paste0(df[,"study_id"], "; ", df[,"redcap_event_name"])

    ## Create final data set: One row per column per missing value, with error
    ## message that matches that column name
    ## id = ID + REDCap event; msg = error message
    limits_codes <- subset(limits_codes, select = c(variables, msg))
    names(limits_codes) <- c("code", "msg")

    limits_data <- create_error_df(
      error_matrix = limits_matrix,
      error_codes = limits_codes
    )
  } else{
    limits_data <- NULL
  }

  return(limits_data)
}
