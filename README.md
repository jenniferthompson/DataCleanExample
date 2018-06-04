# Data Cleaning Workflow for Prospective Clinical Research, Using R + REDCap

This repo contains a tutorial and related files which describe the continual
data cleaning process used by the [Vanderbilt CIBS Center](http://icudelirium.org)
for prospective clinical research.

The tutorial refers to a sample [REDCap](https://projectredcap.org/) database
for a three-month longitudinal study of adult patients taking a dietary
supplement and measuring creatinine, HDL and LDL cholesterol, and weight over
time. (Sample database is adapted with thanks from REDCap's project templates.)

### File Structures

#### Primary Resources

- [**Tutorial (PDF)**](https://github.com/jenniferthompson/DataCleanExample/blob/master/dataclean.pdf): Contains code, links, and prose describing our entire process, from
study and database design through study completion.
- [Example R script](https://github.com/jenniferthompson/DataCleanExample/blob/master/dataclean_script.R), extracted from the tutorial, which
    1. Allows you to code along with the tutorial more easily
    1. Serves as a base for developing your own data cleaning script
- [Script of helper functions for data cleaning](https://github.com/jenniferthompson/DataCleanExample/blob/master/dataclean_helpers.R): Includes export of data dictionary from REDCap and three major helper
functions I use often when cleaning data:
    1. `create_error_df()`: Given a matrix of T/F values and a set of error
    messages and labels, create a data.frame of all data issues represented
    1. `check_missing()`: Check whether variables are simply missing from the
    specified data set; give error messages using labels from REDCap data
    dictionary
    1. `check_limits_numeric()`: Check whether numeric fields are within
    specified limits, using either REDCap data dictionary limits (default) or
    user-supplied values
    
#### Auxilliary Files for Tutorial

- `tutorialfiles/`: At a certain point in the tutorial, we need to wipe the
slate clean and start over with updated datasets. `dataclean_partial.R` reads in
the correct data and performs all operations needed to get you in the
appropriate state at that point.
- Data files (CSV) included for those unable to connect to the OCU REDCap
instance:
    - `rawdata/`: Contains data as originally entered in the example REDCap
    database and exported using the REDCap API, using code as shown in the
    tutorial.
    - `fixeddata/`: Data in the same formats as `rawdata/`, but after a few
    issues have been "corrected" in the original database.
    - `querydata/`: CSV files of the data issues observed at each point in the
    process: using `rawdata/`; using `fixeddata/`; and after unneeded issues
    have been removed.
- Codebooks:
    - `codebook.pdf`: for example study database
    - `codebook_documentation.pdf`: for documentation database

These materials were originally developed for a workshop series at Osaka City
University, Osaka, Japan, June 2018. They are under an [MIT
license](LICENSE).
