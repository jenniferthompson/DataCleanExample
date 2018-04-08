# Data Cleaning Example

This repo holds code demonstrating the ongoing process of data cleaning used by the [Vanderbilt CIBS Center](http://icudelirium.org). More details forthcoming.

The code refers to a sample [REDCap](https://projectredcap.org/) database for a six-month longitudinal study of adult patients taking a dietary supplement and measuring creatinine, HDL and LDL cholesterol, and weight over time. (Sample database is adapted with thanks from REDCap's project templates.) The [study codebook](codebook.pdf) is included here.

*Note*: All code assumes that the user has rights to use the REDCap API, and that a working API token is stored in the `.Renviron` file in this working directory, in the format

`RCTOKEN=manylettersandnumbers`
