# Data Cleaning Example

This repo holds code demonstrating the ongoing process of data cleaning used by the [Vanderbilt CIBS Center](http://icudelirium.org). More details forthcoming.

The code refers to a sample [REDCap](https://projectredcap.org/) database for a three-month longitudinal study of adult patients taking a dietary supplement and measuring creatinine, HDL and LDL cholesterol, and weight over time. (Sample database is adapted with thanks from REDCap's project templates.) The [study codebook](codebook.pdf) is included here.

*Note*: All code assumes that the user has rights to use the REDCap API, and that a working API token is stored in the `.Renviron` file in this working directory, in the format

`RCTOKEN=manylettersandnumbers`

For more information on the REDCap API, please see `Project Setup -> Other Functionality` within an existing REDCap project. For general information on working with the API, the [Github wiki of the `redcapAPI` package](https://github.com/nutterb/redcapAPI/wiki) has a good overview. (This example includes basic API usage and will not use the package, but if you are interested in using more the API's functionality, it would be a great one to investigate.)
