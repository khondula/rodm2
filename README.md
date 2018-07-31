
<!-- README.md is generated from README.Rmd. Please edit that file -->
rodm2
=====

The goal of **rodm2** is to make it easy to use the [ODM2](https://github.com/ODM2/ODM2) data model in R, in order to promote collaboration between ecologists, hydrologists, and soil scientists.

Organize site, sample, and sensor data all in one place! For example:

-   time series hydrologic observations from a well or stream gage
-   sediment sample properties from a soil core analyzed in a lab
-   categorical observations manually recorded in the field

This project is just getting started! Get in touch or open an [issue](https://github.com/khondula/rodm2/issues) if you have a use case or if you're interested in collaborating.

Installation
------------

You can install this package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("khondula/rodm2")
```

Usage
-----

Organize a dataframe of time series values into a new ODM2 sqlite database:

The data to upload must have at least 2 timepoints, a "Timestamp column" formatted as YYYY-MM-DD HH:MM:SS, and column names that are controlled vocabulary variable names (use a tibble to allow for spaces in column names).

| Timestamp           |   wd|   ws|  gustspeed|
|:--------------------|----:|----:|----------:|
| 2018-06-27 13:45:00 |  180|  1.0|        2.0|
| 2018-06-27 13:55:00 |  170|  1.5|        2.5|

``` r
library(rodm2)
dblite <- create_sqlite(connect = TRUE)
```

Create a list that matches the variables to column names and units:

``` r
vars_list <- list(
  'Wind direction' = list(column = 'wd', units = 'Degree'),
  'Wind speed' = list(column = 'ws', units = 'Meter per Second'),
  'Wind gust speed' = list(column = 'gustspeed', units = 'Meter per Second')
)
```

If there are columns in the data that correspond to censor codes or data quality codes, also supply those column names as 'qualitycodecol' and/or 'censorcodecol' in the list for each variable.

Supply database connection object, new data, method, site, variables, and sampled medium to insert time series data.

``` r
db_insert_results_ts(db = dblite, # database connection
                     datavalues = ts_data, # dataframe with Timestamp column
                     method = "SonicAnemometer", 
                     site_code = "Site1", 
                     variables = vars_list, 
                     sampledmedium = "Air"
                     )
#> SonicAnemometer has been added to the Methods table.
#> Wind direction has been added to the Variables table.
#> Wind speed has been added to the Variables table.
#> Wind gust speed has been added to the Variables table.
#> Site Site1 has been entered into the samplingfeatures table.
```

More details
------------

`rodm2` is designed to work either with an existing ODM2 database on a server (e.g. PostgreSQL), or with spreadsheet files that aren't (yet!) in a relational database. This package is intended to help populate, query, and visualize data that is organized with the ODM2 structure. It should not be necessary to understand ODM2 or SQL but it may help! See [here](http://odm2.github.io/ODM2/schemas/ODM2_Current/diagrams/ODM2OverviewSimplified.html) for an overview of the data structure.

**With a PostgreSQL database**

A script for creating a PostgreSQL database with the ODM2 schema is [here](https://github.com/ODM2/ODM2/blob/master/src/blank_schema_scripts/postgresql/ODM2_for_PostgreSQL.sql) and instructions for populating the controlled vocabularies tables using a script are [here](https://github.com/ODM2/ODM2/tree/master/src/load_cvs).

You will need to create a database connection object, eg. using `DBI::dbConnect()` and your host and log-in credentials.

**Without an external database**

Initialize an empty sqlite database with odm2 schema in your working directory (empty except for the controlled vocabulary tables). You can view and edit this database using [DB Browser for SQLite](https://sqlitebrowser.org/) outside of R. By default, `create_sqlite()` just creates the sqlite file but `connect = TRUE` will return the connection object as well.

``` r
library(rodm2)
dblite <- create_sqlite(connect = TRUE)
```

Or create a connection to an existing sqlite file using `dbConnect` and the name of the file.

``` r
dblite <- DBI::dbConnect(RSQLite::SQLite(), "odm2.sqlite")
```

There are 132 tables in the database. The "blank" database has the Units table populated as well as all of the controlled vocabularies.

See the terms for a given controlled vocabulary:

``` r
rodm2::get_cv_terms("methodtype")
#> methodtype controlled vocabulary terms: "Cruise", "Data retrieval", "Derivation", "Equipment deployment", "Equipment maintenance", "Equipment programming", "Equipment retrieval", "Estimation", "Expedition", "Field activity", "Generic non-observation", "Instrument Continuing Calibration Verification", "Instrument calibration", "Instrument deployment", "Instrument retrieval", "Observation", "Simulation", "Site visit", "Specimen analysis", "Specimen collection", "Specimen fractionation", "Specimen preparation", "Specimen preservation", "Submersible launch", "Unknown")
```

Explore the database structure:

``` r
head(DBI::dbListTables(dblite))
#> [1] "ActionAnnotations"             "ActionBy"                     
#> [3] "ActionDirectives"              "ActionExtensionPropertyValues"
#> [5] "Actions"                       "Affiliations"
DBI::dbListFields(dblite, "featureactions")
#> [1] "FeatureActionID"   "SamplingFeatureID" "ActionID"
head(DBI::dbReadTable(dblite, "units"))
#>   UnitsID           UnitsTypeCV UnitsAbbreviation       UnitsName
#> 1       1         Absorbed dose                Gy            Gray
#> 2       2         Absorbed dose               rad             RAD
#> 3       3    Absorbed dose rate              Gy/s Gray per Second
#> 4       4 Amount of Information               ban             Ban
#> 5       5 Amount of Information               bit             Bit
#> 6       6 Amount of Information                By            Byte
#>   UnitsLink
#> 1      <NA>
#> 2      <NA>
#> 3      <NA>
#> 4      <NA>
#> 5      <NA>
#> 6      <NA>
```

"Describe"" functions
---------------------

Add information using `db_describe_%TABLE%` functions.

``` r
rodm2::db_describe_person(db = dblite, PersonFirstName = "Wendy",
     PersonLastName = "Wetland",
     AffiliationStartDate = "2018-01-01",
     PrimaryEmail = "wendy 'at' swamps.edu")
#> Wendy Wetland has been entered into the People table.
```

There are currently "describe" functions for:

-   methods
-   people
-   variables
-   annotations
-   organizations (necessary for equipment)

'Insert' functions
------------------

Data are uploaded using "insert" functions for specific types of results, eg. `db_insert_results_ts()` for a data frame of time series results.

-   time\_series: time series data collected at a site using a sensor

In progress:

-   measurement: in-situ point measurement (numeric) taken at a site
-   category: in-situ point measurement (text/categorical) taken at a site
-   sample\_measurement: ex-situ measurement (numeric) of a sample taken at a site

Acknowledgements
----------------

This project is made possible through support from [rOpenSci](https://ropensci.org/) and [SESYNC](https://www.sesync.org/).

Read about the details and development of ODM2 in the open access paper in Environmental Modelling & Software:

> Horsburgh, J. S., Aufdenkampe, A. K., Mayorga, E., Lehnert, K. A., Hsu, L., Song, L., Spackman Jones, A., Damiano, S. G., Tarboton, D. G., Valentine, D., Zaslavsky, I., Whitenack, T. (2016). [Observations Data Model 2: A community information model for spatially discrete Earth observations](http://dx.doi.org/10.1016/j.envsoft.2016.01.010), Environmental Modelling & Software, 79, 55-74, <http://dx.doi.org/10.1016/j.envsoft.2016.01.010>
