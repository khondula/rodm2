
<!-- README.md is generated from README.Rmd. Please edit that file -->
rodm2
=====

<!-- badges: start -->

[![Travis build status](https://travis-ci.org/khondula/rodm2.svg?branch=master)](https://travis-ci.org/khondula/rodm2)

<!-- badges: end -->

Organize site, sample, and sensor data all in one place!

The goal of **rodm2** is to make it easy to use the [ODM2](https://github.com/ODM2/ODM2) data model in R, in order to promote collaboration between ecologists, hydrologists, and soil scientists. It is intended to help manage complex field data using a sophisticated relational data model without requiring prior knowledge of database principles.

Package functions create parameterized SQL statements to populate and query a flat file (sqlite) database using inputs such as site names, variables, and date ranges. Interactive features (shiny gadgets) are included to facilitate the use of a controlled vocabulary and represent complex hierarchical site and sampling designs within the database structure.

This project is just getting started! Get in touch or open an [issue](https://github.com/khondula/rodm2/issues) if you have a use case or if you're interested in collaborating.

Getting started
---------------

You can install this package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("khondula/rodm2")
```

-   Set up or connect to an sqlite ODM2 database with create\_sql() or connect\_sqlite()
-   Insert metadata about sites, methods, measured variables using `db_describe_*()` functions
-   Optionally, label sites with annotations and describe hierarchical site designs as related features
-   Insert data from time series, in situ point or profile measurements, or ex situ data from samples collected in the field
-   Query results based on site, variable, and data type (e.g. time series, samples) using `db_get_results()`

Workflow
--------

1.  Connect

Load rodm2 and create a connection to your database. If you don't have an existing database, create an empty ODM2 sqlite database and connection using `create_sqlite(connect = TRUE)` or if you are starting from an existing database use `connect_sqlite()`. You can view and edit this database using [DB Browser for SQLite](https://sqlitebrowser.org/) outside of R.

``` r
library(rodm2)
db <- rodm2::create_sqlite()
```

1.  Describe

Add information about sites, methods, measured variables, etc. using `db_describe_%TABLE%` functions.

``` r
db_describe_person(db = db, 
                          PersonFirstName = "Wendy",
                          PersonLastName = "Wetland",
                          AffiliationStartDate = "2018-01-01",
                          PrimaryEmail = "wendy 'at' swamps.edu")

db_describe_site(db, 
                        site_code = "501R1", 
                        sitetypecv = "Site")

db_describe_method(db, 
                          methodname = "Sonic anemometer",
                          methodcode = "SA",
                          methodtypecv = "dataRetrieval")
```

1.  Insert data

The data to upload should have time series data for one or more variables collected at one site using the same instrument. It needs at least 2 timepoints, and a "Timestamp column" formatted as YYYY-MM-DD HH:MM:SS. For example:

| Timestamp           |   wd|   ws|  gustspeed|
|:--------------------|----:|----:|----------:|
| 2018-06-27 13:45:00 |  180|  1.0|        2.0|
| 2018-06-27 13:55:00 |  170|  1.5|        2.5|

Data are uploaded using "insert" functions for specific types of results, eg. `db_insert_results_ts()` for a data frame of time series results. Data needs to have a column "Timestamp" and can have multiple columns of data. The type of data in each column needs to be described in a "variables list" with terms and units from a controlled vocabulary. Create this list for a dataset using a shiny gadget (opens in the Viewer pane of RStudio) with the function `make_vars_list()`.

``` r
vars_list <- make_vars_list(ts_data)
```

![vars list shiny gadget](https://raw.githubusercontent.com/khondula/rodm2/master/man/figures/vars-gadget.png)

Once the `vars_list` is created, use it in the `db_insert_results_ts()` function to upload data along with its associated metadata

``` r
db_insert_results_ts(db, 
                     datavalues = ts_data,
                     method = "SA",
                     site_code = "501R1",
                     variables = vars_list,
                     sampledmedium = "Air")
#> Warning: Closing open result set, pending rows
```

1.  Query

``` r
db_get_results(db, result_type = "ts")
#> Warning: Closing open result set, pending rows
#> $`Time series data`
#>          ValueDateTime DataValue        UnitsName  VariableNameCV
#> 1  2018-06-27 13:55:00     170.0           Degree  Wind direction
#> 2  2018-06-27 13:45:00     180.0           Degree  Wind direction
#> 3  2018-06-27 13:45:00       1.0 Meter per Second      Wind speed
#> 4  2018-06-27 13:55:00       1.5 Meter per Second      Wind speed
#> 5  2018-06-27 13:45:00       2.0 Meter per Second Wind gust speed
#> 6  2018-06-27 13:55:00       2.5 Meter per Second Wind gust speed
#> 7  2018-06-27 13:55:00     170.0           Degree  Wind direction
#> 8  2018-06-27 13:45:00     180.0           Degree  Wind direction
#> 9  2018-06-27 13:45:00       1.0 Meter per Second      Wind speed
#> 10 2018-06-27 13:55:00       1.5 Meter per Second      Wind speed
#> 11 2018-06-27 13:45:00       2.0 Meter per Second Wind gust speed
#> 12 2018-06-27 13:55:00       2.5 Meter per Second Wind gust speed
#> 13 2018-06-27 13:55:00     170.0           Degree  Wind direction
#> 14 2018-06-27 13:45:00     180.0           Degree  Wind direction
#> 15 2018-06-27 13:45:00       1.0 Meter per Second      Wind speed
#> 16 2018-06-27 13:55:00       1.5 Meter per Second      Wind speed
#> 17 2018-06-27 13:45:00       2.0 Meter per Second Wind gust speed
#> 18 2018-06-27 13:55:00       2.5 Meter per Second Wind gust speed
#>    SamplingFeatureCode ProcessingLevelCode
#> 1                501R1            Raw data
#> 2                501R1            Raw data
#> 3                501R1            Raw data
#> 4                501R1            Raw data
#> 5                501R1            Raw data
#> 6                501R1            Raw data
#> 7                501R1            Raw data
#> 8                501R1            Raw data
#> 9                501R1            Raw data
#> 10               501R1            Raw data
#> 11               501R1            Raw data
#> 12               501R1            Raw data
#> 13               501R1            Raw data
#> 14               501R1            Raw data
#> 15               501R1            Raw data
#> 16               501R1            Raw data
#> 17               501R1            Raw data
#> 18               501R1            Raw data
#> 
#> $`Sample data`
#> NULL
#> 
#> $`Measurements data`
#> NULL
```

``` r
RSQLite::dbDisconnect(db)
```

More details
------------

`rodm2` is designed to work either with an existing ODM2 database on a server (e.g. PostgreSQL), or with spreadsheet files that aren't (yet!) in a relational database. This package is intended to help populate, query, and visualize data that is organized with the ODM2 structure. It should not be necessary to understand ODM2 or SQL but it may help! See [here](http://odm2.github.io/ODM2/schemas/ODM2_Current/diagrams/ODM2OverviewSimplified.html) for an overview of the data structure.

Acknowledgements
----------------

This project is made possible through support from [rOpenSci](https://ropensci.org/) and [SESYNC](https://www.sesync.org/).

Read about the details and development of ODM2 in the open access paper in Environmental Modelling & Software:

> Horsburgh, J. S., Aufdenkampe, A. K., Mayorga, E., Lehnert, K. A., Hsu, L., Song, L., Spackman Jones, A., Damiano, S. G., Tarboton, D. G., Valentine, D., Zaslavsky, I., Whitenack, T. (2016). [Observations Data Model 2: A community information model for spatially discrete Earth observations](http://dx.doi.org/10.1016/j.envsoft.2016.01.010), Environmental Modelling & Software, 79, 55-74, <http://dx.doi.org/10.1016/j.envsoft.2016.01.010>
