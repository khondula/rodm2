
<!-- README.md is generated from README.Rmd. Please edit that file -->
rodm2
=====

<!-- badges: start --> [![Travis build status](https://travis-ci.org/khondula/rodm2.svg?branch=master)](https://travis-ci.org/khondula/rodm2) <!-- badges: end -->

The goal of **rodm2** is to make it easy to use the [ODM2](https://github.com/ODM2/ODM2) data model in R, in order to promote collaboration between ecologists, hydrologists, and soil scientists.

Organize site, sample, and sensor data all in one place!

-   create the skeleton of an sqlite ODM2 database with create\_sql()
-   insert time series data using db\_insert\_results\_ts()
-   insert (in situ) measurement data using db\_insert\_results\_m()
-   insert (ex situ) sample measurement data using db\_insert\_results\_samples()
-   helper functions for controlled vocab terms and site names
-   populate sites, people, methods, variables, equipment, organizations, annotation with db\_describe\_%() functions
-   query list of site names, people, methods, variables, equipment, organizations with db\_get\_%() functions

This project is just getting started! Get in touch or open an [issue](https://github.com/khondula/rodm2/issues) if you have a use case or if you're interested in collaborating.

Installation
------------

You can install this package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("khondula/rodm2")
```

More details
------------

`rodm2` is designed to work either with an existing ODM2 database on a server (e.g. PostgreSQL), or with spreadsheet files that aren't (yet!) in a relational database. This package is intended to help populate, query, and visualize data that is organized with the ODM2 structure. It should not be necessary to understand ODM2 or SQL but it may help! See [here](http://odm2.github.io/ODM2/schemas/ODM2_Current/diagrams/ODM2OverviewSimplified.html) for an overview of the data structure.

Getting started
---------------

Load rodm2 and create a connection to your database. If you don't have an existing database, create an empty ODM2 sqlite database and connection using `create_sqlite(connect = TRUE)` or if you are starting from an existing database use `connect_sqlite()`. You can view and edit this database using [DB Browser for SQLite](https://sqlitebrowser.org/) outside of R.

``` r
library(rodm2)
db <- rodm2::create_sqlite()
```

There are 132 tables in the database. The "blank" database has the Units table populated as well as all of the controlled vocabularies.

See the terms for a given controlled vocabulary:

``` r
rodm2::get_cv_terms("methodtype")
#> methodtype controlled vocabulary terms: "Cruise", "Data retrieval", "Derivation", "Equipment deployment", "Equipment maintenance", "Equipment programming", "Equipment retrieval", "Estimation", "Expedition", "Field activity", "Generic non-observation", "Instrument Continuing Calibration Verification", "Instrument calibration", "Instrument deployment", "Instrument retrieval", "Observation", "Simulation", "Site visit", "Specimen analysis", "Specimen collection", "Specimen fractionation", "Specimen preparation", "Specimen preservation", "Submersible launch", "Unknown")
#> use quietly = FALSE to print out all controlled vocabulary terms
```

"Describe" functions
--------------------

Add information using `db_describe_%TABLE%` functions.

``` r
rodm2::db_describe_person(db = db, PersonFirstName = "Wendy",
     PersonLastName = "Wetland",
     AffiliationStartDate = "2018-01-01",
     PrimaryEmail = "wendy 'at' swamps.edu")
#> Wendy Wetland has been entered into the People table.
```

'Insert' functions
------------------

Data are uploaded using "insert" functions for specific types of results, eg. `db_insert_results_ts()` for a data frame of time series results.

-   time\_series: time series data collected at a site using a sensor

Acknowledgements
----------------

This project is made possible through support from [rOpenSci](https://ropensci.org/) and [SESYNC](https://www.sesync.org/).

Read about the details and development of ODM2 in the open access paper in Environmental Modelling & Software:

> Horsburgh, J. S., Aufdenkampe, A. K., Mayorga, E., Lehnert, K. A., Hsu, L., Song, L., Spackman Jones, A., Damiano, S. G., Tarboton, D. G., Valentine, D., Zaslavsky, I., Whitenack, T. (2016). [Observations Data Model 2: A community information model for spatially discrete Earth observations](http://dx.doi.org/10.1016/j.envsoft.2016.01.010), Environmental Modelling & Software, 79, 55-74, <http://dx.doi.org/10.1016/j.envsoft.2016.01.010>
