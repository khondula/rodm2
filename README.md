
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

`rodm2` is designed to work either with an existing ODM2 database on a server (e.g. PostgreSQL), or with spreadsheet files that aren't (yet!) in a formal database.

Initialize an empty sqlite database with odm2 schema in your working directory (empty except for the controlled vocabulary tables). You can view and edit this database using [DB Browser for SQLite](https://sqlitebrowser.org/) outside of R.

``` r
library(rodm2)
create_sqlite(dir = ".")
```

To work with it in R, create a connection object

``` r
library(RSQLite)
dblite <- dbConnect(RSQLite::SQLite(), "odm2.sqlite")
head(dbListTables(dblite))
#> [1] "ActionAnnotations"             "ActionBy"                     
#> [3] "ActionDirectives"              "ActionExtensionPropertyValues"
#> [5] "Actions"                       "Affiliations"
```

Acknowledgements
----------------

This project is made possible through support from [rOpenSci](https://ropensci.org/) and [SESYNC](https://www.sesync.org/).

Read about the details and development of ODM2 in the open access paper in Environmental Modelling & Software:

> Horsburgh, J. S., Aufdenkampe, A. K., Mayorga, E., Lehnert, K. A., Hsu, L., Song, L., Spackman Jones, A., Damiano, S. G., Tarboton, D. G., Valentine, D., Zaslavsky, I., Whitenack, T. (2016). [Observations Data Model 2: A community information model for spatially discrete Earth observations](http://dx.doi.org/10.1016/j.envsoft.2016.01.010), Environmental Modelling & Software, 79, 55-74, <http://dx.doi.org/10.1016/j.envsoft.2016.01.010>
