
<!-- README.md is generated from README.Rmd. Please edit that file -->
rodm2
=====

The goal of **rodm2** is to make it easy to use the [ODM2](https://github.com/ODM2/ODM2) data model in R. Organize your site, sample, and sensor data for watershed science projects all in one place.

This project is just getting started! Get in touch or open an [issue](https://github.com/khondula/rodm2/issues) if you have a use case or if you're interested in collaborating.

Installation
------------

You can install this package from GitHub with:

``` r
# install.packages("devtools")
library(devtools)
install_github("khondula/rodm2")
```

Example
-------

Initialize an empty sqlite database with odm2 schema. You can view and edit this database using [DB Browser for SQLite](https://sqlitebrowser.org/) outside of R.

``` r
library(rodm2)
create_sqlite()
```

Acknowledgements
----------------

This project is made possible with support from rOpenSci and SESYNC.
