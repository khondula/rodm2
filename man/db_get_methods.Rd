% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_describe_method.R
\name{db_get_methods}
\alias{db_get_methods}
\title{Get list of methods currently in database}
\usage{
db_get_methods(db)
}
\arguments{
\item{db}{database connection object}
}
\value{
the current values in the methodname column of the methods table
}
\description{
Get list of methods currently in database
}
\examples{
db <- create_sqlite(connect = TRUE)

db_describe_method(db,
methodname = "new method",
methodcode = "method1",
methodtypecv =  "fieldActivity")

db_get_methods(db)
}
