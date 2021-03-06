% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_describe_sample.R
\name{db_describe_sample}
\alias{db_describe_sample}
\title{Add details about a new sample}
\usage{
db_describe_sample(db, sample_code, site_code)
}
\arguments{
\item{db}{database connection}

\item{sample_code}{sampling feature code for sample}

\item{site_code}{sampling feature code for where sample was collected from}
}
\value{
message that sample was added to sampling feature table
}
\description{
Add details about a new sample
}
\details{
Adds entry to samplingfeatures table as type 'Specimen' and entry into
related features table that "sample_code" wasCollectedAt "site_code".
Does not add an action for specimen collection - for that use \code{\link{db_insert_results_samples}}.
}
\examples{

db <- create_sqlite(connect = TRUE)
db_describe_site(db, site_code = "site1")

db_describe_sample(db,
sample_code = "sample001",
site_code = "site1")

}
