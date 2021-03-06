% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{cm_records}
\alias{cm_records}
\title{Extract records from nested.CM domain}
\usage{
cm_records(d = NULL)
}
\arguments{
\item{d}{input \code{data.frame} of clinical data with concomitant medications}
}
\value{
a \code{data.frame} of concomitant medication records
}
\description{
Extract records from nested.CM domain
}
\details{
Additionally filters to concomitant medication records according to \code{CMCAT}
and removes 'not treated' records.
}
