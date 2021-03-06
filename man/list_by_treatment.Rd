% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{list_by_treatment}
\alias{list_by_treatment}
\title{Identify records by their treatment}
\usage{
list_by_treatment(
  d,
  subset = c("treatment", "control"),
  treatments = c("trt1")
)
}
\arguments{
\item{d}{input \code{data.frame} of clinical data with concomitant medications}

\item{subset}{which treatment arm to consider - treatment vs control}

\item{treatments}{vector of treatment names as found in the exposure domain (to distinguish from control)}
}
\value{
a \code{vector} of the subject IDs who receieved the given subset of treatments
}
\description{
Identify records by their treatment
}
\details{
This function performs some cleanup - particular EXTRT records are considered as 'not treated'. )
Additionally, subject who missed all their doses are considered as 'not treated'.
}
