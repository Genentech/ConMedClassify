% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{is_cm_time_uncertain}
\alias{is_cm_time_uncertain}
\title{Determine if concomitant medication timing is uncertain}
\usage{
is_cm_time_uncertain(
  CMEND = NULL,
  CMSTART = NULL,
  START_TREATMENT = NULL,
  END_TREATMENT = NULL
)
}
\arguments{
\item{CMEND}{ADaM data for end day (relative to start of trial) of concomitant medication exposure}

\item{CMSTART}{ADaM data for start day (relative to start of trial) of concomitant medication exposure}

\item{START_TREATMENT}{ADaM data for start day (relative to start of trial) of treatment exposure}

\item{END_TREATMENT}{ADaM data for end day (relative to start of trial) of treatment exposure}
}
\value{
a \code{logical} vector of whether a given record is uncertain in timing
}
\description{
Determine if concomitant medication timing is uncertain
}
