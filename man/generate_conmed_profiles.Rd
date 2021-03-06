% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{generate_conmed_profiles}
\alias{generate_conmed_profiles}
\title{Build conMed profiles for a given study and a given drug basket}
\usage{
generate_conmed_profiles(
  d = NULL,
  drug_basket_name = NULL,
  drug_basket_data_object = NULL,
  combo_drug_label_prefix = NULL,
  combo_drug_label_suffix = NULL,
  drug_free_label = NULL,
  treatments = c("trt1")
)
}
\arguments{
\item{d}{input \code{data.frame} of clinical data with concomitant medications}

\item{drug_basket_name}{name of drug basket (will be searched - see details)}

\item{drug_basket_data_object}{drug basket input data}

\item{combo_drug_label_prefix}{prefix to use for COMBO records}

\item{combo_drug_label_suffix}{suffix to use for COMBO records}

\item{drug_free_label}{label to use for DRUG_FREE records}

\item{treatments}{vector of treatment names as found in the exposure domain (to distinguish from control)}
}
\value{
output \code{data.frame} of conmed profiles
}
\description{
Build conMed profiles for a given study and a given drug basket
}
\details{
if \code{drug_basket_data_object} is provided, an object with that name will be
fetched from the workspace / global environment. If this is not provided, but
\code{drug_basket_name} is, a basket with this name will be fetched from the package.

This package stores a \code{basket_demo} object for demonstration.
}
