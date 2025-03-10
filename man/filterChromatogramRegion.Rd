% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterChromatogramRegion.R
\name{filterChromatogramRegion}
\alias{filterChromatogramRegion}
\title{Filter Chromatogram Region}
\usage{
filterChromatogramRegion(data, regionOfInterest = NULL, retentionTime)
}
\arguments{
\item{data}{A data frame of the MS spectral data.}

\item{regionOfInterest}{A list representing the start and end time points of a chromatogram region of interest.
(Default: \code{NULL}; Options: \code{configureChromatogramRegion()$regionOfInterest} or \code{configureChromatogramRegion()[["regionOfInterest"]]})}

\item{retentionTime}{A character string representing the name of the retention time column.}
}
\value{
A data frame of the MS spectral data with a retention time between the start and end time point of the chromatogram region of interest.
}
\description{
Filter for spectral data that falls between the start and end time points of a chromatogram region of interest.
}
