% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateAverageMassAccuracy.R
\name{calculateAverageMassAccuracy}
\alias{calculateAverageMassAccuracy}
\title{Calculate Average Mass Accuracy}
\usage{
calculateAverageMassAccuracy(
  data,
  commonColumn,
  spectrumCount,
  firstColumnIndex,
  lastColumnIndex = NULL,
  roundDecimalPlace = NULL
)
}
\arguments{
\item{data}{A data frame of the MS spectral data.}

\item{commonColumn}{A character vector representing the names of the common data columns to be used for data grouping.}

\item{spectrumCount}{A character string representing the name of the spectrum count column.}

\item{firstColumnIndex}{A numeric value representing the index of the first data column to be averaged.}

\item{lastColumnIndex}{A numeric value representing the index of the last data column to be averaged. (Default: \code{NULL})}

\item{roundDecimalPlace}{A numeric value representing the number of decimal places to be used for precision value rounding. (Default: \code{NULL})}
}
\value{
A data frame of the MS spectral data summary grouped by their common data columns and the average mass accuracy of each target m/z value.
}
\description{
Calculate the average mass accuracy (in ppm) of each target m/z for each data group.
}
