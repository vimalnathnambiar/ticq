% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{validateStatisticalNumericVectorElement}
\alias{validateStatisticalNumericVectorElement}
\title{Validate Statistical Numeric Vector Element}
\usage{
validateStatisticalNumericVectorElement(name, value)
}
\arguments{
\item{name}{A character string representing the name of the object being validated.}

\item{value}{A numeric vector.}
}
\value{
Invisible TRUE if the numeric vector is valid; otherwise, it stops with an error message.
}
\description{
Validate that a vector has a length greater than 1, is a numeric type, and does not contain any NA elements.
}
