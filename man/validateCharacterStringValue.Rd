% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{validateCharacterStringValue}
\alias{validateCharacterStringValue}
\title{Validate Character String Value}
\usage{
validateCharacterStringValue(name, value)
}
\arguments{
\item{name}{A character string representing the name of the object being validated.}

\item{value}{A character string.}
}
\value{
Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
}
\description{
Validate that a string has a length of 1, is a character type, and is not NA or empty.
}
