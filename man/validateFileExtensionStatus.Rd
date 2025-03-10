% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{validateFileExtensionStatus}
\alias{validateFileExtensionStatus}
\title{Validate File Extension Status}
\usage{
validateFileExtensionStatus(name, value, fileExtension)
}
\arguments{
\item{name}{A character string representing the name of the object being validated.}

\item{value}{A character string representing a file path.}

\item{fileExtension}{A character string representing a file extension pattern.}
}
\value{
Invisible TRUE if the file matches the specified file extension pattern; otherwise, it returns invisible FALSE.
}
\description{
Validate that a character string representing a file path matches a specific file extension pattern.
}
