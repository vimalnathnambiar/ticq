#' Extract Mass Accuracy
#'
#' Extract the mass accuracy (in ppm) of each target m/z data detected within the spectra.
#'
#' The length of the target list containing unique m/z values must be equal to the length of m/z array for each spectral data and reflect the corresponding m/z values stored.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param mzArray m/z array column name: character
#' @param targetMZ Target m/z values (Must be unique, sorted, and equal length and correspond to the values stored within intensity array): character vector
#' @returns A data frame with appended columns representing the calculated mass accuracy (in ppm) for each target m/z (EIC)
extractMassAccuracy <- function(data, mzArray, targetMZ) {
  # Extract m/z value within the array of each spectra into a matrix
  tmp <- do.call(cbind, data[[mzArray]])
  
  # Calculate mass accuracy (in ppm)
  tmp <- (tmp - targetMZ) / targetMZ * 1e6
  
  # Transpose matrix and convert into data frame
  tmp <- as.data.frame(t(tmp))
  
  # Rename columns
  colnames(tmp) <- as.character(targetMZ)
  
  # Append new columns to data
  data <- cbind(data, tmp)
  
  return(data)
}