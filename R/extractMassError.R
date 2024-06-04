#' Extract Mass Error
#'
#' Extract the mass error (in ppm) of each target m/z data detected within the spectra.
#'
#' The length of the target list containing unique m/z values must be equal to the length of m/z array for each spectral data and reflect the corresponding m/z values stored.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param mzArray m/z array column name: character
#' @param targetList A list of unique target m/z (Must be equal length and correspond to the values stored within m/z array): numeric vector
#' @returns A data frame with appended columns representing the calculated mass error (ppm) of each target m/z (EIC)
extractMassError <- function(data, mzArray, targetList) {
  # Extract m/z value within the array of each spectra into a matrix
  tmp <- do.call(cbind, data[[mzArray]])
  
  # Calculate mass error (in ppm)
  tmp <- (tmp - targetList) / targetList * 1e6
  
  # Transpose matrix and convert into data frame
  tmp <- as.data.frame(t(tmp))
  
  # Rename columns
  colnames(tmp) <- as.character(targetList)
  
  # Append new columns to data
  data <- cbind(data, tmp)
  
  return(data)
}