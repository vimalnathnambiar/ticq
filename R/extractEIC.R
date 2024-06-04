#' Extract Extracted Ion Current (EIC)
#'
#' Extract the intensity of each target m/z (EIC) detected within the spectra.
#' 
#' The length of the target list containing unique m/z values must be equal to the length of intensity array for each spectral data and reflect the corresponding m/z values.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param intensityArray Intensity array column name: character
#' @param targetList A list of unique target m/z (Must be equal length and correspond to the values stored within intensity array): character vector
#' @returns A data frame with appended columns representing the extracted intensity of each target m/z (EIC)
extractEIC <- function(data, intensityArray, targetList) {
  # Extract intensity of each target m/z into a matrix
  tmp <- do.call(cbind, data[[intensityArray]])
  
  # Transpose and convert matrix into data frame
  tmp <- as.data.frame(t(tmp))
  
  # Rename columns
  colnames(tmp) <- targetList
  
  # Append new columns to data
  data <- cbind(data, tmp)
  
  return(data)
}