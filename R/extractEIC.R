#' Extract Extracted Ion Current (EIC)
#'
#' Extract the intensity of each target m/z (EIC) detected within the spectra.
#' 
#' The length of the target list of unique m/z values MUST BE EQUAL to the length of spectra array (m/z and intensity)
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param intensityArray Intensity array column name: character
#' @param targetList A list of unique target m/z mirroring the values stored within the intensity array (Must be equal length): character vector
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