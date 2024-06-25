#' Extract Extracted Ion Current (EIC)
#'
#' Extract the intensity of each target m/z (EIC) detected within the spectra.
#'
#' The length of the target list containing unique m/z values must be equal to the length of intensity array for each spectral data and reflect the corresponding m/z values.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data
#' @param intensityArray A character string representing the name of the intensity array column.
#' @param targetMZ A character vector representing the target m/z values reflecting the intensity values in the array.
#' @returns A data frame with appended columns representing the extracted intensity of each target m/z (EIC).
extractEIC <- function(data, intensityArray, targetMZ) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  # Extract intensity of each target m/z into a matrix
  tmp <- do.call(cbind, data[[intensityArray]])
  
  # Transpose and convert matrix into data frame
  tmp <- as.data.frame(t(tmp))
  
  # Rename columns
  colnames(tmp) <- targetMZ
  
  # Append new columns to data
  return(cbind(data, tmp))
}