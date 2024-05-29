#' extractEIC
#'
#' Extract the intensity of each target m/z (Extracted Ion Current, EIC) stored within the spectra.
#'
#' @import dplyr
#'
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param intensityArray Name of the column that represents intensity array: character
#' @param targetList Unique target m/z value that reflect the intensity value in the array: character vector
#' @returns Data frame with the extracted intensity of each target m/z (EIC) in its own individual column appended
extractEIC <- function(data, intensityArray, targetList) {
  # Extract intensity of each target m/z into a matrix
  tmp <- do.call(cbind, data[[intensityArray]])
  
  # Transpose matrix and convert into data frame
  tmp <- as.data.frame(t(tmp))
  
  # Rename columns
  colnames(tmp) <- targetList
  
  # Append new columns to data
  data <- cbind(data, tmp)
  
  return(data)
}