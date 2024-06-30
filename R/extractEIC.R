#' Extract Extracted Ion Current (EIC)
#'
#' Extract the intensity of each target m/z value in the spectra array.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param intensityArray A character string representing the name of the intensity array column.
#' @param targetMZ A character vector representing the targeted m/z values in the spectra array.
#' @returns A data frame of the MS spectral data and the intensity of each target m/z value.
extractEIC <- function(data, intensityArray, targetMZ) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(intensityArray = intensityArray, targetMZ = targetMZ)
  for (i in names(parameter)) {
    if (i == "intensityArray") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(intensityArray)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to extract EIC: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Extract intensity of each target m/z
  tmp <- do.call(cbind, data[[intensityArray]])
  tmp <- as.data.frame(t(tmp))
  colnames(tmp) <- targetMZ
  
  return(cbind(data, tmp))
}