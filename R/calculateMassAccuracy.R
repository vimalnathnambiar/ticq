#' Calculate Mass Accuracy
#'
#' Calculate the mass accuracy (in ppm) of each target m/z value in the spectra array.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param mzArray A character string representing the name of the m/z array column.
#' @param targetMZ A numeric vector representing the target m/z values in the spectra array.
#' @param roundDecimalPlace A numeric value representing the number of decimal places to be used for precision value rounding. (Default: `NULL`)
#' @returns A data frame of the MS spectral data and the calculated mass accuracy of each target m/z value.
calculateMassAccuracy <- function(data, mzArray, targetMZ, roundDecimalPlace = NULL) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(mzArray = mzArray, targetMZ = targetMZ, roundDecimalPlace = roundDecimalPlace)
  for (i in names(parameter)) {
    if (i == "mzArray") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "targetMZ") {
      validateNumericVectorElement(name = i, value = parameter[[i]])
    } else if (i == "roundDecimalPlace") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(mzArray)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to extract EIC: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Calculate mass accuracy
  tmp <- do.call(cbind, data[[mzArray]])
  tmp <- if (!is.null(roundDecimalPlace)) round((tmp - targetMZ) / targetMZ * 1e6, digits = roundDecimalPlace) else (tmp - targetMZ) / targetMZ * 1e6
  tmp <- as.data.frame(t(tmp))
  colnames(tmp) <- as.character(targetMZ)
  
  return(cbind(data, tmp))
}