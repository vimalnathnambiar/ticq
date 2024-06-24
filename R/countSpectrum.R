#' Count Spectrum
#'
#' Count the total number of spectral data (spectrum count) available for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data.
#' @param commonColumn A character vector representing names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @returns A data frame with a summarised listing of samples grouped by their common data columns and their respective spectrum count.
countSpectrum <- function(data, commonColumn, spectrumCount) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVector(parameterName = i, parameterValue = parameter[[i]])
    } else {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to count spectrum: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Count the total number of spectral data of each data group
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), .groups = "drop")
  )
}