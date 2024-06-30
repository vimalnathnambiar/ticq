#' Count Spectrum
#'
#' Count the total number of MS spectral data (spectrum count) of each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @returns A data frame of MS spectral data summary grouped by their common data columns and its associated spectrum count.
countSpectrum <- function(data, commonColumn, spectrumCount) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to count spectrum: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Count spectral data
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), .groups = "drop")
  )
}