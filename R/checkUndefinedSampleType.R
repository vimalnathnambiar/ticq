#' Check Undefined Sample Type
#'
#' Check for samples that have an undefined sample type.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data.
#' @param commonColumn A character vector representing names of the common data columns to be used for data grouping.
#' @param sampleType A character string representing the name of the sample type column.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @returns A list containing two data frames (`passedData` containing spectral data of samples with a defined sample type and `failedData` containing a summarised listing of samples with an undefined sample type).
checkUndefinedSampleType <- function(data, commonColumn, sampleType, spectrumCount) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleType = sampleType, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVector(parameterName = i, parameterValue = parameter[[i]])
    } else {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    }
  }

  parameter <- c(commonColumn, sampleType, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to check undefined sample type: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Filter data with valid (passed) and undefined (failed) sample type 
  return(
    list(
      passedData = data %>%
        dplyr::filter(!is.na(.data[[sampleType]])),
      failedData = countSpectrum(
        data = data,
        commonColumn = commonColumn,
        spectrumCount = spectrumCount
      ) %>%
        dplyr::filter(is.na(.data[[sampleType]]))
    )
  )
}