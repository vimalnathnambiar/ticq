#' Check Undefined Sample Type
#'
#' Check data for undefined sample type.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param sampleType A character string representing the name of the sample type column.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @returns A list with two data frames (data that passed and failed the undefined sample type check).
checkUndefinedSampleType <- function(data, commonColumn, sampleType, spectrumCount) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleType = sampleType, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    }
  }

  parameter <- c(commonColumn, sampleType, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0(
      "Unable to check undefined sample type: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"
    ))
  }
  
  # Check undefined sample type
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