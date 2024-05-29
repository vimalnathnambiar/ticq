#' checkUndefinedSampleType
#'
#' Check and filter for samples that have an undefined sample type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn Column names that are common across all samples: character vector
#' @param sampleType Name of the column that represents sample type: character
#' @param spectrumCount Name of the column that represents spectrum count: character
#' @returns List containing two data frames (passed and failed data)
checkUndefinedSampleType <- function(data,
                                     commonColumn,
                                     sampleType,
                                     spectrumCount) {
  # Filter for data that have valid and undefined sample type
  passedData <- data %>%
    dplyr::filter(!is.na(.data[[sampleType]]))
  
  failedData <- ticq::countSpectrum(data = data,
                                    commonColumn = commonColumn,
                                    spectrumCount = spectrumCount) %>%
    dplyr::filter(is.na(.data[[sampleType]]))
  
  return(list(passedData = passedData, failedData = failedData))
}