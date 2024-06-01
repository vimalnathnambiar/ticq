#' Check Undefined Sample Type
#'
#' Check and filter for samples that have an undefined sample type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param sampleType Sample type column name: character
#' @param spectrumCount Spectrum count column name: character
#' @returns A list containing two data frames (passed and failed data)
checkUndefinedSampleType <- function(data,
                                     commonColumn,
                                     sampleType,
                                     spectrumCount) {
  # Filter for data that have valid (passed) and undefined (failed) sample type 
  passedData <- data %>%
    dplyr::filter(!is.na(.data[[sampleType]]))
  
  failedData <- ticq::countSpectrum(data = data,
                                    commonColumn = commonColumn,
                                    spectrumCount = spectrumCount) %>%
    dplyr::filter(is.na(.data[[sampleType]]))
  
  return(list(passedData = passedData, failedData = failedData))
}