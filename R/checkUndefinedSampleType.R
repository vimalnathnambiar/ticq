#' Check Undefined Sample Type
#'
#' Check for samples that have an undefined sample type.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names representing common data to be used for data grouping: character
#' @param sampleType Sample type column name: character
#' @param spectrumCount Spectrum count column name: character
#' @returns A list containing two data frames (passed and failed data)
checkUndefinedSampleType <- function(data, commonColumn, sampleType, spectrumCount) {
  # Filter for data that have valid (passed) and undefined (failed) sample type 
  return(
    list(
      passedData = data %>%
        dplyr::filter(!is.na(.data[[sampleType]])),
      failedData = ticq::countSpectrum(
        data = data,
        commonColumn = commonColumn,
        spectrumCount = spectrumCount
      ) %>%
        dplyr::filter(is.na(.data[[sampleType]]))
    )
  )
}