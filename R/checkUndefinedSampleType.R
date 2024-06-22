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
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Data frame is empty")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleType = sampleType, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if ((i == "commonColumn" && (length(parameter[[i]]) == 0 || !is.character(parameter[[i]]) || any(is.na(parameter[[i]])) || any(parameter[[i]] == ""))) ||
        (i != "commonColumn" && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == ""))) {
      if (i == "commonColumn") {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 or more matching 1 or multiple column names in 'data'"))
      } else {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 matching a column name in 'data'"))
      }
    }
  }
  
  parameter <- c(commonColumn, sampleType, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0(
      "Unable to check undefined sample type: Missing one or more data columns (",
      paste(parameter[!parameter %in% colnames(data)], collapse = ", "),
      ")"
    ))
  }
  
  # Filter data with valid (passed) and undefined (failed) sample type 
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