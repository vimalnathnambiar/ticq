#' Check Spectrum Count
#'
#' Check the total number of spectral data (spectrum count) of each sample against an accepted threshold limit (%) from the average mean.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names representing common data to be used for data grouping: character
#' @param sampleID Sample ID column name: character
#' @param spectrumCount Spectrum count column name: character
#' @param threshold Accepted threshold limit % (Default: 20): numeric
#' @returns A list containing the statistical result (sample size n, sum and mean of spectrum count, and the accepted threshold limit), and two data frames (passed and failed data)
checkSpectrumCount <- function(data, commonColumn, sampleID, spectrumCount, threshold = 20) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Data frame is empty")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleID = sampleID, spectrumCount = spectrumCount, threshold = threshold)
  for (i in names(parameter)) {
    if ((i == "commonColumn" && (length(parameter[[i]]) == 0 || !is.character(parameter[[i]]) || any(is.na(parameter[[i]])) || any(parameter[[i]] == ""))) ||
        (i == "threshold" && (length(parameter[[i]]) != 1 || !is.numeric(parameter[[i]]))) ||
        (i != "commonColumn" && i != "threshold" && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == ""))) {
      if (i == "commonColumn") {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 or more matching 1 or multiple column names in 'data'"))
      } else if (i == "threshold") {
        stop(paste0("Invalid '", i, "': Must be a numeric value of length 1"))
      } else {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 matching a column name in 'data'"))
      }
    }
  }
  
  parameter <- c(commonColumn, sampleType, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0(
      "Unable to check spectrum count: Missing one or more data columns (",
      paste(parameter[!parameter %in% colnames(data)], collapse = ", "),
      ")"
    ))
  }
  
  # Perform statistical analysis on spectrum count and filter samples outside the accepted threshold limit
  summarisedData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  stat <- ticq::generateStat(summarisedData[[spectrumCount]])
  
  mean <- stat$mean
  lowerThreshold <- floor(mean - ((threshold / 100) * mean))
  upperThreshold <- ceiling(mean + ((threshold / 100) * mean))
  summarisedData <- summarisedData %>%
    dplyr::filter(.data[[spectrumCount]] < lowerThreshold | .data[[spectrumCount]] > upperThreshold)
  
  return(
    list(
      n = stat$n,
      sum = stat$sum,
      mean = mean,
      threshold = threshold,
      passedData = data %>%
        dplyr::filter(!(.data[[sampleID]] %in% !!summarisedData[[sampleID]])),
      failedData = summarisedData
    )
  )
}