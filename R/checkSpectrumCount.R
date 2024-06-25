#' Check Spectrum Count
#'
#' Check the total number of spectral data (spectrum count) of each sample against an accepted threshold limit (%) from the average mean.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data.
#' @param commonColumn A character vector representing names of the common data columns to be used for data grouping.
#' @param sampleID A character string representing the name of the sample ID column.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param threshold A numeric value representing the accepted threshold limit (%). (Default: `20`)
#' @returns A list containing the statistical result (sample size, sum, and mean) and
#' two data frames (`passedData` containing spectral data that has a spectrum count within the threshold limit and `failedData` containing a summarised listing of samples that falls outside the threshold limit).
checkSpectrumCount <- function(data, commonColumn, sampleID, spectrumCount, threshold = 20) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleID = sampleID, spectrumCount = spectrumCount, threshold = threshold)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVector(parameterName = i, parameterValue = parameter[[i]])
    } else if (i == "threshold") {
      validateNumericValue(parameterName = i, parameterValue = parameter[[i]])
    } else {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, sampleType, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to check spectrum count: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Perform statistical analysis on spectrum count and filter samples outside the accepted threshold limit
  summarisedData <- countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  stat <- generateStat(summarisedData[[spectrumCount]])
  
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