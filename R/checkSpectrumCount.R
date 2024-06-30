#' Check Spectrum Count
#'
#' Check the total number of MS spectral data (spectrum count) of each data group against an accepted threshold limit (%) (+/-) from the average mean.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param sampleID A character string representing the name of the sample ID column.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param threshold A numeric value representing the accepted threshold limit (%). (Default: `20`)
#' @returns A list with statistical information (sample size, sum, and mean) and two data frames (data that passed and failed the spectrum count check).
checkSpectrumCount <- function(data, commonColumn, sampleID, spectrumCount, threshold = 20) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sampleID = sampleID, spectrumCount = spectrumCount, threshold = threshold)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else if (i == "sampleID" || i == "spectrumCount") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "threshold") {
      validateNumericValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, sampleID, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to check spectrum count: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Check spectrum count
  summarisedData <- countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  stat <- generateStatistic(data = summarisedData[[spectrumCount]])
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