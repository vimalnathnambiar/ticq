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
  # Summarise data by spectrum count
  summarisedData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  # Perform statistical analysis
  stat <- ticq::generateStat(summarisedData[[spectrumCount]])
  
  mean <- stat$mean
  lowerThreshold <- floor(mean - ((threshold / 100) * mean))
  upperThreshold <- ceiling(mean + ((threshold / 100) * mean))
  
  # Filter samples that have a spectrum count outside the accepted threshold limit
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