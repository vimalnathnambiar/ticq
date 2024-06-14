#' Check Spectrum Count
#'
#' Check samples against an accepted threshold limit (%) from the average mean of spectral data (spectrum count) present.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param sampleID Sample ID column name: character
#' @param spectrumCount Spectrum count column name: character
#' @param threshold Accepted threshold limit % (Default:20): double
#' @returns A list containing the statistical result (sample size n, total and mean of spectrum count, and the accepted threshold limit), and two data frames (passed and failed data)
checkSpectrumCount <- function(data, commonColumn, sampleID, spectrumCount, threshold = 20) {
  # Summarise data by recounting the total number of spectral data for each sample
  summarisedData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  # Perform basic statistical analysis
  n <- nrow(summarisedData)
  total <- sum(summarisedData[[spectrumCount]])
  mean <- mean(summarisedData[[spectrumCount]])
  lowerThreshold <- floor(mean - ((threshold / 100) * mean))
  upperThreshold <- ceiling(mean + ((threshold / 100) * mean))
  
  # Filter for samples that have a spectrum count outside of the accepted threshold limit
  summarisedData <- summarisedData %>%
    dplyr::filter(.data[[spectrumCount]] < lowerThreshold | .data[[spectrumCount]] > upperThreshold)
  
  return(
    list(
      sampleSize = n,
      total = total,
      mean = mean,
      threshold = threshold,
      passedData = data %>%
        dplyr::filter(!(.data[[sampleID]] %in% !!summarisedData[[sampleID]])),
      failedData = summarisedData
    )
  )
}