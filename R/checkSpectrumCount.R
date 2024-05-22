#' checkSpectrumCount
#' 
#' Check and filter for samples against a specific threshold set on the average spectrum count (mean) of all samples being processed.
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn A vector of column names that are common across all samples: character vector
#' @param sampleID The name of the column that represents sample ID: character
#' @param spectrumCount The name of the column that represents spectrum count: character
#' @param threshold Accepted threshold percentage (Default: 20): double
#' @returns A list containing statistical result generated from the spectrum check (sample size, total and mean of spectrum count, and the threshold value), and two data frames (samples that passed and failed the check)
checkSpectrumCount <- function(data, 
                               commonColumn, 
                               sampleID, 
                               spectrumCount,
                               threshold = 20) {
  # Summarise data and obtain total number of spectrum of each sample
  summarisedData <- ticq::countSpectrum(data = data,
                                        commonColumn = commonColumn,
                                        spectrumCount = spectrumCount)
  
  # Perform basic statistical analysis on spectrum count
  # Sample size
  n <- nrow(summarisedData)
  
  # Sum
  total <- sum(summarisedData[[spectrumCount]])
  
  # Mean
  mean <- mean(summarisedData[[spectrumCount]])
  
  # Threshold
  threshold <- floor(mean - ((threshold / 100) * mean))
  
  # Filter for passed and failed samples
  failedData <- summarisedData %>% 
    dplyr::filter(.data[[spectrumCount]] < threshold)
  
  passedData <- data %>%
    dplyr::filter(!(.data[[sampleID]] %in% !!failedData[[sampleID]]))
  
  return(list(sampleSize = n,
              total = total,
              mean = mean,
              threshold = threshold,
              passedData = passedData,
              failedData = failedData))
}