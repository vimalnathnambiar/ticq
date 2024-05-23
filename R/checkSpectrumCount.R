#' checkSpectrumCount
#' 
#' Check and filter samples against a specific threshold limit set on the average mean of spectral data (spectrum count) present.
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn Column names that are common across all samples: character vector
#' @param sampleID Name of the column that represents sample ID: character
#' @param spectrumCount Name of the column that represents spectrum count: character
#' @param threshold Accepted threshold limit % (Default: 20): double
#' @returns List containing the statistical result (sample size, total and mean of spectrum count, and the threshold limit value), and two data frames (passed and failed data)
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
    
    # Threshold limit %
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