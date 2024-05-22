#' checkUndefinedSampleType
#' 
#' Check and filter for samples that have an undefined sample type.
#' 
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn A vector of column names that are common across all samples: character vector
#' @param sampleType The name of the column that represents sample type: character
#' @param spectrumCount The name of the column that represents spectrum count: character
#' @returns A list containing two data frames (samples that passed and failed the check)
checkUndefinedSampleType <- function(data, 
                                     commonColumn, 
                                     sampleType, 
                                     spectrumCount) {
  # Filter for data that have valid sample type
  passedData <- data %>% 
    dplyr::filter(!is.na(.data[[sampleType]]))
  
  # Filter for data that has undefined sample type
  failedData <- ticq::countSpectrum(data = data,
                              commonColumn = commonColumn,
                              spectrumCount = spectrumCount) %>%
    dplyr::filter(is.na(.data[[sampleType]]))
  
  return(list(passedData = passedData,
              failedData = failedData))
}