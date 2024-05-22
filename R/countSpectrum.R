#' countSpectrum
#' 
#' Count and summarise the total number of spectral data for each sample present within the data frame.
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn A vector of column names that are common across all samples: character vector
#' @param spectrumCount The name of the column that represents spectrum count: character
#' @returns A summarised data frame grouped by common columns and their respective number of spectrum available
countSpectrum <- function(data, 
                          commonColumn, 
                          spectrumCount) {
  # Group data by common columns and sum the total number of rows
  summarisedData <- data %>%
    dplyr::group_by(across(all_of(commonColumn))) %>%
    dplyr::summarise(!!spectrumCount := n(), .groups = "keep")
  
  return(summarisedData)
}