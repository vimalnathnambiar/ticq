#' Sum Extracted Ion Current (EIC)
#'
#' Sum intensity values of target m/z (EIC) from multiple data columns by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param firstColumnIndex Index of the first data column to summed: double
#' @param lastColumnIndex Index of the last data column to summed: double
#' @returns A data frame grouped by common columns and the summed values of each column specified
sumEIC <- function(data, commonColumn, spectrumCount, firstColumnIndex, lastColumnIndex = NULL) {
  # Data frame to store summed EIC values
  summedDataEIC <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  # Check last data column index
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  # Loop through data columns
  for (i in firstColumnIndex:lastColumnIndex) {
    # Data column name
    y <- colnames(data)[i]
    
    # Sum EIC
    tmp <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), !!y := sum(.data[[y]]), .groups = "keep")
    
    summedDataEIC <- dplyr::left_join(summedDataEIC, tmp, by = commonColumn)
  }
  
  return(summedDataEIC)
}