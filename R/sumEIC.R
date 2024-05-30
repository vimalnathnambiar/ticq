#' sumEIC
#'
#' Sum EIC values of multiple data columns by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param startIDX Index of the first data column to summed: double
#' @param endIDX Index of the last data column to summed: double
#' @returns A data frame grouped by common columns and the summed values of each column specified
sumEIC <- function(data,
                   commonColumn,
                   spectrumCount,
                   startIDX,
                   endIDX = NULL) {
  # Base data frame to append summed data to
  sumData <- ticq::countSpectrum(data = data,
                                 commonColumn = commonColumn,
                                 spectrumCount = spectrumCount)
  
  # If index of last data column is NULL
  if (is.null(endIDX)) {
    endIDX <- ncol(data)
  }
  
  # Loop through data columns
  for (i in startIDX:endIDX) {
    # Column name to be summed
    y <- colnames(data)[i]
    
    # Group data by common columns, and sum values
    tmp <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), !!y := sum(.data[[y]]), .groups = "keep")
    
    # Append summed data column to base data frame
    sumData <- dplyr::left_join(sumData, tmp, by = commonColumn)
  }
  
  return(sumData)
}