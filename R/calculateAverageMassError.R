#' Calculate Average Mass Error
#'
#' Calculate the average mass error (in ppm) of target m/z from multiple data columns by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param startIDX Index of the first data column to averaged: double
#' @param endIDX Index of the last data column to averaged: double
#' @param roundDecimal Number of decimal places for precision value rounding (Default: NULL): NULL or double
#' @returns A data frame grouped by common columns and the average ppm error for each target m/z value
calculateAverageMassError <- function(data,
                                      commonColumn,
                                      spectrumCount,
                                      startIDX,
                                      endIDX = NULL,
                                      roundDecimal = NULL) {
  # Base data frame to append average data
  averagedDataMassError <- ticq::countSpectrum(data = data,
                                               commonColumn = commonColumn,
                                               spectrumCount = spectrumCount)
  
  # If index of last data column is NULL
  if (is.null(endIDX)) {
    endIDX <- ncol(data)
  }
  
  # Loop through data columns
  for (i in startIDX:endIDX) {
    # Column name to be averaged
    y <- colnames(data)[i]
    
    # Group data by common columns and average mass error values
    tmp <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(),
                       !!y := if (!is.null(roundDecimal)) round(sum(.data[[y]]) / spectrumCount, digits = roundDecimal) else sum(.data[[y]]) / spectrumCount,
                       .groups = "keep")
    
    averagedDataMassError <- dplyr::left_join(averagedDataMassError, tmp, by = commonColumn)
  }
  
  return(averagedDataMassError)
}