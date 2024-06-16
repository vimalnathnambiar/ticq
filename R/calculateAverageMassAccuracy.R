#' Calculate Average Mass Accuracy
#'
#' Calculate the average mass accuracy (in ppm) of each target m/z from multiple data columns by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param firstColumnIndex Index of the first data column to averaged: double
#' @param lastColumnIndex Index of the last data column to averaged: double
#' @param roundDecimalPlace Number of decimal places for precision value rounding (Default: NULL): NULL or double
#' @returns A data frame grouped by common columns and the average ppm accuracy for each target m/z value
calculateAverageMassAccuracy <- function(data,
                                         commonColumn,
                                         spectrumCount,
                                         firstColumnIndex,
                                         lastColumnIndex = NULL,
                                         roundDecimalPlace = NULL) {
  # Data frame to store average mass accuracy data
  averagedDataMassAccuracy <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  # Check last data column index
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  # Loop through data columns
  for (i in firstColumnIndex:lastColumnIndex) {
    # Data column name
    y <- colnames(data)[i]
    
    # Calculate average mass accuracy
    averagedDataMassAccuracy <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(
        !!spectrumCount := n(),
        !!y := if (!is.null(roundDecimalPlace)) round(sum(.data[[y]]) / spectrumCount, digits = roundDecimalPlace) else sum(.data[[y]]) / spectrumCount,
        .groups = "drop"
      ) %>%
      dplyr::left_join(averagedDataMassAccuracy, ., by = commonColumn)
  }
  
  return(averagedDataMassAccuracy)
}