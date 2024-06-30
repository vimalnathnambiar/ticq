#' Calculate Average Mass Accuracy
#'
#' Calculate the average mass accuracy (in ppm) of each target m/z for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param firstColumnIndex A numeric value representing the index of the first data column to be averaged.
#' @param lastColumnIndex A numeric value representing the index of the last data column to be averaged. (Default: `NULL`)
#' @param roundDecimalPlace A numeric value representing the number of decimal places to be used for precision value rounding. (Default: `NULL`)
#' @returns A data frame of the MS spectral data summary grouped by their common data columns and the average mass accuracy of each target m/z value.
calculateAverageMassAccuracy <- function(data,
                                         commonColumn,
                                         spectrumCount,
                                         firstColumnIndex,
                                         lastColumnIndex = NULL,
                                         roundDecimalPlace = NULL) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    commonColumn = commonColumn,
    spectrumCount = spectrumCount,
    firstColumnIndex = firstColumnIndex,
    lastColumnIndex = lastColumnIndex,
    roundDecimalPlace = roundDecimalPlace
  )
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else if (i == "spectrumCount") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "firstColumnIndex") {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "lastColumnIndex" || i == "roundDecimalPlace") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    }
    
    if ((i == "firstColumnIndex" || (i == "lastColumnIndex" && !is.null(parameter[[i]]))) && (parameter[[i]] < 1 || parameter[[i]] > ncol(data))) {
      stop(paste0("Invalid '", i, "': Data column index out of bound"))
    }
  }
  
  parameter <- c(commonColumn, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to display ", title, ": Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  # Calculate average mass accuracy
  averagedDataMassAccuracy <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  for (i in firstColumnIndex:lastColumnIndex) {
    y <- colnames(data)[i]
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