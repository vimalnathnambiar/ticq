#' Sum Extracted Ion Current (EIC)
#'
#' Sum the intensity of each target m/z value of each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param firstColumnIndex A numeric value representing the index of the first data column to be summed.
#' @param lastColumnIndex A numeric value representing the index of the last data column to be summed. (Default: `NULL`)
#' @returns A data frame of the MS spectral data grouped by their common data columns and the summed intensity of each target m/z value.
sumEIC <- function(data, commonColumn, spectrumCount, firstColumnIndex, lastColumnIndex = NULL) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, spectrumCount = spectrumCount, firstColumnIndex = firstColumnIndex, lastColumnIndex = lastColumnIndex)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else if (i == "spectrumCount") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "firstColumnIndex") {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "lastColumnIndex") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    }
    
    if ((i == "firstColumnIndex" || (i == "lastColumnIndex" && !is.null(parameter[[i]]))) && (parameter[[i]] < 1 || parameter[[i]] > ncol(data))) {
      stop(paste0("Invalid '", i, "': Data column index out of bound"))
    }
  }
  
  parameter <- c(commonColumn, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to sum EIC: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Sum intensity of each target m/z
  summedDataEIC <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  for (i in firstColumnIndex:lastColumnIndex) {
    y <- colnames(data)[i]
    summedDataEIC <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), !!y := sum(.data[[y]]), .groups = "drop") %>%
      dplyr::left_join(summedDataEIC, ., by = commonColumn)
  }
  
  return(summedDataEIC)
}