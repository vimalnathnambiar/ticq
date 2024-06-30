#' Sum Data Column
#'
#' Sum the values of a single data column for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param sumColumn A character string representing the name of the data column with values to be summed.
#' @returns A data frame of the MS spectral data summary grouped by their common data columns and the sum value of the data column.
sumDataColumn <- function(data, commonColumn, sumColumn) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sumColumn = sumColumn)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, sumColumn)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to sum data column: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Sum data column
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(sum = sum(.data[[sumColumn]]), .groups = "drop")
  )
}