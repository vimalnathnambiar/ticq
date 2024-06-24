#' Sum Data Column
#'
#' Sum the values of a single data column for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data.
#' @param commonColumn A character vector representing names of the common data columns to be used for data grouping.
#' @param sumColumn A character string representing the name of the data column to be summed.
#' @returns A data frame grouped by common data columns and the sum value of the data column specified.
sumDataColumn <- function(data, commonColumn, sumColumn) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, sumColumn = sumColumn)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVector(parameterName = i, parameterValue = parameter[[i]])
    } else {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, sumColumn)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to sum data column: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Sum data column for each data group
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(sum = sum(.data[[sumColumn]]), .groups = "drop")
  )
}