#' sumEIC
#'
#' Sum EIC values by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn Column names that are common across all samples: character vector
#' @param spectrumCount Name of the column that represents spectrum count: character
#' @param startIDX Index of the first column to sum: double
#' @param endIDX Index of the last column to sum: double
#' @returns A data frame grouped by common columns and the summed EIC values for each column specified
sumEIC <- function(data,
                   commonColumn,
                   spectrumCount,
                   startIDX,
                   endIDX = NULL) {
  # Base data frame
  result <- ticq::countSpectrum(data = data,
                                commonColumn = commonColumn,
                                spectrumCount = spectrumCount)
  
  # If index of last column is NULL
  if (is.null(endIDX)) {
    endIDX <- ncol(data)
  }
  
  # Loop through target m/z columns
  for (i in startIDX:endIDX) {
    # Column name to be summed
    y <- colnames(data)[i]
    
    # Group data by common columns, and sum the TEIC
    tmp <- data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), !!y := sum(.data[[y]]), .groups = "keep")
    
    # Append summed data column to base data frame
    result <- dplyr::left_join(result, tmp, by = commonColumn)
  }
  
  return(result)
}