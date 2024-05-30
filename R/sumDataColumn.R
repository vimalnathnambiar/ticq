#' sumDataColumn
#'
#' Sum values of a data column by grouping using its common columns.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param sumBy Column name of the data to be summed: character
#' @returns A data frame grouped by common columns and the sum value of the data column specified
sumDataColumn <- function(data, commonColumn, sumBy) {
  # Group data by common columns and sum the values in the data column
  sumData <- data %>%
    dplyr::group_by(across(all_of(commonColumn))) %>%
    dplyr::summarise(sum = sum(.data[[sumBy]]), .groups = "keep")
  
  return(sumData)
}