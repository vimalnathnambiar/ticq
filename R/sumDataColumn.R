#' Sum Data Column
#'
#' Sum the values of a single data column for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names representing common data to be used for data grouping: character
#' @param sumColumn Column name of the data to be summed: character
#' @returns A data frame grouped by common columns and the sum value of the data column specified
sumDataColumn <- function(data, commonColumn, sumColumn) {
  # Sum values of the data column for each data group
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(sum = sum(.data[[sumColumn]]), .groups = "drop")
  )
}