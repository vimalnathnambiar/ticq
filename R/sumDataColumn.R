#' sumDataColumn
#' 
#' Sum values of a data column for each sample by grouping data by its common columns.
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectrum information of samples: data frame
#' @param commonColumn A vector of column names that are common across all samples: character vector
#' @param sumBy The name of the column that represents the data to be summed: character
#' @returns A data frame grouped by common columns and the sum value of the column
sumDataColumn <- function(data, 
                          commonColumn, 
                          sumBy) {
  # Group data by common columns and sum the values of the data column specified
  sumData <- data %>% 
    dplyr::group_by(across(all_of(commonColumn))) %>%
    dplyr::summarise(sum = sum(.data[[sumBy]]), .groups = "keep")
  
  return(sumData)
}