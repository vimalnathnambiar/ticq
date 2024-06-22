#' Count Spectrum
#'
#' Count the total number of spectral data (spectrum count) available for each data group.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names representing common data to be used for data grouping: character
#' @param spectrumCount Spectrum count column name: character
#' @returns A summarised data frame grouped by common columns and their respective total number of spectrum count
countSpectrum <- function(data, commonColumn, spectrumCount) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Data frame is empty")
  }
  
  parameter <- list(commonColumn = commonColumn, spectrumCount = spectrumCount)
  for (i in names(parameter)) {
    if ((i == "commonColumn" && (length(parameter[[i]]) == 0 || !is.character(parameter[[i]]) || any(is.na(parameter[[i]])) || any(parameter[[i]] == ""))) ||
        (i != "commonColumn" && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == ""))) {
      if (i == "commonColumn") {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 or more matching 1 or multiple column names in 'data'"))
      } else {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 matching a column name in 'data'"))
      }
    }
  }
  
  parameter <- c(commonColumn, spectrumCount)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0(
      "Unable to count spectrum: Missing one or more data columns (",
      paste(parameter[!parameter %in% colnames(data)], collapse = ", "),
      ")"
    ))
  }
  
  # Count the total number of spectral data of each data group
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), .groups = "drop")
  )
}