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
  # Count the total number of spectral data of each data group
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), .groups = "drop")
  )
}