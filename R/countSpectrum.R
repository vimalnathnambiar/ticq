#' Count Spectrum
#'
#' Count the total number of spectral data (spectrum count) of each sample present within the data frame.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character or character vector
#' @param spectrumCount Spectrum count column name: character
#' @returns A summarised data frame grouped by common columns and their respective total number of spectrum count
countSpectrum <- function(data, commonColumn, spectrumCount) {
  # Sum the total number of spectral data (spectrum count) of each sample
  return(
    data %>%
      dplyr::group_by(across(all_of(commonColumn))) %>%
      dplyr::summarise(!!spectrumCount := n(), .groups = "drop")
  )
}