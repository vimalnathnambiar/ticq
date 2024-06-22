#' Filter Chromatogram Region
#'
#' Filter spectral data that falls between the respective start and end time points of a chromatogram region of interest.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region (Default: NULL, Options: Options: ticq::configureChromatogram()$region): NULL or list
#' @param retentionTime Retention time column name: character
#' @returns A data frame containing spectral data that falls between the start and end time point of a specific chromatogram region of interest
filterChromatogramRegion <- function(data, regionOfInterest = NULL, retentionTime) {
  # Check region of interest
  if (is.null(regionOfInterest)) {
    regionOfInterest <- list(start = 0, end = max(data[[retentionTime]]))
  } else {
    if (is.null(regionOfInterest$start)) {
      regionOfInterest$start <- 0
    }
    
    if (is.null(regionOfInterest$end)) {
      regionOfInterest$end <- max(data[[retentionTime]])
    }
  }
  
  # Filter spectral data
  return(
    data %>%
      dplyr::filter(.data[[retentionTime]] > regionOfInterest$start & .data[[retentionTime]] <= regionOfInterest$end)
  )
}