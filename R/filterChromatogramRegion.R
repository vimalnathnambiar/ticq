#' filterChromatogramRegion
#'
#' Filter for retention time of a chromatogram region of interest.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region (Default: NULL): NULL or list
#' @param retentionTime Retention time column name: character
#' @returns A data frame containing spectral data that falls within the retention time of the chromatogram region of interest
filterChromatogramRegion <- function(data, regionOfInterest = NULL, retentionTime) {
  # Check if chromatogram region of interest is NULL
  if (is.null(regionOfInterest)) {
    regionOfInterest <- list(start = 0, end = max(data[[retentionTime]]))
  } else {
    # If start time point is NULL
    if (is.null(regionOfInterest$start)) {
      regionOfInterest$start <- 0
    }
    
    # If end time point is NULL
    if (is.null(regionOfInterest$end)) {
      regionOfInterest$end <- max(data[[retentionTime]])
    }
  }
  
  # Filter data by retention time
  regionData <- data %>%
    dplyr::filter(.data[[retentionTime]] > regionOfInterest$start & .data[[retentionTime]] <= regionOfInterest$end)
  
  return(regionData)
}