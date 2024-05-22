#' filterChromatogramRegion
#' 
#' Filter for a chromatogram region of interest.
#' 
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param regionOfInterest A list that represent the start and end point of a regions on the chromatogram (Default: NULL): NULL or list
#' @param filterBy The name of the column that represents the retention time to be filtered: character
#' @returns A data frame containing spliced chromatogram region of interest
filterChromatogramRegion <- function(data, 
                                     regionOfInterest = NULL, 
                                     filterBy) {
  # If region of interest is NULL
  if (is.null(regionOfInterest)) {
    regionOfInterest <- list(start = 0,
                             end = max(data[[filterBy]]))
  } else {
    # If start point is NULL
    if (is.null(regionOfInterest$start)) {
      regionOfInterest$start <- 0
    }
    
    # If end point is NULL
    if (is.null(regionOfInterest$end)) {
      regionOfInterest$end <- max(data[[filterBy]])
    }
  }
  
  # Filter data
  regionData <- data %>%
    dplyr::filter(.data[[filterBy]] > regionOfInterest$start
                  & .data[[filterBy]] <= regionOfInterest$end)
  
  return(regionData)
}