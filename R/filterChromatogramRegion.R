#' Filter Chromatogram Region
#'
#' Filter spectral data that falls between the respective start and end time points of a chromatogram region of interest.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region. (Default: `NULL`; Options: Use `ticq::configureChromatogram()$regionOfInterest`)
#' @param retentionTime A character string representing the name of the retention time column.
#' @returns A data frame containing spectral data that falls between the start and end time point of the specified chromatogram region of interest.
filterChromatogramRegion <- function(data, regionOfInterest = NULL, retentionTime) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  if (!is.null(regionOfInterest)) {
    pattern <- c("start", "end")
    if (length(regionOfInterest) != 2 || !is.list(regionOfInterest)) {
      stop("Invalid 'regionOfInterest': Must be a list containing 2 time points (start, end)")
    } else if (!all(pattern %in% names(regionOfInterest))) {
      stop(paste0("Invalid 'regionOfInterest': Missing one or more time point (", paste(pattern[!pattern %in% names(regionOfInterest)], collapse = ", "), ")"))
    } else {
      for (i in pattern) {
        if (!is.null(regionOfInterest[[i]]) && (length(regionOfInterest[[i]]) != 1 || !is.numeric(regionOfInterest[[i]]))) {
          stop(paste0("Invalid '", i, "': Must either be NULL or a numeric value of length 1"))
        }
      }
    }
  }
  
  validateCharacterString(parameterName = "retentionTime", parameterValue = retentionTime)
  if (!retentionTime %in% colnames(data)) {
    stop("Unable to filter chromatogram region: Missing retention time data column")
  }
  
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