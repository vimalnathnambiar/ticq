#' Filter Chromatogram Region
#'
#' Filter for spectral data that falls between the start and end time points of a chromatogram region of interest.
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame containing MS spectral data.
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region of interest.
#' (Default: `NULL`; Options: `configureChromatogramRegion()$regionOfInterest` or `configureChromatogramRegion()[["regionOfInterest"]]`)
#' @param retentionTime A character string representing the name of the retention time column.
#' @returns A data frame of MS spectral data that has a retention time between the start and end time point of the specified chromatogram region of interest.
filterChromatogramRegion <- function(data, regionOfInterest = NULL, retentionTime) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(regionOfInterest = regionOfInterest, retentionTime = retentionTime)
  for (i in names(parameter)) {
    if (i == "regionOfInterest" && !is.null(parameter[[i]]) && !validateRegionOfInterest(name = i, value = parameter[[i]])) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a list of the chromatogram region data of interest"))
    } else if (i == "retentionTime") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(retentionTime)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to filter chromatogram region: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
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
  
  # Filter chromatogram region spectral data
  return(
    data %>%
      dplyr::filter(.data[[retentionTime]] > regionOfInterest$start & .data[[retentionTime]] <= regionOfInterest$end)
  )
}