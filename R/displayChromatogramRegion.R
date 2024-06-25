#' Display Chromatogram Region
#'
#' Display vertical lines representing the start and end time points of a chromatogram region.
#'
#' @import ggplot2
#'
#' @param plot A ggplot object of a chromatogram.
#' @param maxX A numeric value representing the maximum value on the x-axis.
#' @param maxY A numeric value representing the maximum value on the y-axis.
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region. (Options: Use `ticq::configureChromatogram()$regionOfInterest`)
#' @param label A character string representing the chromatogram region label, (Default: `NULL`)
#' @returns A ggplot object with the added vertical lines representing the chromatogram region start and end time points
displayChromatogramRegion <- function(plot, maxX, maxY, regionOfInterest, label = NULL) {
  # Validate parameters
  parameter <- list(maxX = maxX, maxY = maxY, regionOfInterest = regionOfInterest, label = label)
  for (i in names(parameter)) {
    if (i == "maxX" || i == "maxY") {
      validateNumericValue(parameterName = i, parameterValue = parameter[[i]])
    } else if (i == "label" && !is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a character string of length 1"))
    } else if (i == "regionOfInterest") {
      pattern <- c("start", "end")
      if (length(parameter[[i]]) != 2 || !is.list(parameter[[i]])) {
        stop(paste0("Invalid '", i, "': Must be a list containing 2 time points (start, end)"))
      } else if (!all(pattern %in% names(parameter[[i]]))) {
        stop(paste0("Invalid '", i, "': Missing one or more time point (", paste(pattern[!pattern %in% names(parameter[[i]])], collapse = ", "), ")"))
      } else if (length(parameter[[i]][[pattern[[1]]]]) != 1 || !is.numeric(parameter[[i]][[pattern[[1]]]])) {
        stop(paste0("Invalid '", pattern[[1]], "': Must be a numeric value of length 1"))
      } else if (!is.null(parameter[[i]][[pattern[[2]]]]) && (length(parameter[[i]][[pattern[[2]]]]) != 1 || !is.numeric(parameter[[i]][[pattern[[2]]]]))) {
        stop(paste0("Invalid '", pattern[[2]], "': Must either be NULL or a numeric value of length 1"))
      }
    }
  }
  
  # Check region end time point
  if (is.null(regionOfInterest$end)) {
    regionOfInterest$end <- maxX
  }
  
  # Add vertical lines and annotate region
  return(
    plot +
      ggplot2::geom_vline(xintercept = regionOfInterest$start, linetype = "dashed", color = "grey") +
      ggplot2::annotate( "text", label = label, x = (regionOfInterest$start + regionOfInterest$end) / 2, y = (maxY / 4) * 3, angle = 90, size = 2)
  )
}