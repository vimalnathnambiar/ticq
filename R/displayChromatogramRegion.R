#' Display Chromatogram Region
#'
#' Display vertical lines on a chromatogram plot representing the start and end time points of a chromatogram region of interest.
#'
#' @import ggplot2
#'
#' @param plot A ggplot object of a chromatogram.
#' @param maxX A numeric value representing the maximum value on the x-axis.
#' @param maxY A numeric value representing the maximum value on the y-axis.
#' @param regionOfInterest A list representing the start and end time points of a chromatogram region of interest.
#' @param label A character string representing the chromatogram region label.
#' @returns A ggplot object with added dashed vertical lines representing the chromatogram region start and end time points.
displayChromatogramRegion <- function(plot, maxX, maxY, regionOfInterest, label) {
  # Validate parameters
  if (is.null(regionOfInterest$end)) {
    regionOfInterest$end <- maxX
  }
  
  # Display chromatogram region
  return(
    plot +
      ggplot2::geom_vline(xintercept = regionOfInterest$start, linetype = "dashed", color = "grey") +
      ggplot2::annotate( "text", label = label, x = (regionOfInterest$start + regionOfInterest$end) / 2, y = (maxY / 4) * 3, angle = 90, size = 2)
  )
}