#' Display Chromatogram Region
#'
#' Display vertical lines highlighting the start and end time points of a chromatogram region on a chromatogram plot.
#'
#' This function is not exported as it is only used by:
#' ticq::displayChromatogram()
#' ticq::displayMultipleChromatogram()
#'
#' @import ggplot2
#'
#' @param plot A ggplot object of a chromatogram: ggplot
#' @param maxX x-axis maximum value: double
#' @param maxY y-axis maximum value: double
#' @param chromatogramRegion Chromatogram region to be displayed: list
#' @param label Chromatogram region label: character
#' @returns A ggplot object with the added vertical lines displaying the chromatogram region start and end time points
displayChromatogramRegion <- function(plot, maxX, maxY, chromatogramRegion, label) {
  # Check chromatogram region endpoint
  if (is.null(chromatogramRegion$end)) {
    chromatogramRegion$end <- maxX
  }
  
  # Add vertical lines and annotate the chromatogram region
  return(
    plot +
      ggplot2::geom_vline(xintercept = chromatogramRegion$start, linetype = "dashed", color = "grey") +
      ggplot2::annotate( "text", label = label, x = (chromatogramRegion$start + chromatogramRegion$end) / 2, y = (maxY / 4) * 3, angle = 90, size = 2)
  )
}