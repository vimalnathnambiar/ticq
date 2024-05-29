#' displayChromatogramRegion
#'
#' Add vertical lines to represent the different regions of the chromatogram.
#'
#' This function is not exported as it is only used by:
#' ticq::plotChromatogram()
#'
#' @import ggplot2
#'
#' @param plot ggplot object that represents a chromatogram: ggplot
#' @param maxX Maximum value of the x-axis: double
#' @param maxY Maximum value of the y-axis: double
#' @param chromatogramRegion Chromatogram region to be displayed: list
#' @param label Chromatogram region label: character
#' @returns ggplot object with the added vertical lines displaying the chromatogram region start and end points
displayChromatogramRegion <- function(plot,
                                      maxX,
                                      maxY,
                                      chromatogramRegion,
                                      label) {
  # If chromatogram region endpoint is NULL
  if (is.null(chromatogramRegion$end)) {
    # Set region endpoint to maximum value of x-axis
    chromatogramRegion$end <- maxX
  }
  
  # Add vertical lines for the chromatogram region start and end points
  plot <- plot +
    ggplot2::geom_vline(xintercept = chromatogramRegion$start,
                        linetype = "dashed",
                        color = "darkgrey") +
    ggplot2::annotate("text",
                      label = label,
                      x = (chromatogramRegion$start + chromatogramRegion$end) / 2,
                      y = (maxY / 4) * 3,
                      angle = 90,
                      size = 2)
  
  return(plot)
}