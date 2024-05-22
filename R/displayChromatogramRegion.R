#' displayChromatogramRegion
#' 
#' Add vertical lines to represent the different regions of the chromatogram. 
#' 
#' This function is not exported as it is only used by: 
#' ticq::plotChromatogram()
#' 
#' @import ggplot2
#' 
#' @param plot A ggplot that represents a chromatogram: ggplot
#' @param maxX The maximum value of the x-axis: double
#' @param maxY The maximum value of the y-axis: double
#' @param chromatogramRegion The chromatogram region to be displayed: list
#' @param label The region label to be displayed: character
#' @returns ggplot with the added vertical lines representing the chromatogram region
displayChromatogramRegion <- function(plot, 
                                      maxX, 
                                      maxY, 
                                      chromatogramRegion, 
                                      label) {
  # If region endpoint is NULL
  if (is.null(chromatogramRegion$end)) {
    # Set region endpoint to maximum value of x-axis
    chromatogramRegion$end <- maxX
  }
  
  # Add vertical line to chromatogram
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