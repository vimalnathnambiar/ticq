#' plotChromatogram
#' 
#' Plot chromatogram data. 
#' 
#' @import ggplot2
#' 
#' @export
#' @param data Data frame that contains plot data: data frame
#' @param x Name of the column that represents the continuous data series for the x-axis: character
#' @param y Name of the column that represents the continuous data series for the y-axis: character
#' @param colour Name of the column used for colour grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Chromatogram"): character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param labelX x-axis label (Default: Value used for x): NULL or character
#' @param labelY y-axis label (Default:Value used for y): NULL or character
#' @param legend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param chromatogramRegion List that represents the different chromatogram regions of interest (Default: NULL): NULL or list
plotChromatogram <- function(data, 
                             x, 
                             y, 
                             colour = NULL, 
                             title = "Chromatogram", 
                             subtitle = NULL, 
                             caption = NULL, 
                             labelX = x, 
                             labelY = y, 
                             legend = colour, 
                             chromatogramRegion = NULL) {
  tryCatch({
    # Plot chromatogram
    chromatogram <- ggplot2::ggplot(data = data,
                                    aes(x = .data[[x]], y = .data[[y]]))
    
    # Add colour grouping
    if (is.null(colour)) {
      chromatogram <- chromatogram +
        ggplot2::geom_line(alpha = 0.25)
    } else {
      chromatogram <- chromatogram + 
        ggplot2::geom_line(aes(colour = .data[[colour]],
                               group = ifelse(.data[[colour]] == "Historical", 1, 2)),
                           alpha = 0.25)
    }
    
    # Add theme and labels
    chromatogram <- chromatogram +
      ggplot2::theme(panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_text(size = 8),
                     legend.text = element_text(size = 8)) + 
      ggplot2::labs(title = title,
                    subtitle = subtitle,
                    caption = caption,
                    x = labelX,
                    y = labelY,
                    colour = legend)
    
    # Display chromatogram regions of interest
    if (!is.null(chromatogramRegion)) {
      # Mass calibration region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$massCal,
                                                label = "Mass Calibration")
      
      # Analyte region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$analyte,
                                                label = "Analyte")
      
      # Wash region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$wash,
                                                label = "Wash")
    }
    
    print(chromatogram)
  },
  warning = function(w) {
    print(paste0("Unable to generate Chromatogram - ", w))
  }, 
  error = function(e) {
    print(paste0("Unable to generate Chromatogram - ", e))
  })
}