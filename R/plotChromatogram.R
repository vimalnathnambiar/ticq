#' plotChromatogram
#' 
#' Plot chromatogram data using continuous data for x- and y-axis. 
#' 
#' @import ggplot2
#' 
#' @export
#' @param data Data frame that contains the data to plot: data frame
#' @param x The name of the column that represents the continuous data series for the x-axis: character
#' @param y The name of the column that represents the continuous data series that represents the y-axis: character
#' @param colour The name of the column that will be used to colour categorise the data (Default: NULL): NULL or character
#' @param title The title for the plot (Default: "Chromatogram"): character
#' @param subtitle The subtitle for the plot (Default: NULL): NULL or character
#' @param caption The caption for the plot (Default: NULL): NULL or character
#' @param labelX Label for x-axis (Default: the name of the column used for the x-axis): NULL or character
#' @param labelY Label for y-axis (Default: the name of the column used for the y-axis): NULL or character
#' @param legend Legend label that represents the different colour categories (Default: the name of the column used to colour categorise the data): NULL or character
#' @param chromatogramRegion A list that represents the different chromatogram regions (Default: NULL): NULL or list
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
    
    # If colour is NULL
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
    
    # If chromatogram region is NOT NULL
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