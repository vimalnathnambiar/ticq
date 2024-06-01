#' Plot Chromatogram
#'
#' Plot chromatogram data.
#'
#' @import ggplot2
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param y Column name representing the continuous data series to be used for y-axis: character
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Chromatogram"): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param xLabel x-axis label (Default: Column name used for x): NULL or character
#' @param yLabel y-axis label (Default:Column name used for y): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param chromatogramRegion A list that represents the different chromatogram regions of interest to be plotted (Default: NULL): NULL or list
plotChromatogram <- function(data,
                             x,
                             y,
                             colour = NULL,
                             title = "Chromatogram",
                             subtitle = NULL,
                             caption = NULL,
                             xLabel = x,
                             yLabel = y,
                             colourLabel = colour,
                             chromatogramRegion = NULL) {
  tryCatch({
    # Plot chromatogram
    chromatogram <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_line(aes(colour = if (!is.null(colour)) .data[[colour]] else NULL,
                             group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL),
                         alpha = 0.25) +
      ggplot2::theme(panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_text(size = 8),
                     legend.text = element_text(size = 8)) + 
      ggplot2::labs(title = title,
                    subtitle = subtitle,
                    caption = caption,
                    x = xLabel,
                    y = yLabel,
                    colour = colourLabel)
    
    # Display chromatogram regions of interest
    if (!is.null(chromatogramRegion)) {
      # Mass calibration region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$massCal,
                                                label = "Mass Calibration Region")
      
      # Analyte region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$analyte,
                                                label = "Analyte Region")
      
      # Wash region
      chromatogram <- displayChromatogramRegion(plot = chromatogram,
                                                maxX = max(data[[x]]),
                                                maxY = max(data[[y]]),
                                                chromatogramRegion = chromatogramRegion$wash,
                                                label = "Wash Region")
    }
    
    print(chromatogram)
  },
  warning = function(w) print(paste0("Unable to display Chromatogram - ", w)),
  error = function(e) print(paste0("Unable to display Chromatogram - ", e)))
}