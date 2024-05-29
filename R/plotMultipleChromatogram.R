#' plotMultipleChromatogram
#'
#' Plot multiple chromatogram data in specified number of rows and columns based on the same x-axis but different y-axis.
#'
#' @import ggplot2
#' @import ggpubr
#'
#' @export
#' @param data Data frame that contains plot data: data frame
#' @param x Name of the column that represents the continuous data series for the x-axis: character
#' @param startIDX Index of the first data column that contains continuous data series for the y-axis: double
#' @param endIDX Index of the last data column data that contains continuous data series for the y-axis (Default: NULL): NULL or double
#' @param colour Name of the column used for colour grouping (Default: NULL): NULL or character
#' @param caption Plot caption. This will append a caption value to the column name used for the y-axis (Default: Column name used for y-axis): NULL or character
#' @param labelX x-axis label (Default: the name of the column used for the x-axis): NULL or character
#' @param labelY y-axis label (Default: NULL): NULL or character
#' @param colourLegend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param nPlotCol Number of plots in a single columns (Default: 1): double
#' @param nPlotRow Number of plot in a single row (Default: 1): double
#' @returns Print multiple chromatogram plot
plotMultipleChromatogram <- function(data,
                                     x,
                                     startIDX,
                                     endIDX = NULL,
                                     colour = NULL,
                                     caption = NULL,
                                     labelX = x,
                                     labelY = NULL,
                                     colourLegend = colour,
                                     nPlotCol = 1,
                                     nPlotRow = 1) {
  tryCatch({
    # Defaults
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- nPlotCol * nPlotRow
    
    
      # If index of last column is NULL
      if (is.null(endIDX)) {
        endIDX <- ncol(data)
      }
    
    # Loop through y-axis data columns for plotting
    for (i in startIDX:endIDX) {
      # y-axis column name
      y <- colnames(data)[i]
      
      # Plot chromatogram
      chromatogram <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
        ggplot2::geom_line(aes(colour = if (!is.null(colour)) .data[[colour]] else NULL,
                               group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL),
                           alpha = 0.25) +
        ggplot2::theme(panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.title = element_text(size = 6),
                       legend.text = element_text(size = 6)) + 
        ggplot2::labs(caption = if (is.null(caption)) y else paste(y, caption),
                      x = labelX,
                      y = labelY,
                      colour = colourLegend)
      
      # Add plot to plotList
      plotList[[y]] <- chromatogram
      
      # Display plots
      if (length(plotList) %% nPlotData == 0 || i == endIDX) {
        plotGrid <- ggpubr::ggarrange(plotlist = plotList,
                                      ncol = nPlotCol, nrow = nPlotRow,
                                      common.legend = TRUE,
                                      legend = ifelse(isFirstPlot, "top", "none"))
        print(plotGrid)
        
        # Reset list and change isFirstPlot status
        plotList <- list()
        isFirstPlot <- FALSE
      }
    }
  },
  warning = function(w) print(paste0("Unable to generate Chromatogram - ", w)), 
  error = function(e) print(paste0("Unable to generate Chromatogram - ", e)))
}