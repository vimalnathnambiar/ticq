#' plotMultipleChromatogram
#'
#' Plot multiple chromatogram data in specified number of rows and columns based on the same x-axis but different y-axis.
#'
#' @import ggplot2
#' @import ggpubr
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param startIDX Index of the first data column containing continuous data series for the y-axis: double
#' @param endIDX Index of the last data column data containing continuous data series for the y-axis (Default: NULL): NULL or double
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param caption Plot caption. This will append a caption value to the column name used for the y-axis (Default: Column name used for y): NULL or character
#' @param xLabel x-axis label (Default: Column name used for x): NULL or character
#' @param yLabel y-axis label (Default: NULL): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param nPlotCol Number of plots to be plotted in a single column (Default: 1): double
#' @param nPlotRow Number of plot to be plotted in a single row (Default: 1): double
plotMultipleChromatogram <- function(data,
                                     x,
                                     startIDX,
                                     endIDX = NULL,
                                     colour = NULL,
                                     caption = NULL,
                                     xLabel = x,
                                     yLabel = NULL,
                                     colourLabel = colour,
                                     nPlotCol = 1,
                                     nPlotRow = 1) {
  tryCatch({
    # Defaults
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- nPlotCol * nPlotRow
    
    
      # If index of last data column to plot is NULL
      if (is.null(endIDX)) {
        endIDX <- ncol(data)
      }
    
    # Loop through data columns for plotting y-axis
    for (i in startIDX:endIDX) {
      # y-axis data column name
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
                      x = xLabel,
                      y = yLabel,
                      colour = colourLabel)
      
      # Add plot to plotList
      plotList[[y]] <- chromatogram
      
      # Display plots in specified columns and rows
      if (length(plotList) %% nPlotData == 0 || i == endIDX) {
        plotGrid <- ggpubr::ggarrange(plotlist = plotList,
                                      ncol = nPlotCol, 
                                      nrow = nPlotRow,
                                      common.legend = TRUE,
                                      legend = ifelse(isFirstPlot, "top", "none"))
        print(plotGrid)
        
        # Reset
        plotList <- list()
        isFirstPlot <- FALSE
      }
    }
  },
  warning = function(w) print(paste0("Unable to generate Chromatogram - ", w)), 
  error = function(e) print(paste0("Unable to generate Chromatogram - ", e)))
}