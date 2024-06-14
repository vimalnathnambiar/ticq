#' Display Multiple Chromatogram
#'
#' Display multiple chromatogram data.
#'
#' @import ggplot2
#' @import ggpubr
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param firstColumnIndex Index of the first data column containing continuous data series for the y-axis: double
#' @param lastColumnIndex Index of the last data column data containing continuous data series for the y-axis (Default: NULL): NULL or double
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Chromatogram"): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption. This will append a caption value to the column name used for the y-axis (Default: Column name used for y): NULL or character
#' @param xLabel x-axis label (Default: Column name used for x): NULL or character
#' @param yLabel y-axis label (Default: NULL): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param xTickToggle Toggle to display ticks on x-axis (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param facetWrapBy Column name representing data to be used for facet wrapping (Default: NULL): NULL or character
#' @param facetColumn Number of columns to use for facet wrapping (Default: NULL): NULL or double
#' @param facetRow Number of rows to use for facet wrapping (Default: NULL): NULL or double
#' @param plotColumn Number of columns to use for plotting (Default: 1): double
#' @param plotRow Number of rows to use for plotting (Default: 1): double
#' @param chromatogramRegion A list representing the different chromatogram regions of interest to be plotted (Default: NULL): NULL or list
displayMultipleChromatogram <- function(data,
                                        x,
                                        firstColumnIndex,
                                        lastColumnIndex = NULL,
                                        colour = NULL,
                                        title = "Chromatogram",
                                        subtitle = NULL,
                                        caption = NULL,
                                        xLabel = x,
                                        yLabel = NULL,
                                        colourLabel = colour,
                                        xTickToggle = TRUE,
                                        facetWrapBy = NULL,
                                        facetColumn = NULL,
                                        facetRow = NULL,
                                        plotColumn = 1,
                                        plotRow = 1,
                                        chromatogramRegion = NULL) {
  tryCatch({
    # Defaults
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- plotColumn * plotRow
    
    
    # Check last data column index
    if (is.null(lastColumnIndex)) {
      lastColumnIndex <- ncol(data)
    }
    
    # Loop through data columns
    for (i in firstColumnIndex:lastColumnIndex) {
      # Data column name
      y <- colnames(data)[i]
      
      # Plot chromatogram
      chromatogram <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
        ggplot2::geom_line(
          aes(
            colour = if (!is.null(colour)) .data[[colour]] else NULL,
            group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL
          ),
          alpha = 0.25
        ) +
        ggplot2::theme(
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 6)
        ) + 
        ggplot2::labs(caption = if (is.null(caption)) y else paste(y, caption), x = xLabel, y = yLabel, colour = colourLabel)
      
        # x-axis ticks
        if (!xTickToggle) {
          chromatogram <- chromatogram +
            ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        }
        
        # Facet wrap
        if (!is.null(facetWrapBy)) {
          chromatogram <- chromatogram +
            ggplot2::facet_wrap( ~ .data[[facetWrapBy]], ncol = facetColumn, nrow = facetRow)
        }
      
      # Chromatogram regions of interest
      if (!is.null(chromatogramRegion)) {
        # Region label
        regionLabel <- list(massCalibration = "Mass Calibration Region", analyte = "Analyte Region", wash = "Wash Region")
        
        # Maximum value of x- and y-axis
        maxX <- max(data[[x]])
        maxY <- max(data[[y]])
        
        # Display region
        for (i in names(regionLabel)) {
          chromatogram <- displayChromatogramRegion(
            plot = chromatogram,
            maxX = maxX,
            maxY = maxY,
            chromatogramRegion = chromatogramRegion[[i]],
            label = regionLabel[[i]]
          )
        }
      }
      
      # Append plot to plot list
      plotList[[y]] <- chromatogram
      
      # Display chromatogram
      if (length(plotList) %% nPlotData == 0 || i == lastColumnIndex) {
        # Arrange plots into grid
        plotGrid <- ggpubr::ggarrange(
          plotlist = plotList,
          ncol = plotColumn,
          nrow = plotRow,
          common.legend = TRUE,
          legend = ifelse(isFirstPlot, "top", "none")
        )
        
        # Plot grid title and subtitle
        if (isFirstPlot) {
          plotGrid <- ggpubr::annotate_figure(plotGrid, top = ggpubr::text_grob(paste0(title, ": ", subtitle)))
        }
        
        # Display grid
        print(plotGrid)
        
        # Reset
        plotList <- list()
        isFirstPlot <- FALSE
      }
    }
  },
  warning = function(w) print(paste("Unable to display", title, "-", w)),
  error = function(e) print(paste("Unable to display", title, "-", e)))
}