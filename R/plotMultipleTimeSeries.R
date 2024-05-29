#' plotMultipleTimeSeries
#'
#' Plot multiple time series data and perform boundary analysis (if specified).
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#' @param data Data frame that contains plot data: data frame
#' @param commonColumn Column names that are common across all samples: character vector
#' @param x Name of the column that represents the continuous data series for x-axis: character
#' @param startIDX Index of the first data column that contains continuous data series for the y-axis: double
#' @param endIDX Index of the last data column data that contains continuous data series for the y-axis (Default: NULL): NULL or double
#' @param colour Name of the column used for colour grouping (Default: NULL): NULL or character
#' @param shape Name of the column used for shape grouping (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param labelX x-axis label (Default: Value used for x): NULL or character
#' @param labelY y-axis label (Default: NULL): NULL or character
#' @param colourLegend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param shapeLegend Legend label for shape grouping (Default: Value used for shape)
#' @param boundary Boundary analysis to perform based on a +/- value from 0 (value) or 100 (percentage), or up to 2 standard deviation (sd) from average mean (Default: NULL, Options: "value", "percentage", "sd"): NULL or character
#' @param boundaryValue Value used for boundary analysis. To specify values for "sd" boundary analysis, use list(mean = meanValue, sd = sdValue, sd2 = sd2Value) (Default: 0): double or list
#' @param xTickToggle Toggle ticks on x-axis (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param nPlotCol Number of plots in a single columns (Default: 1): double
#' @param nPlotRow Number of plot in a single row (Default: 1): double
#' @returns Data frame containing the boundary analysis result (if specified)
plotMultipleTimeSeries <- function(data,
                                   commonColumn,
                                   x,
                                   startIDX,
                                   endIDX = NULL,
                                   colour = NULL,
                                   shape = NULL,
                                   caption = NULL,
                                   labelX = x,
                                   labelY = NULL,
                                   colourLegend = colour,
                                   shapeLegend = shape,
                                   boundary = NULL,
                                   boundaryValue = 0,
                                   xTickToggle = TRUE,
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
    
      # Base data frame
      if (!is.null(boundary)) {
        result <- data %>%
          dplyr::select(all_of(commonColumn))
      } else {
        result <- NULL
      }
      
    # Loop through y-axis data columns for plotting
    for (i in startIDX:endIDX) {
      # y-axis column name
      y <- colnames(data)[i]
      
      # Plot time series
      timeSeries <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
        ggplot2::geom_point(aes(colour = if (!is.null(colour)) .data[[colour]] else NULL,
                                shape = if (!is.null(shape)) .data[[shape]] else NULL,
                                group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL),
                            alpha = 0.25) +
        ggplot2::theme(panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.title = element_text(size = 8),
                       legend.text = element_text(size = 8)) + 
        ggplot2::labs(caption = if (is.null(caption)) y else paste(y, caption),
                      x = labelX,
                      y = labelY,
                      colour = colourLegend,
                      shape = shapeLegend)
      
        # Remove xTick
        if (!xTickToggle) {
          timeSeries <- timeSeries +
            ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        }
      
      # Add and perform boundary analysis
      if (!is.null(boundary)) {
        # Check the type of boundary to draw (value, percentage or sd)
        if (boundary == "value") {
          # Add boundaries
          lowerBound <- 0 - boundaryValue
          upperBound <- 0 + boundaryValue
          
          timeSeries <- timeSeries +
            ggplot2::geom_hline(yintercept = 0,
                                linetype = "dashed",
                                colour = "grey") +
            ggplot2::geom_hline(yintercept = lowerBound,
                                linetype = "dashed",
                                colour = "darkred") +
            ggplot2::geom_hline(yintercept = upperBound,
                                linetype = "dashed",
                                colour = "darkred")
          
          # Analysis result
          low <- data %>%
            dplyr::filter(.data[[y]] < lowerBound) %>%
            dplyr::mutate(sampleRange = "Low") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          normal <- data %>%
            dplyr::filter(.data[[y]] >= lowerBound & .data[[y]] <= upperBound) %>%
            dplyr::mutate(sampleRange = "Normal") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          high <- data %>%
            dplyr::filter(.data[[y]] > upperBound) %>%
            dplyr::mutate(sampleRange = "High") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          result <- dplyr::left_join(result, rbind(low, normal, high), by = commonColumn) %>%
            dplyr::rename(!!y := sampleRange)
        } else if (boundary == "percentage") {
          # Add boundaries
          lowerBound <- 100 - boundaryValue
          upperBound <- 100 + boundaryValue
          
          timeSeries <- timeSeries +
            ggplot2::geom_hline(yintercept = 100,
                                linetype = "dashed",
                                colour = "grey") +
            ggplot2::geom_hline(yintercept = lowerBound,
                                linetype = "dashed",
                                colour = "darkred") +
            ggplot2::geom_hline(yintercept = upperBound,
                                linetype = "dashed",
                                colour = "darkred")
          
          # Analysis result
          low <- data %>%
            dplyr::filter(.data[[y]] < lowerBound) %>%
            dplyr::mutate(sampleRange = "Low") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          normal <- data %>%
            dplyr::filter(.data[[y]] >= lowerBound & .data[[y]] <= upperBound) %>%
            dplyr::mutate(sampleRange = "Normal") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          high <- data %>%
            dplyr::filter(.data[[y]] > upperBound) %>%
            dplyr::mutate(sampleRange = "High") %>%
            dplyr::select(all_of(commonColumn), sampleRange)
          
          result <- dplyr::left_join(result, rbind(low, normal, high), by = commonColumn) %>%
            dplyr::rename(!!y := sampleRange)
        } else if (boundary == "sd") {
          # Check if there are more than 1 data point or if boundaryValue (list containing mean and necessary standard deviation values) is provided
          if (nrow(data) > 1 || is.list(boundaryValue)) {
            # Get mean and standard deviation values
            if (is.list(boundaryValue)) {
              mean <- boundaryValue$mean
              sd <- boundaryValue$sd
              sd2 <- boundaryValue$sd2
            } else {
              mean <- mean(data[[y]])
              sd <- sd(data[[y]])
              sd2 <- sd(2 * data[[y]])
            }
            
            # Add boundaries
            lowerBound1 <- mean - sd
            upperBound1 <- mean + sd
            lowerBound2 <- mean - sd2
            upperBound2 <- mean + sd2
            
            timeSeries <- timeSeries +
              ggplot2::geom_hline(yintercept = mean,
                                  linetype = "dashed",
                                  colour = "grey") +
              ggplot2::geom_hline(yintercept = lowerBound1,
                                  linetype = "dashed",
                                  colour = "darkblue") +
              ggplot2::geom_hline(yintercept = upperBound1,
                                  linetype = "dashed",
                                  colour = "darkblue") +
              ggplot2::geom_hline(yintercept = lowerBound2,
                                  linetype = "dashed",
                                  colour = "darkred") +
              ggplot2::geom_hline(yintercept = upperBound2,
                                  linetype = "dashed",
                                  colour = "darkred")
            
            # Analysis result
            veryLow <- data %>%
              dplyr::filter(.data[[y]] < lowerBound2) %>%
              dplyr::mutate(sampleRange = "Very Low") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            low <- data %>%
              dplyr::filter(.data[[y]] >= lowerBound2 & .data[[y]] < lowerBound1) %>%
              dplyr::mutate(sampleRange = "Low") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            normal <- data %>%
              dplyr::filter(.data[[y]] >= lowerBound1 & .data[[y]] <= upperBound1) %>%
              dplyr::mutate(sampleRange = "Normal") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            high <- data %>%
              dplyr::filter(.data[[y]] > upperBound1 & .data[[y]] <= upperBound2) %>%
              dplyr::mutate(sampleRange = "High") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            veryHigh <- data %>%
              dplyr::filter(.data[[y]] > upperBound2) %>%
              dplyr::mutate(sampleRange = "Very High") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            result <- dplyr::left_join(result, rbind(veryLow, low, normal, high, veryHigh), by = commonColumn) %>%
              dplyr::rename(!!y := sampleRange)
          } else {
            normal <- data %>%
              dplyr::mutate(sampleRange = "Normal") %>%
              dplyr::select(all_of(commonColumn), sampleRange)
            
            result <- dplyr::left_join(result,
                                       normal,
                                       by = commonColumn) %>%
              dplyr::rename(!!y := sampleRange)
          }
        } else {
          result <- NULL
        }
      }
      
      # Add plot to plotList
      plotList[[y]] <- timeSeries
      
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
    
    return(result)
  }, 
  warning = function(w) {
    print(paste0("Unable to perform Time Series Analysis - ", w))
    return(NULL)
  },
  error = function(e) {
    print(paste0("Unable to perform Time Series Analysis - ", e))
    return(NULL)
  })
}