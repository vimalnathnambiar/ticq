#' Display Multiple Time Series
#'
#' Display multiple time series data (and perform boundary analysis if defined).
#'
#' Boundary analysis that can be performed:
#' - value: Evaluates data by +/- value from 0
#' - percentage: Evaluates data +/- value from 100
#' - sd: Evaluates data using 2 standard deviations from average mean of the data or using defined mean and standard deviation values
#'
#' To define mean and standard deviation values to be used for "sd" boundary analysis, pass reference data that mirrors the same columns as data.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param firstColumnIndex Index of the first data column containing continuous data series for the y-axis: double
#' @param lastColumnIndex Index of the last data column data containing continuous data series for the y-axis (Default: NULL): NULL or double
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param shape Column name representing data to be used for shape grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Chromatogram"): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param xLabel x-axis label (Default: Value used for x): NULL or character
#' @param yLabel y-axis label (Default: NULL): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param shapeLabel Shape grouping label (Default: Value used for shape)
#' @param boundary Boundary analysis to perform (Default: NULL, Options: "value", "percentage", "sd"): NULL or character.
#' @param boundaryValue Value used for "value" and "percentage" boundary analysis (Default: 0): double
#' @param referenceData A data frame with the same column names and data to calculate "sd" boundaries (Default: NULL): NULL or data frame
#' @param trendlineToggle Toggle to display trendline in the data (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param xTickToggle Toggle to display ticks on x-axis (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param facetWrapBy Column name representing data to be used for facet wrapping (Default: NULL): NULL or character
#' @param facetColumn Number of columns to use for facet wrapping (Default: NULL): NULL or double
#' @param facetRow Number of rows to use for facet wrapping (Default: NULL): NULL or double
#' @param plotColumn Number of columns to use for plotting (Default: 1): double
#' @param plotRow Number of rows to use for plotting (Default: 1): double
#' @returns A data frame containing the result of the boundary analysis performed
displayMultipleTimeSeries <- function(data,
                                      commonColumn, 
                                      x,
                                      firstColumnIndex,
                                      lastColumnIndex = NULL,
                                      colour = NULL,
                                      shape = NULL,
                                      title = "Time Series",
                                      subtitle = NULL,
                                      caption = NULL,
                                      xLabel = x,
                                      yLabel = NULL,
                                      colourLabel = colour,
                                      shapeLabel = shape,
                                      boundary = NULL,
                                      boundaryValue = 0,
                                      referenceData = NULL,
                                      trendlineToggle = TRUE,
                                      xTickToggle = TRUE,
                                      facetWrapBy = NULL,
                                      facetColumn = NULL,
                                      facetRow = NULL,
                                      plotColumn = 1,
                                      plotRow = 1) {
  tryCatch({
    # Defaults
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- plotColumn * plotRow
    
    # Check last data column index
    if (is.null(lastColumnIndex)) {
      lastColumnIndex <- ncol(data)
    }
    
    # Data frame to store analysis result
    if (!is.null(boundary)) {
      result <- data %>%
        dplyr::select(all_of(commonColumn))
    } else {
      result <- NULL
    }
      
    # Loop through data columns
    for (i in firstColumnIndex:lastColumnIndex) {
      # Data column name
      y <- colnames(data)[i]
      
      # Plot time series
      timeSeries <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
        ggplot2::geom_point(
          aes(
            colour = if (!is.null(colour)) .data[[colour]] else NULL,
            shape = if (!is.null(shape)) .data[[shape]] else NULL,
            group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL
          ),
          alpha = 0.25
        ) +
        ggplot2::theme(
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        ) + 
        ggplot2::labs(caption = if (is.null(caption)) y else paste(y, caption), x = xLabel, y = yLabel, colour = colourLabel, shape = shapeLabel)
      
        # Trendline
        if (trendlineToggle) {
          timeSeries <- timeSeries +
            ggplot2::geom_smooth(colour = "red", fill = "grey", alpha = 0.25)
        }
      
        # x-axis ticks
        if (!xTickToggle) {
          timeSeries <- timeSeries +
            ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        }
      
        # Facet wrap
        if (!is.null(facetWrapBy)) {
          timeSeries <- timeSeries +
            ggplot2::facet_wrap( ~ .data[[facetWrapBy]], ncol = facetColumn, nrow = facetRow)
        }
      
      # Boundary analysis
      if (!is.null(boundary)) {
        tryCatch({
          # Check boundary type
          if (boundary == "value") {
            # Determine boundaries
            lowerBound <- 0 - boundaryValue
            upperBound <- 0 + boundaryValue
            
            # Display boundaries
            timeSeries <- timeSeries +
              ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
              ggplot2::geom_hline(yintercept = lowerBound, linetype = "dashed", colour = "red") +
              ggplot2::geom_hline(yintercept = upperBound, linetype = "dashed", colour = "red")
            
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
            # Determine boundaries
            lowerBound <- 100 - boundaryValue
            upperBound <- 100 + boundaryValue
            
            # Display boundaries
            timeSeries <- timeSeries +
              ggplot2::geom_hline(yintercept = 100, linetype = "dashed", colour = "grey") +
              ggplot2::geom_hline(yintercept = lowerBound, linetype = "dashed", colour = "red") +
              ggplot2::geom_hline(yintercept = upperBound, linetype = "dashed", colour = "red")
            
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
            # Check reference data
            if (!is.null(referenceData)) {
              if (!is.data.frame(referenceData) ||
                  ncol(referenceData) != ncol(data) ||
                  all(colnames(referenceData) != colnames(data)) ||
                  nrow(referenceData) < 2 ||
                  any(is.na(referenceData[[y]]))) {
                referenceData <- NULL
              }
            }
            
            # Check sample data and reference data
            if (nrow(data) > 1 || !is.null(referenceData)) {
              # Determine boundaries
              if (!is.null(referenceData)) {
                # Generate statistics
                stat <- ticq::generateStat(data = referenceData[[y]])
                
                mean <- stat$mean
                sd <- stat$sd
                sd2 <- stat$sd2
              } else {
                mean <- mean(data[[y]])
                sd <- sd(data[[y]])
                sd2 <- sd(2 * data[[y]])
              }
              
              lowerBound1 <- mean - sd
              upperBound1 <- mean + sd
              lowerBound2 <- mean - sd2
              upperBound2 <- mean + sd2
              
              # Display boundaries
              timeSeries <- timeSeries +
                ggplot2::geom_hline(yintercept = mean, linetype = "dashed", colour = "grey") +
                ggplot2::geom_hline(yintercept = lowerBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = upperBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = lowerBound2, linetype = "dashed", colour = "red") +
                ggplot2::geom_hline(yintercept = upperBound2, linetype = "dashed", colour = "red")
              
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
              
              result <- dplyr::left_join(result, normal, by = commonColumn) %>%
                dplyr::rename(!!y := sampleRange)
            }
          } else {
            result <- NULL
          }
        },
        warning = function(w) {
          print(paste("Unable to perform boundary analysis for", y, "-", w))
          result <- result %>%
            dplyr::mutate(!!y := NA_character_)
        },
        error = function(e) {
          print(paste("Unable to perform boundary analysis for", y, "-", e))
          result <- result %>%
            dplyr::mutate(!!y := NA_character_)
        })
      }
      
      # Append plot to plot list
      plotList[[y]] <- timeSeries
      
      # Display time series
      suppressMessages(
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
      )
    }
    return(result)
  }, 
  warning = function(w) {
    print(paste("Unable to display", title, "-", w))
    return(NULL)
  },
  error = function(e) {
    print(paste("Unable to display", title, "-", e))
    return(NULL)
  })
}