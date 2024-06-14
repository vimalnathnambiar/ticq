#' Display Multiple Time Series
#'
#' Display multiple time series data (and perform boundary analysis if defined).
#'
#' Boundary analysis that can be performed:
#' - value: Evaluates data by +/- value from 0
#' - percentage: Evaluates data by +/- value from 100
#' - sd: Evaluates data using 1st and 2nd standard deviations from average mean of the data or reference data
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
    result <- if (!is.null(boundary)) {
      data %>%
        dplyr::select(all_of(commonColumn))
    } else {
      NULL
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
          if (boundary == "value" || boundary == "percentage") {
            # Determine boundaries
            mid <- if (boundary == "value") 0 else 100
            lowerBound <- mid - boundaryValue
            upperBound <- mid + boundaryValue
            
            # Display boundaries
            timeSeries <- timeSeries +
              ggplot2::geom_hline(yintercept = mid, linetype = "dashed", colour = "grey") +
              ggplot2::geom_hline(yintercept = lowerBound, linetype = "dashed", colour = "red") +
              ggplot2::geom_hline(yintercept = upperBound, linetype = "dashed", colour = "red")
            
            # Analysis result
            result <- data %>%
              dplyr::mutate(
                !!y := dplyr::case_when(
                  .data[[y]] < lowerBound ~ "Low",
                  .data[[y]] >= lowerBound & .data[[y]] <= upperBound ~ "Normal",
                  .data[[y]] > upperBound ~ "High"
                )
              ) %>%
              dplyr::select(all_of(commonColumn), !!y) %>%
              dplyr::left_join(result, ., by = commonColumn)
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
              } else {
                mean <- mean(data[[y]])
                sd <- sd(data[[y]])
              }
              
              lowerBound1 <- mean - sd
              upperBound1 <- mean + sd
              lowerBound2 <- mean - (2 * sd)
              upperBound2 <- mean + (2 * sd)
              
              # Display boundaries
              timeSeries <- timeSeries +
                ggplot2::geom_hline(yintercept = mean, linetype = "dashed", colour = "grey") +
                ggplot2::geom_hline(yintercept = lowerBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = upperBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = lowerBound2, linetype = "dashed", colour = "red") +
                ggplot2::geom_hline(yintercept = upperBound2, linetype = "dashed", colour = "red")
              
              # Analysis result
              result <- data %>%
                dplyr::mutate(
                  !!y := dplyr::case_when(
                    .data[[y]] < lowerBound2 ~ "Very Low",
                    .data[[y]] >= lowerBound2 & .data[[y]] < lowerBound1 ~ "Low",
                    .data[[y]] >= lowerBound1 & .data[[y]] <= upperBound1 ~ "Normal",
                    .data[[y]] > upperBound1 & .data[[y]] <= upperBound2 ~ "High",
                    .data[[y]] > upperBound2 ~ "Very High"
                  )
                ) %>%
                dplyr::select(all_of(commonColumn), !!y) %>%
                dplyr::left_join(result, ., by = commonColumn)
            } else {
              # Analysis result
              result <- data %>%
                dplyr::mutate(!!y := "Normal") %>%
                dplyr::select(all_of(commonColumn), !!y) %>%
                dplyr::left_join(result, ., by = commonColumn)
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