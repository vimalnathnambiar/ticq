#' plotTimeSeries
#' 
#' Plot time series data and perform boundary analysis (if specified).
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @export
#' @param data Data frame that contains plot data: data frame
#' @param x Name of the column that represents the continuous data series for x-axis: character
#' @param y Name of the column that represents the continuous data series for y-axis: character
#' @param colour Name of the column used for colour grouping (Default: NULL): NULL or character
#' @param shape Name of the column used for shape grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Time Series"): character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param labelX x-axis label (Default: Value used for x): NULL or character
#' @param labelY y-axis label (Default: Value used for y): NULL or character
#' @param colourLegend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param shapeLegend Legend label for shape grouping (Default: Value used for shape)
#' @param boundary Boundary analysis to perform based on a +/- value from 0 (value) or 100 (percentage), or up to 2 standard deviation (sd) from average mean (Default: NULL, Options: "value", "percentage", "sd"): NULL or character
#' @param boundaryValue Value used for boundary analysis. Not applicable for "sd" boundary analysis (Default: 0): double
#' @param xTick Toggle ticks on x-axis (Default: TRUE, Options: TRUE or FALSE): boolean
#' @returns Data frame containing the boundary analysis result (if specified)
plotTimeSeries <- function(data, 
                           x, 
                           y, 
                           colour = NULL, 
                           shape = NULL,
                           title = "Time Series", 
                           subtitle = NULL, 
                           caption = NULL, 
                           labelX = x, 
                           labelY = y, 
                           colourLegend = colour, 
                           shapeLegend = shape,
                           boundary = NULL, 
                           boundaryValue = 0, 
                           xTick = TRUE) {
  tryCatch({    
    # Plot time series
    timeSeries <- ggplot2::ggplot(data = data,
                                  aes(x = .data[[x]], y = .data[[y]]))
    
    # Add colour and shape grouping
    if (is.null(colour)) {
      if (is.null(shape)) {
        timeSeries <- timeSeries +
          ggplot2::geom_point(alpha = 0.25)
      } else {
        timeSeries <- timeSeries +
          ggplot2::geom_point(aes(shape = data[[shape]]),
                              alpha = 0.25)
      }
    } else {
      if (is.null(shape)) {
        timeSeries <- timeSeries +
          ggplot2::geom_point(aes(colour = data[[colour]],
                                  group = ifelse(data[[colour]] == "Historical", 1, 2)),
                              alpha = 0.25)
      } else {
        timeSeries <- timeSeries +
          ggplot2::geom_point(aes(colour = data[[colour]],
                                  shape = data[[shape]],
                                  group = ifelse(data[[colour]] == "Historical", 1, 2)),
                              alpha = 0.25)
      }
    }
    
    # Add theme and labels
    timeSeries <- timeSeries +
      ggplot2::theme(panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_text(size = 8),
                     legend.text = element_text(size = 8)) + 
      ggplot2::labs(title = title,
                    subtitle = subtitle,
                    caption = caption,
                    x = labelX,
                    y = labelY,
                    colour = colourLegend,
                    shape = shapeLegend)
    
    # Remove xTick
    if (!xTick) {
      timeSeries <- timeSeries +
        ggplot2::theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank())
    }
    
    # Add and perform boundary analysis
    if (!is.null(boundary)) {
      # Data frame to store analysis result
      result <- data.frame()
      
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
          dplyr::mutate(sampleRange = "Low")
        
        normal <- data %>%
          dplyr::filter(.data[[y]] >= lowerBound & .data[[y]] <= upperBound) %>%
          dplyr::mutate(sampleRange = "Normal")
        
        high <- data %>%
          dplyr::filter(.data[[y]] > upperBound) %>%
          dplyr::mutate(sampleRange = "High")
        
        result <- rbind(result, low, normal, high)
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
          dplyr::mutate(sampleRange = "Low")
        
        normal <- data %>%
          dplyr::filter(.data[[y]] >= lowerBound & .data[[y]] <= upperBound) %>%
          dplyr::mutate(sampleRange = "Normal")
        
        high <- data %>%
          dplyr::filter(.data[[y]] > upperBound) %>%
          dplyr::mutate(sampleRange = "High")
        
        result <- rbind(result, low, normal, high)
      } else if (boundary == "sd") {
        # Check if there are more than 1 data point to generate stats on
        if (nrow(data) > 1) {
          # Generate stats
          mean <- mean(data[[y]])
          sd <- sd(data[[y]])
          sd2 <- sd(2 * data[[y]])
          
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
            dplyr::mutate(sampleRange = "Very Low")
          
          low <- data %>%
            dplyr::filter(.data[[y]] >= lowerBound2 & .data[[y]] < lowerBound1) %>%
            dplyr::mutate(sampleRange = "Low")
          
          normal <- data %>%
            dplyr::filter(.data[[y]] >= lowerBound1 & .data[[y]] <= upperBound1) %>%
            dplyr::mutate(sampleRange = "Normal")
          
          high <- data %>%
            dplyr::filter(.data[[y]] > upperBound1 & .data[[y]] <= upperBound2) %>%
            dplyr::mutate(sampleRange = "High")
          
          veryHigh <- data %>%
            dplyr::filter(.data[[y]] > upperBound2) %>%
            dplyr::mutate(sampleRange = "Very High")
          
          result <- rbind(result, veryLow, low, normal, high, veryHigh)
        } else {
          result <- data %>%
            dplyr::mutate(sampleRange = "Normal")
        }
      } else {
        result <- NULL
      }
    }
    
    print(timeSeries)
    return(result)
  }, 
  warning = function(w) {
    print(paste0("Unable to perform Time Series Analysis - ", w))
    result <- NULL
    return(result)
  },
  error = function(e) {
    print(paste0("Unable to perform Time Series Analysis - ", e))
    result <- NULL
    return(result)
  })
}