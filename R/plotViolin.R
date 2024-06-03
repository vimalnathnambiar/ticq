#' Plot Violin
#'
#' Plot violin plot (and perform boundary analysis if defined).
#'
#' Boundary analysis that can be performed:
#' - value: Evaluates data by +/- value from 0
#' - percentage: Evaluates data +/- value from 100
#' - sd: Evaluates data using 2 standard deviations from average mean of the data or using defined mean and standard deviation values
#'
#' To define mean and standard deviation values to be used for "sd" boundary analysis, use list(mean = meanValue, sd = firstValueSD, sd2 = secondValueSD)
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param y Column name representing the continuous data series to be used for y-axis: character
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param shape Column name representing data to be used for shape grouping (Default: NULL): NULL or character
#' @param outlierShape Shape style to be used to identify outliers (Default: NA): NA or double
#' @param title Plot title (Default: "Violin Plot"): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param xLabel x-axis label (Default: Value used for x): NULL or character
#' @param yLabel y-axis label (Default: Value used for y): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param shapeLabel Shape grouping label (Default: Value used for shape)
#' @param boundary Boundary analysis to perform (Default: NULL, Options: "value", "percentage", "sd"): NULL or character.
#' @param boundaryValue Value used for "value" and "percentage" boundary analysis (Default: 0): double
#' @param referenceData A data frame with the same column names and data to calculate "sd" boundaries (Default: NULL): NULL or data frame
#' @param violinPlotToggle Toggle to display violin plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param tailTrimToggle Toggle to trim the tails of the violin plot. Applicable only if violinPlotToggle is set to TRUE (Default: FALSE, Options: TRUE or FALSE): boolean
#' @param boxPlotToggle Toggle to display box plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param boxWidth Box plot width. Applicable only if boxPlotToggle is set to TRUE (Default: 1): double
#' @returns A data frame containing the result of the boundary analysis performed
plotViolin <- function(data, 
                       x,
                       y,
                       colour = NULL,
                       shape = NULL,
                       outlierShape = NA,
                       title = "Violin Plot",
                       subtitle = NULL,
                       caption = NULL,
                       xLabel = x,
                       yLabel = y,
                       colourLabel = colour,
                       shapeLabel = shape,
                       boundary = NULL,
                       boundaryValue = 0,
                       referenceData = NULL,
                       violinPlotToggle = TRUE,
                       tailTrimToggle = FALSE,
                       boxPlotToggle = TRUE,
                       boxWidth = 1) {
  tryCatch({
    # Plot violin plot
    violinPlot <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]]))
    
      # Display violin plot
      if (violinPlotToggle) {
        violinPlot <- violinPlot +
          ggplot2::geom_violin(trim = tailTrimToggle)
      }
    
      # Display box plot
      if (boxPlotToggle) {
        violinPlot <- violinPlot +
          ggplot2::geom_boxplot(outlier.color = "red",
                                outlier.shape = outlierShape,
                                width = boxWidth)
      }
    
      # Colour and shape grouping, theme and labels
      violinPlot <- violinPlot +
        ggplot2::geom_jitter(aes(colour = if (!is.null(colour)) .data[[colour]] else NULL,
                                 shape = if (!is.null(shape)) .data[[shape]] else NULL,
                                 group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL),
                             alpha = 0.25,
                             position = position_jitter(seed = 1, width = 0.2)) +
        ggplot2::theme(panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.title = element_text(size = 8),
                       legend.text = element_text(size = 8)) + 
        ggplot2::labs(title = title,
                      subtitle = subtitle,
                      caption = caption,
                      x = xLabel,
                      y = yLabel,
                      colour = colourLabel,
                      shape = shapeLabel)
    
    # Perform boundary analysis if defined
    result <- NULL
    if (!is.null(boundary)) {
      # Check boundary type
      if (boundary == "value") {
        # Determine boundaries
        lowerBound <- 0 - boundaryValue
        upperBound <- 0 + boundaryValue
        
        # Display boundaries
        violinPlot <- violinPlot +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "dashed",
                              colour = "grey") +
          ggplot2::geom_hline(yintercept = lowerBound,
                              linetype = "dashed",
                              colour = "red") +
          ggplot2::geom_hline(yintercept = upperBound,
                              linetype = "dashed",
                              colour = "red")
        
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
        
        result <- rbind(low, normal, high)
      } else if (boundary == "percentage") {
        # Determine boundaries
        lowerBound <- 100 - boundaryValue
        upperBound <- 100 + boundaryValue
        
        # Display boundaries
        violinPlot <- violinPlot +
          ggplot2::geom_hline(yintercept = 100,
                              linetype = "dashed",
                              colour = "grey") +
          ggplot2::geom_hline(yintercept = lowerBound,
                              linetype = "dashed",
                              colour = "red") +
          ggplot2::geom_hline(yintercept = upperBound,
                              linetype = "dashed",
                              colour = "red")
        
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
        
        result <- rbind(low, normal, high)
      } else if (boundary == "sd") {
        # Check status of reference data
        if (!is.null(referenceData)) {
          if (!is.data.frame(referenceData) ||
              ncol(referenceData) != ncol(data) ||
              all(colnames(referenceData) != colnames(data)) ||
              nrow(referenceData) < 2 ||
              any(is.na(referenceData[[y]]))) {
            referenceData <- NULL
          }
        }
        
        # Check if there are more than 1 data point or if reference data is available
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
          violinPlot <- violinPlot +
            ggplot2::geom_hline(yintercept = mean,
                                linetype = "dashed",
                                colour = "grey") +
            ggplot2::geom_hline(yintercept = lowerBound1,
                                linetype = "dashed",
                                colour = "blue") +
            ggplot2::geom_hline(yintercept = upperBound1,
                                linetype = "dashed",
                                colour = "blue") +
            ggplot2::geom_hline(yintercept = lowerBound2,
                                linetype = "dashed",
                                colour = "red") +
            ggplot2::geom_hline(yintercept = upperBound2,
                                linetype = "dashed",
                                colour = "red")
          
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
          
          result <- rbind(veryLow, low, normal, high, veryHigh)
        } else {
          result <- data %>%
            dplyr::mutate(sampleRange = "Normal")
        }
      }
    }
    
    print(violinPlot)
    return(result)
  }, 
  warning = function(w) {
    print(paste0("Unable to display Violin Plot Analysis - ", w))
    return(NULL)
  },
  error = function(e) {
    print(paste0("Unable to display Violin Plot Analysis - ", e))
    return(NULL)
  })
}