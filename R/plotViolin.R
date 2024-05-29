#' plotViolin
#'
#' Plot violin plots and perform boundary analysis (if specified).
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
#' @param outlierShape Shape used to identify outliers ((Default: NA): NA or double)
#' @param title Plot title (Default: "Time Series"): character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param labelX x-axis label (Default: Value used for x): NULL or character
#' @param labelY y-axis label (Default: Value used for y): NULL or character
#' @param colourLegend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param shapeLegend Legend label for shape grouping (Default: Value used for shape)
#' @param boundary Boundary analysis to perform based on a +/- value from 0 (value) or 100 (percentage), or up to 2 standard deviation (sd) from average mean (Default: NULL, Options: "value", "percentage", "sd"): NULL or character
#' @param boundaryValue Value used for boundary analysis. For "sd" boundary analysis, use list(mean = meanValue, sd = sdValue, sd2 = sd2Value) (Default: 0): double or list
#' @param violinPlotToggle Toggle printing of violin plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param tailTrimToggle Toggle to trim the tails of the violin plot. Applicable only if violinPlotToggle is set to TRUE (Default: FALSE, Options: TRUE or FALSE): boolean
#' @param boxPlotToggle Toggle printing of box plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param boxWidth Width of box plot. Applicable if boxPlotToggle is set to TRUE (Default: 1): double
#' @returns Data frame containing the boundary analysis result (if specified)
plotViolin <- function(data,
                       x,
                       y,
                       colour = NULL,
                       shape = NULL,
                       outlierShape = NA,
                       title = "Violin Plot",
                       subtitle = NULL,
                       caption = NULL,
                       labelX = x,
                       labelY = y,
                       colourLegend = colour,
                       shapeLegend = shape,
                       boundary = NULL,
                       boundaryValue = 0,
                       violinPlotToggle = TRUE,
                       tailTrimToggle = FALSE,
                       boxPlotToggle = TRUE,
                       boxWidth = 1) {
  tryCatch({
    # Plot violin plot
    violinPlot <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]]))
    
      # Display box or violin or both
        # Violin plot
      if (violinPlotToggle) {
        violinPlot <- violinPlot +
          ggplot2::geom_violin(trim = tailTrimToggle)
      }
      
        # Box plot
        if (boxPlotToggle) {
          violinPlot <- violinPlot +
            ggplot2::geom_boxplot(outlier.color = "red",
                                  outlier.shape = outlierShape,
                                  width = boxWidth)
        }
    
      # Add colour and shape grouping, theme and labels
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
                      x = labelX,
                      y = labelY,
                      colour = colourLegend,
                      shape = shapeLegend)
    
    # Add and perform boundary analysis
    result <- NULL
    if (!is.null(boundary)) {
      # Check the type of boundary to draw (value, percentage or sd)
      if (boundary == "value") {
        # Add boundaries
        lowerBound <- 0 - boundaryValue
        upperBound <- 0 + boundaryValue
        
        violinPlot <- violinPlot +
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
        
        result <- rbind(low, normal, high)
      } else if (boundary == "percentage") {
        # Add boundaries
        lowerBound <- 100 - boundaryValue
        upperBound <- 100 + boundaryValue
        
        violinPlot <- violinPlot +
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
        
        result <- rbind(low, normal, high)
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
          
          violinPlot <- violinPlot +
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
    print(paste0("Unable to perform Violin Plot Analysis - ", w))
    return(NULL)
  },
  error = function(e) {
    print(paste0("Unable to perform Violin Plot Analysis - ", e))
    return(NULL)
  })
}