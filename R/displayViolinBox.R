#' Display Violin/Box Plot
#'
#' Display data distribution using violin and/or box plot and perform boundary analysis to evaluate data performance.
#' - Value (Evaluates data by +/- value from 0)
#' - Percentage (Evaluates data by +/- value from 100)
#' - Standard deviation, sd (Evaluates data using 1st and 2nd standard deviations from average mean of the data or reference data)
#'
#' Type of boundary analysis:
#' 
#' 1. "value" - Uses a boundary value set +/- from 0 as the boundary ranges for evaluation.
#' 
#' 2. "percentage" - Uses a boundary value set +/- from 100 as the boundary ranges for evaluation.
#' 
#' 3. "sd" - Uses 1st and 2nd standard deviation value from the average mean as the boundary ranges. 
#' To specify an external mean and standard deviation value to be used for analysis, provide reference data (Should have the same data columns as the actual data).
#'
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#' @param data A data frame of the data to be used for plotting.
#' @param x A character string representing the name of the data column to be used for the x-axis.
#' @param y A character string representing the name of the data column to be used for the y-axis.
#' @param colour A character string representing the name of the data column to be used for colour grouping. (Default: `NULL`)
#' @param shape A character string representing the name of the data column to be used for shape grouping. (Default: `NULL`)
#' @param title A character string representing the plot title. (Default: `"Violin/Box Plot"`)
#' @param subtitle A character string representing the plot subtitle. (Default: `NULL`)
#' @param caption A character string representing the plot caption. (Default: `NULL`)
#' @param xLabel A character string representing the x-axis label. (Default: `x`)
#' @param yLabel A character string representing the y-axis label. (Default: `y`)
#' @param colourLabel A character string representing the colour grouping label. (Default: `colour`)
#' @param shapeLabel A character string representing the shape grouping label. (Default: `shape`)
#' @param boundary A character string representing the boundary analysis type to perform. (Default: `NULL`, Options: `"value"`, `"percentage"`, or `"sd"`)
#' @param boundaryValue A numeric value to be used for "value" and "percentage" type boundary analysis. (Default: `0`)
#' @param referenceData A data frame containing reference data to be used for "sd" type boundary analysis. (Default: `NULL`)
#' @param violinPlotToggle A logical value representing the toggle to display a violin plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param tailTrimToggle A logical value representing the toggle to trim the tails of the violin plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param boxPlotToggle A logical value representing the toggle to display a box plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param outlierShape A numeric value representing the shape style to be used to highlight identified outliers. (Default: `NA`)
#' @param boxWidth A numeric value representing the width size to be used for the box plot. (Default: `1`)
#' @param facetWrapBy A character string representing the name of the data column to be used for facet wrapping. (Default: `NULL`)
#' @param facetColumn A numeric value representing the number of columns to be used for facet wrapping. (Default: `NULL`)
#' @param facetRow A numeric value representing the number of rows to be used for face wrapping. (Default: `NULL`)
#' @returns A data frame of the boundary analysis result if performed; otherwise, this function does not return any value.
#' It prints the ggplot object displaying violin/box plot.
displayViolinBox <- function(data,
                             x,
                             y,
                             colour = NULL,
                             shape = NULL,
                             title = "Violin/Box Plot",
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
                             outlierShape = NA,
                             boxWidth = 1,
                             facetWrapBy = NULL,
                             facetColumn = NULL,
                             facetRow = NULL) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    x = x,
    y = y,
    colour = colour,
    shape = shape,
    title = title,
    subtitle = subtitle,
    caption = caption,
    xLabel = xLabel,
    yLabel = yLabel,
    colourLabel = colourLabel,
    shapeLabel = shapeLabel,
    boundary = boundary,
    boundaryValue = boundaryValue,
    violinPlotToggle = violinPlotToggle,
    tailTrimToggle = tailTrimToggle,
    boxPlotToggle = boxPlotToggle,
    outlierShape = outlierShape,
    boxWidth = boxWidth,
    facetWrapBy = facetWrapBy,
    facetColumn = facetColumn,
    facetRow = facetRow
  )
  for (i in names(parameter)) {
    if (i == "x" || i == "y") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "colour" || i == "shape" || i == "facetWrapBy") {
      validateNullableCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "title" || i == "subtitle" || i == "caption" || i == "xLabel" || i == "yLabel" || i == "colourLabel" || i == "shapeLabel") {
      validateNullableCharacterString(name = i, value = parameter[[i]])
    } else if (i == "boundary" && !is.null(parameter[[i]]) &&
               (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || !parameter[[i]] %in% c("value", "percentage", "sd"))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a character string of length 1 ('value', 'percentage', or 'sd'"))
    } else if ((i == "boundaryValue" && boundary %in% c("value", "percentage")) || (i == "boxWidth" && boxPlotToggle)) {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "violinPlotToggle" || (i == "tailTrimToggle" && violinPlotToggle) || i == "boxPlotToggle") {
      validateLogicalValue(name = i, value = parameter[[i]])
    } else if (i == "outlierShape" && boxPlotToggle) {
      validateMissingNumericValue(name = i, value = parameter[[i]])
    } else if (i == "facetColumn" || i == "facetRow") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    }
  }
  
  parameter <- c(x, y, colour, shape, facetWrapBy)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to display ", title, ": Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  validateNumericVectorElement(name = paste0("data[[", y, "]]"), value = data[[y]])
  if (!is.null(referenceData) && boundary == "sd") {
    if (!is.data.frame(referenceData)) {
      stop("Invalid 'referenceData': Must be a data frame")
    } else if (!identical(colnames(referenceData), colnames(data))) {
      stop("Invalid 'referenceData': Data columns must be identical to 'data'")
    }

    if (length(referenceData[[y]]) < 2) {
      message("Invalid referenceData': Insufficient data to generate statistics (Setting default)")
      referenceData <- NULL
    } else {
      validateStatisticalNumericVectorElement(name = paste0("referenceData[[", y, "]]"), value = referenceData[[y]])
    }
  }
  
  # Display violin / box plot
  tryCatch({
    violinBoxPlot <- ggplot2::ggplot(data = data, aes(x = .data[[x]], y = .data[[y]]))
    
    if (violinPlotToggle) {
      violinBoxPlot <- violinBoxPlot +
        ggplot2::geom_violin(trim = tailTrimToggle)
    }
    
    if (boxPlotToggle) {
      violinBoxPlot <- violinBoxPlot +
        ggplot2::geom_boxplot(outlier.color = "red", outlier.shape = outlierShape, width = boxWidth)
    }
    
    violinBoxPlot <- violinBoxPlot +
      ggplot2::geom_jitter(
        aes(
          colour = if (!is.null(colour)) .data[[colour]] else NULL,
          shape = if (!is.null(shape)) .data[[shape]] else NULL,
          group = if (!is.null(colour)) ifelse(.data[[colour]] == "Historical", 1, 2) else NULL
        ),
        alpha = 0.25, position = position_jitter(seed = 1, width = 0.2)
      ) +
      ggplot2::theme(
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
      ) + 
      ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel, colour = colourLabel, shape = shapeLabel)
    
    if (!is.null(facetWrapBy)) {
      violinBoxPlot <- violinBoxPlot +
        ggplot2::facet_wrap( ~ .data[[facetWrapBy]], ncol = facetColumn, nrow = facetRow)
    }
    
    # Perform boundary analysis
    if (!is.null(boundary)) {
      tryCatch({
        if (boundary == "value" || boundary == "percentage") {
          mid <- if (boundary == "value") 0 else 100
          lowerBound <- mid - boundaryValue
          upperBound <- mid + boundaryValue
          
          timeSeries <- timeSeries +
            ggplot2::geom_hline(yintercept = mid, linetype = "dashed", colour = "grey") +
            ggplot2::geom_hline(yintercept = lowerBound, linetype = "dashed", colour = "red") +
            ggplot2::geom_hline(yintercept = upperBound, linetype = "dashed", colour = "red")
          
          result <- data %>%
            dplyr::mutate(
              sampleRange = dplyr::case_when(
                .data[[y]] < lowerBound ~ "Low",
                .data[[y]] >= lowerBound & .data[[y]] <= upperBound ~ "Normal",
                .data[[y]] > upperBound ~ "High",
              )
            )
        } else if (boundary == "sd") {
          if (!is.null(referenceData) || nrow(data) > 1) {
            if (!is.null(referenceData)) {
              stat <- generateStatistic(data = referenceData[[y]])
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
            
            violinBoxPlot <- violinBoxPlot +
              ggplot2::geom_hline(yintercept = mean, linetype = "dashed", colour = "grey") +
              ggplot2::geom_hline(yintercept = lowerBound1, linetype = "dashed", colour = "blue") +
              ggplot2::geom_hline(yintercept = upperBound1, linetype = "dashed", colour = "blue") +
              ggplot2::geom_hline(yintercept = lowerBound2, linetype = "dashed", colour = "red") +
              ggplot2::geom_hline(yintercept = upperBound2, linetype = "dashed", colour = "red")
            
            result <- data %>%
              dplyr::mutate(
                sampleRange = dplyr::case_when(
                  .data[[y]] < lowerBound2 ~ "Very Low",
                  .data[[y]] >= lowerBound2 & .data[[y]] < lowerBound1 ~ "Low",
                  .data[[y]] >= lowerBound1 & .data[[y]] <= upperBound1 ~ "Normal",
                  .data[[y]] > upperBound1 & .data[[y]] <= upperBound2 ~ "High",
                  .data[[y]] > upperBound2 ~ "Very High"
                )
              )
          } else {
            result <- data %>%
              dplyr::mutate(sampleRange = "Normal")
          }
        }
      },
      warning = function(w) {
        message(paste0("Unable to perform '", boundary, "' type boundary analysis: ", w))
        result <- NULL
      },
      error = function(e) {
        message(paste0("Unable to perform '", boundary, "' type boundary analysis: ", e))
        result <- NULL
      })
    }
    
    print(violinBoxPlot)
    if (!is.null(boundary)) {
      return(result)
    }
  }, 
  warning = function(w) {
    message(paste0("Unable to display ", title, ": ", w))
    return(invisible(NULL))
  },
  error = function(e) {
    message(paste0("Unable to display ", title, ": ", e))
    return(invisible(NULL))
  })
}