#' Display Multiple Time Series
#'
#' Display multiple time series data and perform boundary analysis to evaluate data performance over time.
#' - Value (Evaluates data by +/- value from 0)
#' - Percentage (Evaluates data by +/- value from 100)
#' - Standard deviation, sd (Evaluates data using 1st and 2nd standard deviations from average mean of the data or reference data).
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
#' @import ggplot2
#' @import dplyr
#'
#' @export
#' @param data A data frame of the time series data to be used for plotting.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param x A character string representing the name of the data column to be used for the x-axis.
#' @param firstColumnIndex A numeric value representing the index of the first data column to be used for y-axis.
#' @param lastColumnIndex A numeric value representing the index of the last data column to be used for y-axis. (Default: `NULL`)
#' @param colour A character string representing the name of the data column to be used for colour grouping. (Default: `NULL`)
#' @param shape A character string representing the name of the data column to be used for shape grouping. (Default: `NULL`)
#' @param title A character string representing the plot title. (Default: `"Time Series"`)
#' @param subtitle A character string representing the plot subtitle. (Default: `NULL`)
#' @param caption A character string representing the plot caption. (Default: `NULL`)
#' @param xLabel  character string representing the x-axis label. (Default: `x`)
#' @param yLabel A character string representing the y-axis label. (Default: `y`)
#' @param colourLabel A character string representing the colour grouping label. (Default: `colour`)
#' @param shapeLabel A character string representing the shape grouping label. (Default: `shape`)
#' @param boundary A character string representing the boundary analysis type to perform. (Default: `NULL`, Options: `"value"`, `"percentage"`, or `"sd"`)
#' @param boundaryValue A numeric value to be used for "value" and "percentage" type boundary analysis. (Default: `0`)
#' @param referenceData A data frame containing reference data to be used for "sd" type boundary analysis. (Default: `NULL`)
#' @param trendlineToggle A logical value representing the toggle to display the data trendline. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param xTickToggle A logical value representing the toggle to display the ticks on the x-axis. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param facetWrapBy A character string representing the name of the data column to be used for facet wrapping. (Default: `NULL`)
#' @param facetColumn A numeric value representing the number of columns to be used for facet wrapping. (Default: `NULL`)
#' @param facetRow A numeric value representing the number of rows to be used for face wrapping. (Default: `NULL`)
#' @param plotColumn A numeric value representing the number of columns to be used for plotting (Default: `1`)
#' @param plotRow A numeric value representing the number of rows to be used for plotting (Default: `1`)
#' @returns A data frame of the boundary analysis result if performed; otherwise, this function does not return any value.
#' It prints the ggplot objects displaying the multiple time series data.
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
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    commonColumn = commonColumn,
    x = x,
    firstColumnIndex = firstColumnIndex,
    lastColumnIndex = lastColumnIndex,
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
    trendlineToggle = trendlineToggle,
    xTickToggle = xTickToggle,
    facetWrapBy = facetWrapBy,
    facetColumn = facetColumn,
    facetRow = facetRow,
    plotColumn = plotColumn,
    plotRow = plotRow
  )
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else if (i == "x") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "firstColumnIndex" || i == "plotColumn" || i == "plotRow" || (i == "boundaryValue" && boundary %in% c("value", "percentage"))) {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "lastColumnIndex" || i == "facetColumn" || i == "facetRow") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    } else if (i == "colour" || i == "shape" || i == "facetWrapBy") {
      validateNullableCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "title" || i == "subtitle" || i == "caption" || i == "xLabel" || i == "yLabel" || i == "colourLabel" || i == "shapeLabel") {
      validateNullableCharacterString(name = i, value = parameter[[i]])
    } else if (i == "boundary" && !is.null(parameter[[i]]) &&
               (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || !parameter[[i]] %in% c("value", "percentage", "sd"))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a character string of length 1 ('value', 'percentage', or 'sd'"))
    } else if (i == "trendlineToggle" || i == "xTickTogle") {
      validateLogicalValue(name = i, value = parameter[[i]])
    }
    
    if ((i == "firstColumnIndex" || (i == "lastColumnIndex" && !is.null(parameter[[i]]))) && (parameter[[i]] < 1 || parameter[[i]] > ncol(data))) {
      stop(paste0("Invalid '", i, "': Data column index out of bound"))
    }
  }
  
  parameter <- c(commonColumn, x, colour, shape, facetWrapBy)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to display ", title, ": Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  for (i in firstColumnIndex:lastColumnIndex) {
    y <- colnames(data)[i]
    validateNumericVectorElement(name = paste0("data[[", y, "]]"), value = data[[y]])
  }
  
  if (!is.null(referenceData) && boundary == "sd") {
    if (!is.data.frame(referenceData)) {
      stop("Invalid 'referenceData': Must be a data frame")
    } else if (!identical(colnames(referenceData), colnames(data))) {
      stop("Invalid 'referenceData': Data columns must be identical to 'data'")
    }
    
    if (nrow(referenceData) < 2) {
      message("Invalid 'referenceData': Insufficient data to generate statistics (Setting default)")
      referenceData <- NULL
    } else {
      for (i in firstColumnIndex:lastColumnIndex) {
        y <- colnames(data)[i]
        validateStatisticalNumericVectorElement(name = paste0("referenceData[[", y, "]]"), value = referenceData[[y]])
      }
    }
  }
  
  # Display multiple time series
  tryCatch({
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- plotColumn * plotRow
    
    result <- if (!is.null(boundary)) {
      data %>%
        dplyr::select(all_of(commonColumn))
    }
    
    for (i in firstColumnIndex:lastColumnIndex) {
      y <- colnames(data)[i]
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
      
      if (trendlineToggle) {
        timeSeries <- timeSeries +
          ggplot2::geom_smooth(colour = "red", fill = "grey", alpha = 0.25)
      }
    
      if (!xTickToggle) {
        timeSeries <- timeSeries +
          ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
      
      if (!is.null(facetWrapBy)) {
        timeSeries <- timeSeries +
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
                !!y := dplyr::case_when(
                  .data[[y]] < lowerBound ~ "Low",
                  .data[[y]] >= lowerBound & .data[[y]] <= upperBound ~ "Normal",
                  .data[[y]] > upperBound ~ "High"
                )
              ) %>%
              dplyr::select(all_of(commonColumn), !!y) %>%
              dplyr::left_join(result, ., by = commonColumn)
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
              
              timeSeries <- timeSeries +
                ggplot2::geom_hline(yintercept = mean, linetype = "dashed", colour = "grey") +
                ggplot2::geom_hline(yintercept = lowerBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = upperBound1, linetype = "dashed", colour = "blue") +
                ggplot2::geom_hline(yintercept = lowerBound2, linetype = "dashed", colour = "red") +
                ggplot2::geom_hline(yintercept = upperBound2, linetype = "dashed", colour = "red")
              
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
              result <- data %>%
                dplyr::mutate(!!y := "Normal") %>%
                dplyr::select(all_of(commonColumn), !!y) %>%
                dplyr::left_join(result, ., by = commonColumn)
            }
          }
        },
        warning = function(w) {
          message(paste0("Unable to perform '", boundary, "' type boundary analysis for ", y, ": ", w))
          result <- result %>%
            dplyr::mutate(!!y := NA_character_)
        },
        error = function(e) {
          message(paste0("Unable to perform '", boundary, "' type boundary analysis for ", y, ": ", w))
          result <- result %>%
            dplyr::mutate(!!y := NA_character_)
        })
      }
      
      plotList[[y]] <- timeSeries
      suppressMessages(
        if (length(plotList) %% nPlotData == 0 || i == lastColumnIndex) {
          plotGrid <- ggpubr::ggarrange(
            plotlist = plotList,
            ncol = plotColumn,
            nrow = plotRow,
            common.legend = TRUE,
            legend = ifelse(isFirstPlot, "top", "none")
          )
          
          if (isFirstPlot) {
            plotGrid <- ggpubr::annotate_figure(plotGrid, top = ggpubr::text_grob(paste0(title, ": ", subtitle)))
          }
          
          print(plotGrid)
          
          plotList <- list()
          isFirstPlot <- FALSE
        }
      )
    }
    
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