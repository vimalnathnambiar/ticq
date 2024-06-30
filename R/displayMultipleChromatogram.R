#' Display Multiple Chromatogram
#'
#' Display multiple chromatogram data.
#' 
#' Chromatogram region can be configured using ticq::configureChromatogramRegion().
#'
#' @import ggplot2
#' @import ggpubr
#'
#' @export
#' @param data A data frame of the chromatogram data to be used for plotting.
#' @param x A character string representing the name of the data column to be used for the x-axis.
#' @param firstColumnIndex A numeric value representing the index of the first data column to be used for y-axis.
#' @param lastColumnIndex A numeric value representing the index of the last data column to be used for y-axis. (Default: `NULL`)
#' @param colour A character string representing the name of the data column to be used for colour grouping. (Default: `NULL`)
#' @param title A character string representing the plot title. (Default: `"Chromatogram"`)
#' @param subtitle A character string representing the plot subtitle. (Default: `NULL`)
#' @param caption A character string representing the plot caption. (Default: `NULL`)
#' @param xLabel A character string representing the x-axis label. (Default: `x`)
#' @param yLabel A character string representing the y-axis label. (Default: `y`)
#' @param colourLabel A character string representing the colour grouping label. (Default: `colour`)
#' @param xTickToggle A logical value representing the toggle to display the ticks on the x-axis. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param facetWrapBy A character string representing the name of the data column to be used for facet wrapping. (Default: `NULL`)
#' @param facetColumn A numeric value representing the number of columns to be used for facet wrapping. (Default: `NULL`)
#' @param facetRow A numeric value representing the number of rows to be used for face wrapping. (Default: `NULL`)
#' @param plotColumn A numeric value representing the number of columns to be used for plotting (Default: `1`)
#' @param plotRow A numeric value representing the number of rows to be used for plotting (Default: `1`)
#' @param chromatogramRegion A list representing the chromatogram region data of interests. (Default: `NULL`; Options: `configureChromatogramRegion()`)
#' @returns This function does not return any value. It prints the ggplot objects displaying the multiple chromatogram.
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
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    x = x,
    firstColumnIndex = firstColumnIndex,
    lastColumnIndex = lastColumnIndex,
    colour = colour,
    title = title,
    subtitle = subtitle,
    caption = caption,
    xLabel = xLabel,
    yLabel = yLabel,
    colourLabel = colourLabel,
    xTickToggle = xTickToggle,
    facetWrapBy = facetWrapBy,
    facetColumn = facetColumn,
    facetRow = facetRow,
    plotColumn = plotColumn,
    plotRow = plotRow,
    chromatogramRegion = chromatogramRegion
  )
  for (i in names(parameter)) {
    if (i == "x") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "firstColumnIndex" || i == "plotColumn" || i == "plotRow") {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "lastColumnIndex" || i == "facetColumn" || i == "facetRow") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    } else if (i == "colour" || i == "facetWrapBy") {
      validateNullableCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "title" || i == "subtitle" || i == "caption" || i == "xLabel" || i == "yLabel" || i == "colourLabel") {
      validateNullableCharacterString(name = i, value = parameter[[i]])
    } else if (i == "xTickTogle") {
      validateLogicalValue(name = i, value = parameter[[i]])
    } else if (i == "chromatogramRegion" && !is.null(parameter[[i]]) && !validateChromatogramRegion(name = i, value = parameter[[i]])) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a list of the chromatogram region data of interests"))
    }
    
    if ((i == "firstColumnIndex" || (i == "lastColumnIndex" && !is.null(parameter[[i]]))) && (parameter[[i]] < 1 || parameter[[i]] > ncol(data))) {
      stop(paste0("Invalid '", i, "': Data column index out of bound"))
    }
  }
  
  parameter <- c(x, colour, facetWrapBy)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to display ", title, ": Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  if (is.null(lastColumnIndex)) {
    lastColumnIndex <- ncol(data)
  }
  
  # Display multiple chromatogram
  tryCatch({
    plotList <- list()
    isFirstPlot <- TRUE
    nPlotData <- plotColumn * plotRow
    
    for (i in firstColumnIndex:lastColumnIndex) {
      y <- colnames(data)[i]
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
      
      if (!xTickToggle) {
        chromatogram <- chromatogram +
          ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
      
      if (!is.null(facetWrapBy)) {
        chromatogram <- chromatogram +
          ggplot2::facet_wrap( ~ .data[[facetWrapBy]], ncol = facetColumn, nrow = facetRow)
      }
      
      if (!is.null(chromatogramRegion)) {
        label <- list(massCalibration = "Mass Calibration Region", analyte = "Analyte Region", wash = "Wash Region")
        for (i in names(label)) {
          chromatogram <- displayChromatogramRegion(
            plot = chromatogram,
            maxX = max(data[[x]]),
            maxY = max(data[[y]]),
            regionOfInterest = chromatogramRegion[[i]],
            label = label[[i]]
          )
        }
      }
      
      plotList[[y]] <- chromatogram
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
    }
  },
  warning = function(w) message(paste0("Unable to display ", title, ": ", w)),
  error = function(e) message(paste0("Unable to display ", title, ": ", e)))
}