#' Display Chromatogram
#'
#' Display chromatogram data.
#'
#' @import ggplot2
#'
#' @export
#' @param data A data frame containing plot data.
#' @param x A character string representing the name of the data column to be used for the x-axis.
#' @param y A character string representing the name of the data column to be used for the y-axis.
#' @param colour A character string representing the name of the data column to be used for colour grouping. (Default: `NULL`)
#' @param title A character string representing the plot title. (Default: `"Chromatogram"`)
#' @param subtitle A character string representing the plot subtitle. (Default: `NULL`)
#' @param caption A character string representing the plot caption. (Default: `NULL`)
#' @param xLabel A character string representing the x-axis label. (Default: Value used for `x`)
#' @param yLabel A character string representing the y-axis label. (Default: Value used for `y`)
#' @param colourLabel A character string representing the colour grouping label. (Default: Value used for `colour`)
#' @param xTickToggle A logical value representing the toggle to display ticks on the x-axis. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param facetWrapBy A character string representing the name of the data column to be used for facet wrapping. (Default: `NULL`)
#' @param facetColumn A numeric value representing the number of columns to be used for facet wrapping. (Default: `NULL`)
#' @param facetRow A numeric value representing the number of rows to be used for face wrapping. (Default: `NULL`)
#' @param chromatogramRegion A list of lists representing the different chromatogram regions of interest and their respective start and end time points. (Default: `NULL`; Options: Use `ticq::configureChromatogramRegion()`)
#' @returns This function does not return any value. It prints the ggplot object displaying the chromatogram.
displayChromatogram <- function(data,
                                x,
                                y,
                                colour = NULL,
                                title = "Chromatogram",
                                subtitle = NULL,
                                caption = NULL,
                                xLabel = x,
                                yLabel = y,
                                colourLabel = colour,
                                xTickToggle = TRUE,
                                facetWrapBy = NULL,
                                facetColumn = NULL,
                                facetRow = NULL,
                                chromatogramRegion = NULL) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(x = x, y = y, colour = colour, title = title, subtitle = subtitle, caption = caption, xLabel = xLabel, yLabel = yLabel, colourLabel = colourLabel,
                    xTickToggle = xTickToggle, facetWrapBy = facetWrapBy, facetColumn = facetColumn, facetRow = facetRow, chromatogramRegion = chromatogramRegion)
  for (i in names(parameter)) {
    if (i == "x" || i == "y") {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    } else if ((i == "colour" || i == "facetWrapBy") && !is.null(parameter[[i]]) &&
               (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == "")) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a non-NA, non-empty character string of length 1"))
    } else if (i == "xTickToggle") {
      validateLogicalValue(parameterName = i, parameterValue = parameter[[i]])
    } else if (i == "chromatogramRegion" && !is.null(parameter[[i]]) && !validateChromatogramRegion(parameterName = i, parameterValue = parameter[[i]])) {
      stop(paste0("Invalid '", i, "': Must either be NULL or contain a valid list of chromatogram region data"))
    } else if ((i == "facetColumn" || i == "facetRow") && !is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.numeric(parameter[[i]]))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a numeric value of length 1"))
    } else if ((i == "title" || i == "subtitle" || i == "caption" || i == "xLabel" || i == "yLabel" || i == "colourLabel") &&
               !is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]))){
      stop(paste0("Invalid '", i, "': Must either be NULL or a character string of length 1"))
    }
  }
  
  parameter <- c(x, y, colour, facetWrapBy)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to display ", title, ": Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Display chromatogram
  tryCatch({
    # Plot data
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
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
      ) + 
      ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel, colour = colourLabel)
    
    # Display x-axis ticks
    if (!xTickToggle) {
      chromatogram <- chromatogram +
        ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    
    # Facet wrap
    if (!is.null(facetWrapBy)) {
      chromatogram <- chromatogram +
        ggplot2::facet_wrap( ~ .data[[facetWrapBy]], ncol = facetColumn, nrow = facetRow)
    }
      
    # Display chromatogram regions
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
    
    print(chromatogram)
  },
  warning = function(w) message(paste0("Unable to display ", title, ": ", w)),
  error = function(e) message(paste0("Unable to display ", title, ": ", e)))
}