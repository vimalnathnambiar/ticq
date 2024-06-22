#' Display Chromatogram
#'
#' Display chromatogram data.
#'
#' @import ggplot2
#'
#' @export
#' @param data A data frame containing plot data: data frame
#' @param x Column name representing the continuous data series to be used for x-axis: character
#' @param y Column name representing the continuous data series to be used for y-axis: character
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param title Plot title (Default: "Chromatogram"): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param caption Plot caption (Default: NULL): NULL or character
#' @param xLabel x-axis label (Default: Column name used for x): NULL or character
#' @param yLabel y-axis label (Default:Column name used for y): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param xTickToggle Toggle to display ticks on x-axis (Default: TRUE, Options: TRUE or FALSE): logical
#' @param facetWrapBy Column name representing data to be used for facet wrapping (Default: NULL): NULL or character
#' @param facetColumn Number of columns to use for facet wrapping (Default: NULL): NULL or numeric
#' @param facetRow Number of rows to use for facet wrapping (Default: NULL): NULL or numeric
#' @param chromatogramRegion A list representing the different chromatogram regions of interest (Default: NULL, Options: ticq::configureChromatogramRegion()): NULL or list
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
    stop("Invalid 'data': Data frame is empty")
  }
  
  parameter <- list(
    x = x,
    y = y,
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
  )
  for (i in names(parameter)) {
    if ((i == "x" || i == "y") && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == "") ||
        ((i == "colour" || i == "facetWrapBy") && !is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || is.na(parameter[[i]]) || parameter[[i]] == "")) ||
        (i == "xTickToggle" && (length(parameter[[i]]) != 1 || !is.logical(parameter[[i]]) || is.na(parameter[[i]]))) ||
        ((i == "facetColumn" || i == "facetRow") && (length(parameter[[i]]) != 1 || !is.numeric(parameter[[i]])))) {
      if (i == "x" || i == "y") {
        stop(paste0("Invalid '", i, "': Must be a non-NA and non-empty character string of length 1 matching a column name in 'data'"))
      } else if (i == "colour" || i == "facetWrapBy") {
        stop(paste0("Invalid '", i, "': Must be NULL or a non-NA and non-empty character string of length 1 matching a column name in 'data'"))
      } else if (i == "xTickToggle") {
        stop(paste0("Invalid '", i, "': Must be a logical value of length 1 (TRUE or FALSE)"))
      } else if (i == "facetColumn" || i == "facetRow") {
        stop(paste0("Invalid '", i, "': Must be a numeric value of length 1"))
      }
    } else if (!is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]))) {
      stop(paste0("Invalid '", i, "': Must be NULL or a character string of length 1"))
    }
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