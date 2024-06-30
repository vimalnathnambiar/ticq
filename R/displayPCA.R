#' Display PCA
#'
#' Perform principle component analysis (PCA) and display analysis outcomes:
#' - Scree plot
#' - Scores plot
#' - Loadings plot
#' - Biplot
#'
#' @import ggplot2
#' @import HotellingEllipse
#' @import ggforce
#'
#' @export
#' @param data A data frame of the data to be used to perform PCA.
#' @param firstColumnIndex A numeric value representing the index of the first data column to perform PCA.
#' @param lastColumnIndex A numeric value representing the index of the last data column to perform PCA. (Default: `NULL`)
#' @param scale A logical value representing the scaling to be used for the PCA. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param confidence A numeric value representing the data confidence (%) to be used for identifying the principle component (PC) threshold.
#' The value is also used to draw the data distribution and confidence (Hotelling T² Ellipse) on the scores plot and biplot.
#' (Default: `95`, Options: `95` or `99`)
#' @param distribution A character string representing the data distribution type to be drawn on the scores plot and biplot.
#' (Default: `NULL`, Options: `"normal"` or `"t"`)
#' @param colour A character string representing the name of the data column to be used for colour grouping. (Default: `NULL`)
#' @param shape A character string representing the name of the data column to be used for shape grouping. (Default: `NULL`)
#' @param subtitle A character string representing the plot subtitle. (Default: `NULL`)
#' @param colourLabel A character string representing the colour grouping label. (Default: `colour`)
#' @param shapeLabel A character string representing the shape grouping label. (Default: `shape`)
#' @param screePlotToggle A logical value representing the toggle to display the generated scree plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param scoresPlotToggle A logical value representing the toggle to display the generated scores plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param loadingsPlotToggle A logical value representing the toggle to display the generated loadings plot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @param biPlotToggle A logical value representing the toggle to display the generated biplot. (Default: `TRUE`, Options: `TRUE` or `FALSE`)
#' @returns This function does not return any value. It prints the ggplot objects representing the scree plot, scores plot, loadings plot, and/or biplot.
displayPCA <- function(data,
                       firstColumnIndex,
                       lastColumnIndex = NULL,
                       scale = TRUE,
                       confidence = 95,
                       distribution = NULL,
                       colour = NULL,
                       shape = NULL,
                       subtitle = NULL,
                       colourLabel = colour,
                       shapeLabel = shape,
                       screePlotToggle = TRUE,
                       scoresPlotToggle = TRUE,
                       loadingsPlotToggle = TRUE,
                       biPlotToggle = TRUE) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    firstColumnIndex = firstColumnIndex,
    lastColumnIndex = lastColumnIndex,
    scale = scale,
    confidence = confidence,
    distribution = distribution,
    colour = colour,
    shape = shape,
    subtitle = subtitle,
    colourLabel = colourLabel,
    shapeLabel = shapeLabel,
    screePlotToggle = screePlotToggle,
    scoresPlotToggle = scoresPlotToggle,
    loadingsPlotToggle = loadingsPlotToggle,
    biPlotToggle = biPlotToggle
  )
  for (i in names(parameter)) {
    if (i == "firstColumnIndex") {
      validateNumericValue(name = i, value = parameter[[i]])
    } else if (i == "lastColumnIndex") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    } else if (i == "scale" || i == "screePlotToggle" || i == "scoresPlotToggle" || i == "loadingsPlotToggle" || i == "biPlotToggle") {
      validateLogicalValue(name = i, value = parameter[[i]])
    } else if (i == "confidence" && (length(parameter[[i]]) != 1 || !is.numeric(parameter[[i]]) || !parameter[[i]] %in% c(95, 99))) {
      stop(paste0("Invalid '", i, "': Must be a numeric value of length 1 (95 or 99)"))
    } else if (i == "distribution" && !is.null(parameter[[i]]) &&
               (length(parameter[[i]]) != 1 || !is.character(parameter[[i]]) || !parameter[[i]] %in% c("normal", "t"))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a character string of length 1 ('normal' or 't'"))
    } else if (i == "colour" || i == "shape") {
      validateNullableCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "subtitle" || i == "colourLabel" || i == "shapeLabel") {
      validateNullableCharacterString(name = i, value = parameter[[i]])
    }
    
    if ((i == "firstColumnIndex" || (i == "lastColumnIndex" && !is.null(parameter[[i]]))) && (parameter[[i]] < 1 || parameter[[i]] > ncol(data))) {
      stop(paste0("Invalid '", i, "': Data column index out of bound"))
    }
  }
  
  parameter <- c(colour, shape)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0(
      "Unable to perform Principal Component Analysis: Missing one or more data column (", 
      paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"
    ))
  }
  
  # Perform PCA
  tryCatch({
    pca <- summary(prcomp(if (is.null(lastColumnIndex)) data[, firstColumnIndex:ncol(data)] else data[, firstColumnIndex:lastColumnIndex], scale = scale))
    variance <- data.frame(
      principalComponent = paste0("PC", 1:length(pca$importance[1, ])),
      standardDev = pca$importance[1, ],
      varianceProportion = pca$importance[2, ] * 100,
      cumulativeProportion = pca$importance[3, ] * 100
    )
    
    # PCA outcomes
    threshold <- which(variance$cumulativeProportion >= confidence)[1]
    if (threshold > 1) {
      # Display scree plot
      screePlot <- ggplot2::ggplot(data = variance, aes(x = principalComponent, y = varianceProportion, group = 1)) + 
        ggplot2::geom_col(fill = "orange") +
        ggplot2::geom_point() +
        ggplot2::geom_line(aes(y = varianceProportion, colour = "Proportion of Variance")) +
        ggplot2::geom_line(aes(y = cumulativeProportion, colour = "Cumulative Proportion"), linetype = "dashed") +
        ggplot2::geom_vline(aes(xintercept = threshold, colour = "Threshold"), linetype = "dotted") +
        ggplot2::geom_text(aes(label = paste0(round(varianceProportion, 2), "%")), vjust = -0.5, size = 3) +
        ggplot2::theme(
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        ) +
        ggplot2::scale_colour_manual(
          values = c("red", "darkgreen", "blue"),
          labels = c("Cumulative Proportion", "Proportion of Variance", paste("Threshold", paste0("(>=", confidence, "%)")))
        ) +
        ggplot2::labs(title = "Principal Component Analysis: Scree Plot", subtitle = subtitle, x = "Principal Components", y = "Proportion of Variance (%)")
      
        if (screePlotToggle == TRUE) {
          print(screePlot)
        }
      
      # PCA scores and loadings
      tryCatch({
        scores <- data.frame(pca$x)
        loading <- data.frame(pca$rotation)
        
        for (i in 1:(threshold - 1)) {
          for (j in (i+1):threshold) {
            # Display scores plot
            scoresPlot <- ggplot2::ggplot(data = scores, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]])) +
              ggplot2::geom_point(
                aes(
                  colour = if (!is.null(colour)) data[[colour]] else NULL,
                  shape = if (!is.null(shape)) data[[shape]] else NULL,
                  group = if (!is.null(colour)) ifelse(data[[colour]] == "Historical", 1, 2) else NULL
                ),
                alpha = 0.25
              ) +
              ggplot2::geom_vline(xintercept = 0, linetype = "solid", colour = "grey", linewidth = 0.2) +
              ggplot2::geom_hline(yintercept = 0,  linetype = "solid", colour = "grey", linewidth = 0.2) +
              ggplot2::theme(
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 8)
              ) +
              ggplot2::labs(
                title = "Principal Component Analysis: Scores Plot",
                subtitle = subtitle,
                x = paste0("PC", i, " [", round(pca$importance[2, i] * 100, 2), "%]"),
                y = paste0("PC", j, " [", round(pca$importance[2, j] * 100, 2), "%]"),
                colour = colourLabel,
                shape = shapeLabel
              )
            
            hotelling <- HotellingEllipse::ellipseParam(data = scores, k = 2, pcx = i, pcy = j)
            scoresPlot <- scoresPlot +
              ggforce::geom_ellipse(
                aes(
                  x0 = 0, y0 = 0, a = hotelling$Ellipse[[paste0("a.", confidence, "pct")]], b = hotelling$Ellipse[[paste0("b.", confidence, "pct")]], angle = 0
                ),
                linetype = "dashed", colour = "red"
              ) +
              ggplot2::labs(caption = paste0("Hotelling T² Ellipse (", confidence, "% Confidence)"))
            
            if (!is.null(distribution)) {
              scoresPlot <- scoresPlot +
                ggplot2::stat_ellipse(type = distribution, level = confidence / 100, linewidth = 0.25)
            }
              
            if (scoresPlotToggle == TRUE) {
              print(scoresPlot)
            }
            
            # Display loadings plot
            loadingsPlot <- ggplot2::ggplot(data = loading, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]])) +
              ggplot2::geom_point(aes(colour = row.names(loading))) +
              ggplot2::theme(
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 8)
              ) +
              ggplot2::labs(
                title = "Principal Component Analysis: Loadings Plot",
                subtitle = subtitle,
                x = paste0("PC", i, " [", round(pca$importance[2, i] * 100, 2), "%]"),
                y = paste0("PC", j, " [", round(pca$importance[2, j] * 100, 2), "%]"),
                colour = "Variables"
              )
              
            if (loadingsPlotToggle == TRUE) {
              print(loadingsPlot)
            }
            
            # Display biplot
            biplot <- scoresPlot +
              ggplot2::geom_segment(
                data = loading,
                aes(x = 0, y = 0, xend = .data[[paste0("PC", i)]], yend = .data[[paste0("PC", j)]]),
                arrow = arrow(length = unit(0.25, "cm")),
                color = "red"
              ) +
              ggplot2::geom_text(data = loading, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]], label = row.names(loading))) + 
              ggplot2::labs(title = "Principal Component Analysis: Biplot")
              
            if (biPlotToggle == TRUE) {
              print(biplot)
            }
          }
        }
      },
      warning = function(w) message(paste("Unable to display Principal Component Analysis:", w)),
      error = function(e) message(paste("Unable to display Principal Component Analysis:", e)))
    } else {
      message("Unable to display Principal Component Analysis: Only PC1 satisfies the threshold limit")
    }
  },
  warning = function(w) message(paste("Unable to perform Principal Component Analysis:", w)),
  error = function(e) message(paste("Unable to perform Principal Component Analysis:", e)))
}