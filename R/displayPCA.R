#' Display Principal Component Analysis (PCA)
#'
#' Perform Principle Component Analysis (PCA) and display analysis outcomes (scree plot, scores plot, loadings plot and/or biplot).
#'
#' @import ggplot2
#' @import HotellingEllipse
#' @import ggforce
#'
#' @export
#' @param data A data frame containing data to perform PCA on (Must be equal sized and should not contain NA values): data frame
#' @param firstColumnIndex Index of the first data column to perform PCA on: numeric
#' @param lastColumnIndex Index of the last data column data to perform PCA on (Default: NULL): NULL or numeric
#' @param scale PCA scaling (Default: TRUE, Options: TRUE or FALSE): logical
#' @param confidence Data confidence % (Default: 95, Options: 95 or 99): numeric
#' @param distribution Data distribution to be drawn on scores plot and biplot (Default: NULL, Options: "normal" or "t"): NULL or character
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param shape Column name representing data to be used for shape grouping (Default: NULL): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param shapeLabel Shape grouping label (Default: Value used for shape): NULL or character
#' @param screePlotToggle Toggle to display scree plot (Default: TRUE, Options: TRUE or FALSE): logical
#' @param scoresPlotToggle Toggle to display scores plot (Default: TRUE, Options: TRUE or FALSE): logical
#' @param loadingsPlotToggle Toggle to display loadings plot (Default: TRUE, Options: TRUE or FALSE): logical
#' @param biPlotToggle Toggle to display biplot (Default: TRUE, Options: TRUE or FALSE): logical
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
  
  
  # Perform PCA
  tryCatch({
    # PCA summary
    pca <- summary(prcomp(if (is.null(lastColumnIndex)) data[, firstColumnIndex:ncol(data)] else data[, firstColumnIndex:lastColumnIndex], scale = scale))
    
    # Identify variances
    variance <- data.frame(
      principalComponent = paste0("PC", 1:length(pca$importance[1, ])),
      standardDev = pca$importance[1, ],
      varianceProportion = pca$importance[2, ] * 100,
      cumulativeProportion = pca$importance[3, ] * 100
    )
    
    # Check PC threshold status
    threshold <- which(variance$cumulativeProportion >= confidence)[1]
    if (threshold > 1) {
      # Plot scree plot
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
      
        # Display scree plot
        if (screePlotToggle == TRUE) {
          print(screePlot)
        }
      
      tryCatch({
        # PCA scores and loadings data
        scores <- data.frame(pca$x)
        loading <- data.frame(pca$rotation)
        
        # Loop through PCs that passes the threshold
        for (i in 1:(threshold - 1)) {
          for (j in (i+1):threshold) {
            # Plot scores
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
            
              # Hotelling Ellipse
              hotelling <- HotellingEllipse::ellipseParam(data = scores, k = 2, pcx = i, pcy = j)
              if (confidence == 95) {
                scoresPlot <- scoresPlot +
                  ggforce::geom_ellipse(
                    aes(x0 = 0, y0 = 0, a = hotelling$Ellipse$a.95pct, b = hotelling$Ellipse$b.95pct, angle = 0), linetype = "dashed", colour = "red"
                  ) +
                  ggplot2::labs(caption = "Hotelling T² Ellipse (95% Confidence)")
              } else if (confidence == 99) {
                scoresPlot <- scoresPlot +
                  ggforce::geom_ellipse(
                    aes(x0 = 0, y0 = 0, a = hotelling$Ellipse$a.99pct, b = hotelling$Ellipse$b.99pct, angle = 0), linetype = "dashed", colour = "red"
                  ) +
                  ggplot2::labs(caption = "Hotelling T² Ellipse (99% Confidence)")
              }
              
              # Data distribution
              if (!is.null(distribution)) {
                scoresPlot <- scoresPlot +
                  ggplot2::stat_ellipse(type = distribution, level = confidence / 100, linewidth = 0.25)
              }
            
              # Display scores plot
              if (scoresPlotToggle == TRUE) {
                print(scoresPlot)
              }
            
            # Plot loadings
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
            
              # Display loadings plot
              if (loadingsPlotToggle == TRUE) {
                print(loadingsPlot)
              }
            
            # Plot biplot
            biplot <- scoresPlot +
              ggplot2::geom_segment(
                data = loading,
                aes(x = 0, y = 0, xend = .data[[paste0("PC", i)]], yend = .data[[paste0("PC", j)]]),
                arrow = arrow(length = unit(0.25, "cm")),
                color = "red"
              ) +
              ggplot2::geom_text(data = loading, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]], label = row.names(loading))) + 
              ggplot2::labs(title = "Principal Component Analysis: Biplot")
              
              # Display biplot
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