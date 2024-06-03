#' Plot PCA
#'
#' Perform Principle Component Analysis (PCA) and display associated plots (scree plot, scores plot, loadings plot and biplot).
#' 
#' Data used for PCA MUST BE equal sized and MUST NOT contain NA values.
#'
#' @import ggplot2
#' @import HotellingEllipse
#' @import ggforce
#'
#' @export
#' @param data A data frame containing data to perform PCA (Equal sized and must not contain NA values): data frame
#' @param startIDX Index of the first data column to perform PCA on: double
#' @param endIDX Index of the last data column data to perform PCA on (Default: NULL): NULL or double
#' @param scale PCA scaling (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param confidence Data confidence % (Default: 95): double
#' @param distribution Data distribution to be drawn on scores plot and biplot (Default: NULL, Options: "normal" or "t"): NULL or character
#' @param colour Column name representing data to be used for colour grouping (Default: NULL): NULL or character
#' @param shape Column name representing data to be used for shape grouping (Default: NULL): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param colourLabel Colour grouping label (Default: Value used for colour): NULL or character
#' @param shapeLabel Shape grouping label (Default: Value used for shape): NULL or character
#' @param screePlotToggle Toggle to display scree plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param scoresPlotToggle Toggle to display scores plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param loadingsPlotToggle Toggle to display loadings plot (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param biPlotToggle Toggle to display biplot (Default: TRUE, Options: TRUE or FALSE): boolean
plotPCA <- function(data,
                    startIDX,
                    endIDX = NULL,
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
  tryCatch({
    # Check and ensure no NA values
    if (is.null(endIDX)) {
      tmp <- data[, startIDX:ncol(data)]
    } else {
      tmp <- data[, startIDX:endIDX]
    }
    tmp <- tmp[complete.cases(tmp), ]
    
    # Perform PCA
    pca <- summary(prcomp(tmp, scale = scale))
    
    # Identify variances
    variance <- data.frame(principalComponent = paste0("PC", 1:length(pca$importance[1, ])),
                           standardDev = pca$importance[1, ],
                           varianceProportion = pca$importance[2, ] * 100,
                           cumulativeProportion = pca$importance[3, ] * 100)
    
    # Check PC threshold status
    threshold <- which(variance$cumulativeProportion >= confidence)[1]
    if (threshold > 1) {
      # Plot scree plot
      screePlot <- ggplot2::ggplot(data = variance, aes(x = principalComponent, y = varianceProportion, group = 1)) + 
        ggplot2::geom_col(fill = "orange") +
        ggplot2::geom_point() +
        ggplot2::geom_line(aes(y = varianceProportion, colour = "Proportion of Variance")) +
        ggplot2::geom_line(aes(y = cumulativeProportion, colour = "Cumulative Proportion"),
                           linetype = "dashed") +
        ggplot2::geom_vline(aes(xintercept = threshold, colour = "Threshold"),
                            linetype = "dotted") +
        ggplot2::geom_text(aes(label = paste0(round(varianceProportion, 2), "%")),
                           vjust = -0.5,
                           size = 3) +
        ggplot2::theme(panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.title = element_text(size = 8),
                       legend.text = element_text(size = 8)) +
        ggplot2::scale_colour_manual(values = c("red", "darkgreen", "blue"),
                                     labels = c("Cumulative Proportion", "Proportion of Variance", "Threshold (>=95%)")) +
        ggplot2::labs(title = "Scree Plot",
                      subtitle = subtitle,
                      x = "Principal Components",
                      y = "Proportion of Variance (%)")
      
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
            # Generate Hotelling Ellipse
            hotelling <- HotellingEllipse::ellipseParam(data = scores,
                                                        k = 2,
                                                        pcx = i,
                                                        pcy = j)
            
            # Plot scores
            scoresPlot <- ggplot2::ggplot(data = scores, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]])) +
              ggplot2::geom_point(aes(colour = if (!is.null(colour)) data[[colour]] else NULL,
                                      shape = if (!is.null(shape)) data[[shape]] else NULL,
                                      group = if (!is.null(colour)) ifelse(data[[colour]] == "Historical", 1, 2) else NULL),
                                  alpha = 0.25) +
              ggplot2::geom_vline(xintercept = 0,
                                  linetype = "solid",
                                  colour = "grey",
                                  linewidth = 0.2) +
              ggplot2::geom_hline(yintercept = 0,
                                  linetype = "solid",
                                  colour = "grey",
                                  linewidth = 0.2) +
              ggforce::geom_ellipse(aes(x0 = 0, y0 = 0, a = hotelling$Ellipse$a.95pct, b = hotelling$Ellipse$b.95pct, angle = 0),
                                    linetype = "dashed",
                                    colour = "blue") +
              ggforce::geom_ellipse(aes(x0 = 0, y0 = 0, a = hotelling$Ellipse$a.99pct, b = hotelling$Ellipse$b.99pct, angle = 0),
                                    linetype = "dashed",
                                    colour = "red") +
              ggplot2::theme(panel.background = element_blank(),
                             axis.line = element_line(colour = "black"),
                             legend.title = element_text(size = 8),
                             legend.text = element_text(size = 8)) +
              ggplot2::labs(title = "PCA Scores Plot",
                            subtitle = subtitle,
                            x = paste0("PC", i, " [", round(pca$importance[2, i] * 100, 2), "%]"),
                            y = paste0("PC", j, " [", round(pca$importance[2, j] * 100, 2), "%]"),
                            colour = colourLabel,
                            shape = shapeLabel,
                            caption = paste0("Hotelling T² Ellipse (95% (Blue) and 99% (Red) Confidence)"))
              
              # Display data distribution if defined
              if (!is.null(distribution)) {
                if (distribution == "normal") {
                  scoresPlot <- scoresPlot +
                    ggplot2::stat_ellipse(type = "norm",
                                          level = confidence / 100,
                                          linewidth = 0.25)
                } else if (distribution == "t") {
                  scoresPlot <- scoresPlot +
                    ggplot2::stat_ellipse(type = "t",
                                          level = confidence / 100,
                                          linewidth = 0.25)
                }
              }
              
              # Display scores plot
              if (scoresPlotToggle == TRUE) {
                print(scoresPlot)
              }
            
            # Plot loadings
            loadingsPlot <- ggplot2::ggplot(data = loading, aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]])) +
              ggplot2::geom_point(aes(colour = row.names(loading))) +
              ggplot2::theme(panel.background = element_blank(),
                             axis.line = element_line(colour = "black"),
                             legend.title = element_text(size = 8),
                             legend.text = element_text(size = 8)) +
              ggplot2::labs(title = "PCA Loadings Plot",
                            subtitle = subtitle,
                            x = paste0("PC", i, " [", round(pca$importance[2, i] * 100, 2), "%]"),
                            y = paste0("PC", j, " [", round(pca$importance[2, j] * 100, 2), "%]"),
                            colour = "Variables")
            
              # Display loadings plot
              if (loadingsPlotToggle == TRUE) {
                print(loadingsPlot)
              }
            
            # Plot biplot
            biplot <- scoresPlot +
              ggplot2::geom_segment(data = loading,
                                    aes(x = 0, y = 0, xend = .data[[paste0("PC", i)]], yend = .data[[paste0("PC", j)]]),
                                    arrow = arrow(length = unit(0.25, "cm")),
                                    color = "red") +
              ggplot2::geom_text(data = loading,
                                 aes(x = .data[[paste0("PC", i)]], y = .data[[paste0("PC", j)]], label = row.names(loading))) + 
              ggplot2::labs(title = "PCA Biplot")
              
              # Display biplot
              if (biPlotToggle == TRUE) {
                print(biplot)
              }
          }
        }
      },
      warning = function(w) print(paste0("Unable to display Principal Component Analysis - ", w)),
      error = function(e) print(paste0("Unable to display Principal Component Analysis - ", e)))
    } else {
      print(paste("Unable to display Principal Component Analysis - Only PC1 satisfies the threshold limit"))
    }
  },
  warning = function(w) print(paste0("Unable to perform Principal Component Analysis - ", w)),
  error = function(e) print(paste0("Unable to perform Principal Component Analysis - ", e)))
}