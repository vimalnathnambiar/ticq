#' plotPCA
#' 
#' Perform Principle Component Analysis (PCA) and plot all associated plots (scree plot, scores plot, loadings plot and biplot).
#' 
#' @import ggplot2
#' @import HotellingEllipse
#' @import ggforce
#' 
#' @export
#' @param data Data frame that contains columns representing data to perform PCA on. Should be equal sized and not contain NA values: data frame
#' @param startIDX Index of the first data column to perform PCA on: double
#' @param endIDX Index of the last data column data to perform PCA on (Default: NULL): NULL or double
#' @param scale Scaling status for the PCA (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param confidence Data confidence % (Default: 95): double
#' @param distribution Data distribution. Distribution boundary is drawn on scores plot and biplot (Default: NULL, Options: "normal" or "t"): NULL or character
#' @param colour Name of the column used for colour grouping (Default: NULL): NULL or character
#' @param shape Name of the column used for shape grouping (Default: NULL): NULL or character
#' @param subtitle Plot subtitle (Default: NULL): NULL or character
#' @param colourLegend Legend label for colour grouping (Default: Value used for colour): NULL or character
#' @param shapeLegend Legend label for shape grouping (Default: Value used for shape): NULL or character
#' @param screePlt Toggle printing of scree plot (Default: FALSE, Options: TRUE or FALSE): boolean 
#' @param scoresPlt Toggle printing of scores plot (Default: FALSE, Options: TRUE or FALSE): boolean 
#' @param loadingsPlt Toggle printing of loadings plot (Default: FALSE, Options: TRUE or FALSE): boolean 
#' @param biPlt Toggle printing of biplot (Default: FALSE, Options: TRUE or FALSE): boolean 
plotPCA <- function(data,
                    startIDX,
                    endIDX = NULL,
                    scale = TRUE, 
                    confidence = 95,
                    distribution = NULL,
                    colour = NULL,
                    shape = NULL,
                    subtitle = NULL,
                    colourLegend = colour,
                    shapeLegend = shape,
                    screePlt = FALSE,
                    scoresPlt = FALSE,
                    loadingsPlt = FALSE,
                    biPlt = FALSE) {
  tryCatch({
    # Check and ensure no NA values in data frame
    if (is.null(endIDX)) {
      tmp <- data[, startIDX:ncol(data)]
    } else {
      tmp <- data[, startIDX:endIDX]
    }
    tmp <- tmp[complete.cases(tmp), ]
    
    # Perform PCA
    pca <- summary(prcomp(tmp, scale = scale))
    
    # Identify variance based on the PCA
    variance <- data.frame(principalComponent = paste0("PC", 1:length(pca$importance[1, ])),
                           standardDev = pca$importance[1, ],
                           varianceProportion = pca$importance[2, ] * 100,
                           cumulativeProportion = pca$importance[3, ] * 100)
    
    # Threshold limit based on variance and data confidence %
    threshold <- which(variance$cumulativeProportion >= confidence)[1]
    
    # Check if threshold is more than 1
    if (threshold > 1) {
      # Plot scree plot
      screePlot <- ggplot2::ggplot(data = variance,
                                   aes(x = principalComponent,
                                       y = varianceProportion,
                                       group = 1)) + 
        ggplot2::geom_col(fill = "orange") +
        ggplot2::geom_point() +
        ggplot2::geom_line(aes(y = varianceProportion, 
                               colour = "Proportion of Variance")) +
        ggplot2::geom_line(aes(y = cumulativeProportion, 
                               colour = "Cumulative Proportion"),
                           linetype = "dashed") +
        ggplot2::geom_vline(aes(xintercept = threshold, 
                                colour = "Threshold"),
                            linetype = "dotted") +
        ggplot2::geom_text(aes(label = paste0(round(varianceProportion, 2), "%")),
                           vjust = -0.5, 
                           size = 3) +
        ggplot2::theme(panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.title = element_text(size = 8),
                       legend.text = element_text(size = 8)) +
        ggplot2::scale_colour_manual(values = c("red", "darkgreen", "blue"),
                                     labels = c("Cumulative Proportion",
                                                "Proportion of Variance", 
                                                "Threshold (>=95%)")) +
        ggplot2::labs(title = "Scree Plot",
                      subtitle = subtitle,
                      x = "Principal Components",
                      y = "Proportion of Variance (%)")
      
        # Print scree plot
        if (screePlt == TRUE) {
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
            scoresPlot <- ggplot2::ggplot(data = scores,
                                          aes(x = .data[[paste0("PC", i)]],
                                              y = .data[[paste0("PC", j)]]))
            
            
              # Add colour and shape grouping
              if (is.null(colour)) {
                if (is.null(shape)) {
                  scoresPlot <- scoresPlot +
                    ggplot2::geom_point(alpha = 0.25)
                } else {
                  scoresPlot <- scoresPlot +
                    ggplot2::geom_point(aes(shape = data[[shape]]),
                                        alpha = 0.25)
                }
              } else {
                if (is.null(shape)) {
                  scoresPlot <- scoresPlot +
                    ggplot2::geom_point(aes(colour = data[[colour]],
                                            group = ifelse(data[[colour]] == "Historical", 1, 2)),
                                        alpha = 0.25)
                } else {
                  scoresPlot <- scoresPlot +
                    ggplot2::geom_point(aes(colour = data[[colour]],
                                            shape = data[[shape]],
                                            group = ifelse(data[[colour]] == "Historical", 1, 2)),
                                        alpha = 0.25)
                }
              }
              
              # Add lines
              scoresPlot <- scoresPlot +
                ggplot2::geom_vline(xintercept = 0,
                                    linetype = "solid",
                                    colour = "grey",
                                    linewidth = 0.2) +
                ggplot2::geom_hline(yintercept = 0,
                                    linetype = "solid",
                                    colour = "grey",
                                    linewidth = 0.2) +
                ggforce::geom_ellipse(aes(x0 = 0,
                                          y0 = 0,
                                          a = hotelling$Ellipse$a.95pct,
                                          b = hotelling$Ellipse$b.95pct,
                                          angle = 0),
                                      linetype = "dashed",
                                      colour = "blue") +
                ggforce::geom_ellipse(aes(x0 = 0,
                                          y0 = 0,
                                          a = hotelling$Ellipse$a.99pct,
                                          b = hotelling$Ellipse$b.99pct,
                                          angle = 0),
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
                              colour = colourLegend,
                              shape = shapeLegend,
                              caption = paste0("Hotelling T² Ellipse (Blue: 95% Confidence; Red: 99% Confidence)"))
              
              # Add distribution boundary at a specific confidence
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
              
              # Print scores plot
              if (scoresPlt == TRUE) {
                print(scoresPlot)
              }
            
            # Plot loadings
            loadingsPlot <- ggplot2::ggplot(data = loading,
                                            aes(x = .data[[paste0("PC", i)]], 
                                                y = .data[[paste0("PC", j)]])) +
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
            
              # Print loadings plot
              if (loadingsPlt == TRUE) {
                print(loadingsPlot)
              }
            
            # Plot biplot
            biplot <- scoresPlot +
              ggplot2::geom_segment(data = loading,
                                    aes(x = 0, y = 0, 
                                        xend = .data[[paste0("PC", i)]], 
                                        yend = .data[[paste0("PC", j)]]),
                                    arrow = arrow(length = unit(0.25, "cm")),
                                    color = "red") +
              ggplot2::geom_text(data = loading,
                                 aes(x = .data[[paste0("PC", i)]], 
                                     y = .data[[paste0("PC", j)]], 
                                     label = row.names(loading))) + 
              ggplot2::labs(title = "PCA Biplot")
              
              # Print biplot
              if (biPlt == TRUE) {
                print(biplot)
              }
          }
        }
      },
      warning = function(w) {
        print(paste0("Unable to plot PCA scores and loadings - ", w))
      },
      error = function(e) {
        print(paste0("Unable to plot PCA scores and loadings - ", e))
      })
    } else {
      print(paste("Unable to plot PCA results - Only PC1 satisfies the threshold limit"))
    }
  },
  warning = function(w) {
    print(paste0("Unable to perform Principal Component Analysis - ", w))
  },
  error = function(e) {
    print(paste0("Unable to perform Principal Component Analysis - ", e))
  })
}