#' plotPCA
#' 
#' Plot data generated from Principle Component Analysis (PCA): Scree plot, scores plot, loadings plot and biplot..
#' 
#' @import ggplot2
#' @import HotellingEllipse
#' @import ggforce
#' 
#' @export
#' @param data An equal size data frame that contains columns representing data (variance) to perform PCA on. Should not contain NA values: data frame
#' @param startIDX The index of the first column data to perform PCA on: double
#' @param endIDX The index of the last column data to perform PCA on (Default: NULL): NULL or double
#' @param scale Scaling status for the PCA (Default: TRUE, Options: TRUE or FALSE): boolean
#' @param confidence Confidence value of the data (Default: 95): double
#' @param distribution Add distribution boundaries to the PCA scores plot (Default: NULL, Options: "normal" or "t"): NULL or character
#' @param colour The name of the column that will be used to colour categorise the data (Default: NULL): NULL or character
#' @param subtitle The subtitle for the plot (Default: NULL): NULL or character
#' @param legend Legend label that represents the different colour categories (Default: the name of the column used to colour categorise the data): NULL or character
plotPCA <- function(data,
                    startIDX,
                    endIDX = NULL,
                    scale = TRUE, 
                    confidence = 95,
                    distribution = NULL,
                    colour = NULL,
                    subtitle = NULL,
                    legend = NULL,
                    screePlt = FALSE,
                    scoresPlt = FALSE,
                    loadingsPlt = FALSE,
                    biPlt = FALSE) {
  tryCatch({
    # Ensure no NA values in data frame
    if (is.null(endIDX)) {
      tmp <- data[, startIDX:ncol(data)]
    } else {
      tmp <- data[, startIDX:endIDX]
    }
    tmp <- tmp[complete.cases(tmp), ]
    
    # Generate PCA data
    pca <- summary(prcomp(tmp, scale = scale))
    
    # Variance data
    variance <- data.frame(principalComponent = paste0("PC", 1:length(pca$importance[1, ])),
                           standardDev = pca$importance[1, ],
                           varianceProportion = pca$importance[2, ] * 100,
                           cumulativeProportion = pca$importance[3, ] * 100)
    
    # Threshold based on variance
    threshold <- which(variance$cumulativeProportion >= confidence)[1]
    
    # Print plots
    # If threshold is more than 1
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
      
      
      if (screePlt == TRUE) {
        print(screePlot)
      }
      
      tryCatch({
        # Plot scores and loadings of the PCA
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
            
              # If colour is NULL
              if (is.null(colour)) {
                scoresPlot <- scoresPlot + 
                  ggplot2::geom_point(alpha = 0.25)
              } else {
                scoresPlot <- scoresPlot +
                  ggplot2::geom_point(aes(colour = data[[colour]],
                                          group = ifelse(data[[colour]] == "Historical", 1, 2)),
                                      alpha = 0.25)
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
                              colour = legend,
                              caption = paste0("Hotelling T² Ellipse (Blue: 95% Confidence; Red: 99% Confidence)"))
              
              # Add distribution at a specific confidence
              # If distribution is NOT NULL
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
              ggplot2::labs(title = "PCA Biplot",
                            subtitle = subtitle,
                            x = paste0("PC", i, " [", round(pca$importance[2, i] * 100, 2), "%]"),
                            y = paste0("PC", j, " [", round(pca$importance[2, j] * 100, 2), "%]"),
                            colour = legend,
                            caption = paste0("Hotelling T² Ellipse (Blue: 95% Confidence; Red: 99% Confidence)"))
            
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