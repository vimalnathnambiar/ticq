#' Generate Statistics
#'
#' Generate statistical information of a dataset including: 
#' - Sample size (n)
#' - Minimum (min)
#' - Mean (mean)
#' - Maximum (max)
#' - 1st Quartile (Q1)
#' - Median
#' - 3rd Quartile (Q3)
#' - Inter-quantile range (IQR)
#' - IQR inner fence (Lower and upper)
#' - IQR outer fence (Lower and upper)
#' - 1st standard deviation (sd)
#' - 2nd standard deviation (sd2)
#' - Standard error (se)
#' - Margin error (me) - 95% and 99%
#' - Confidence interval (CI) - 95% and 99% (Lower and upper)
#' - Confidence quantile range (CQ) - 95% and 99% (Lower and upper)
#' 
#' Data must have a minimum of 2 data points and must not contain any NA values.
#'
#' @export
#' @param data A continuous data series of numerical values (Must have a minimum of 2 data points and do not contain NA values): numerical vector
#' @returns A list of all generated statistics
#'
#' @examples
#' stats <- ticq::generateStat(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' print(stats)
generateStat <- function(data) {
  # Sample size (n)
  n <- length(data)
  
  # Min, Mean, Max
  min <- min(data)
  mean <- mean(data)
  max <- max(data)
  
  # Quantiles
  Q1 <- quantile(data, 0.25)
  median <- quantile(data, 0.5)
  Q3 <- quantile(data, 0.75)
  
  # Inter-quantile Ranges (IQR)
  IQR <- Q3 - Q1
  
    # Inner Fences
    lowerInnerFence <- Q1 - (1.5 * IQR)
    upperInnerFence <- Q3 + (1.5 * IQR)
    
    # Outer Fences
    lowerOuterFence <- Q1 - (3 * IQR)
    upperOuterFence <- Q3 - (3 * IQR)
  
  # Standard Deviations (1st and 2nd)
  sd <- sd(data)
  sd2 <- sd(2 * data)
  
  # Standard Error
  se <- sd / sqrt(n)
  
  # Confidence Alpha Value & Probability Area
    # 95%
    alpha95 <- 1 - 0.95
    probArea95 <- alpha95 / 2
    
    # 99%
    alpha99 <- 1 - 0.99
    probArea99 <- alpha99 / 2
  
  # Margin Error
  me95 <- 0
  me99 <- 0
  
    # Check sample size
    if (n < 30) {
      # Calculate using t-critical score
        # 95%
        tCritical95 <- abs(qt(probArea95, (n - 1)))
        me95 <- tCritical95 * se
        
        # 99%
        tCritical99 <- abs(qt(probArea99, (n - 1)))
        me99 <- tCritical99 * se
    } else {
      # Calculate using z-score
        # 95%
        zScore95 <- 1.960
        me95 <- zScore95 * se
        
        # 99%
        zScore99 <- 2.576
        me99 <- zScore99 * se
    }
  
  # Confidence Interval
    # 95%
    lowerCI95 <- mean - me95
    upperCI95 <- mean + me95
    
    # 99%
    lowerCI99 <- mean - me99
    upperCI99 <- mean + me99
  
  # Confidence Quantile Range
    # 95%
    lowerCQ95 <- quantile(data, probArea95)
    upperCQ95 <- quantile(data, (1 - probArea95))
    
    # 99%
    lowerCQ99 <- quantile(data, probArea99)
    upperCQ99 <- quantile(data, (1 - probArea99))
  
  return(
    list(
      n = n,
      min = min,
      mean = mean,
      max = max,
      Q1 = Q1,
      median = median,
      Q3 = Q3,
      IQR = IQR,
      lowerInnerFence = lowerInnerFence,
      upperInnerFence = upperInnerFence,
      lowerOuterFence = lowerOuterFence,
      upperOuterFence = upperOuterFence,
      sd = sd,
      sd2 = sd2,
      se = se,
      me95 = me95,
      me99 = me99,
      lowerCI95 = lowerCI95,
      upperCI95 = upperCI95,
      lowerCI99 = lowerCI99,
      upperCI99 = upperCI99,
      lowerCQ95 = lowerCQ95,
      upperCQ95 = upperCQ95,
      lowerCQ99 = lowerCQ99,
      upperCQ99 = upperCQ99
    )
  )
}