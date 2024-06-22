#' Generate Statistics
#'
#' Generate statistical information of a dataset including: 
#' - Sample size (n)
#' - Sum (sum)
#' - Minimum (min)
#' - Mean (mean)
#' - Maximum (max)
#' - 1st Quartile (Q1)
#' - Median
#' - 3rd Quartile (Q3)
#' - Inter-quantile range (IQR)
#' - IQR inner fence (Lower and upper)
#' - IQR outer fence (Lower and upper)
#' - Standard deviation (sd)
#' - Standard error (se)
#' - Margin error (me) - 95% and 99%
#' - Confidence interval (CI) - 95% and 99% (Lower and upper)
#' - Confidence quantile range (CQ) - 95% and 99% (Lower and upper)
#'
#' @export
#' @param data A continuous data series of numerical values (Must have a minimum of 2 data points and should not contain any NA values): numeric
#' @returns A list of generated statistical information
#'
#' @examples
#' stats <- ticq::generateStat(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' print(stats)
generateStat <- function(data) {
  # Validate parameters
  if (length(data) <= 1 || !is.numeric(data) || any(is.na(data))) {
    stop("Invalid 'data': Must be a numerical value of length 2 or more with no NA values")
  }
  
  # Sample size (n)
  n <- length(data)
  
  # Sum, Minimum, Mean, Maximum
  sum <- sum(data)
  min <- min(data)
  mean <- mean(data)
  max <- max(data)
  
  # Quantiles
  Q1 <- quantile(data, 0.25)
  median <- quantile(data, 0.5)
  Q3 <- quantile(data, 0.75)
  
  # Inter-quantile Ranges (IQR)
  IQR <- Q3 - Q1
  
  lowerInnerFence <- Q1 - (1.5 * IQR)
  upperInnerFence <- Q3 + (1.5 * IQR)
  lowerOuterFence <- Q1 - (3 * IQR)
  upperOuterFence <- Q3 - (3 * IQR)
  
  # Standard Deviation
  sd <- sd(data)
  
  # Standard Error
  se <- sd / sqrt(n)
  
  # Confidence Alpha Value & Probability Area
  alpha95 <- 1 - 0.95
  probArea95 <- alpha95 / 2
  
  alpha99 <- 1 - 0.99
  probArea99 <- alpha99 / 2
  
  # Margin Error
  me95 <- 0
  me99 <- 0
  
  if (n < 30) {
    # Calculate using t-critical score
    tCritical95 <- abs(qt(probArea95, (n - 1)))
    me95 <- tCritical95 * se
    
    tCritical99 <- abs(qt(probArea99, (n - 1)))
    me99 <- tCritical99 * se
  } else {
    # Calculate using z-score
    zScore95 <- 1.960
    me95 <- zScore95 * se

    zScore99 <- 2.576
    me99 <- zScore99 * se
  }
  
  # Confidence Interval
  lowerCI95 <- mean - me95
  upperCI95 <- mean + me95
  
  lowerCI99 <- mean - me99
  upperCI99 <- mean + me99
  
  # Confidence Quantile Range
  lowerCQ95 <- quantile(data, probArea95)
  upperCQ95 <- quantile(data, (1 - probArea95))
  
  lowerCQ99 <- quantile(data, probArea99)
  upperCQ99 <- quantile(data, (1 - probArea99))
  
  return(
    list(
      n = n,
      sum = sum,
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