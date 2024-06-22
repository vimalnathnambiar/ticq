#' Splice Chromatogram Region
#'
#' Splice spectral data by summing the values of a column by the respective start and end time points of the different chromatogram regions of interest.
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names representing common data to be used for data grouping: character
#' @param spectrumCount Spectrum count column name: character
#' @param chromatogramRegion A list representing the different chromatogram regions of interest (Default: NULL, Options: ticq::configureChromatogramRegion()): NULL or list
#' @param retentionTime Retention time column name: character
#' @param sumColumn Column name of the data to be summed: character
#' @returns A data frame containing spliced spectral data of the different chromatogram regions of interest
spliceChromatogramRegion <- function(data, commonColumn, spectrumCount, chromatogramRegion = NULL, retentionTime, sumColumn) {
  # Splice overall region
  splicedChromatogramData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount) %>%
    mutate(overallRegion = ticq::sumDataColumn(data = data, commonColumn = commonColumn, sumColumn = sumColumn)$sum)
  
  # Splice chromatogram regions
  if (!is.null(chromatogramRegion)) {
    pattern <- c("prewash", "massCalibration", "analyte", "wash")
    for (i in pattern) {
      splicedChromatogramData[[paste0(i, "Region")]] <- (
        ticq::filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion[[i]], retentionTime = retentionTime) %>%
        ticq::sumDataColumn(data = ., commonColumn = commonColumn, sumColumn = sumColumn)
      )$sum
    }
  }
  
  return(splicedChromatogramData)
}