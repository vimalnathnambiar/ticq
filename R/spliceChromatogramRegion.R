#' Splice Chromatogram Region
#'
#' Splice chromatogram regions of interest by summing values of a column by the respective start and end time points.
#' 
#' Configure chromatogram region using: ticq::configureChromatogramRegion()
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param chromatogramRegion A list containing the start and end time points of the different chromatogram regions of interest (prewash, maass calibration, analyte and wash) (Default: NULL): NULL or list
#' @param retentionTime Retention time column name: character
#' @param sumColumn Column name of the data to be summed: character
#' @returns A data frame containing the summed values for each chromatogram regions of interest
spliceChromatogramRegion <- function(data, commonColumn, spectrumCount, chromatogramRegion = NULL, retentionTime, sumColumn) {
  # Check and splice chromatogram regions of interests
  splicedChromatogramData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount) %>%
    mutate(overallRegion = ticq::sumDataColumn(data = data, commonColumn = commonColumn, sumColumn = sumColumn)$sum)
  
  if (!is.null(chromatogramRegion)) {
    # Chromatogram region pattern
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