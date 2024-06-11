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
  # Data frame to store splice chromatogram data
  splicedData <- ticq::countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount)
  
  # Splice overall chromatogram
  regionData <- ticq::sumDataColumn(data = data, commonColumn = commonColumn, sumColumn = sumColumn)
  splicedData <- cbind(splicedData, overallRegion = regionData$sum)
  
  # Splice chromatogram regions of interest
  if (!is.null(chromatogramRegion)) {
    # Prewash region
    regionData <- ticq::filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion$prewash, retentionTime = retentionTime)
    regionData <- ticq::sumDataColumn(data = regionData, commonColumn = commonColumn, sumColumn = sumColumn)
    splicedData <- cbind(splicedData, prewashRegion = regionData$sum)
    
    # Mass calibration region
    regionData <- ticq::filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion$massCalibration, retentionTime = retentionTime)
    regionData <- ticq::sumDataColumn(data = regionData, commonColumn = commonColumn, sumColumn = sumColumn)
    splicedData <- cbind(splicedData, massCalibrationRegion = regionData$sum)
    
    # Analyte region
    regionData <- ticq::filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion$analyte, retentionTime = retentionTime)
    regionData <- ticq::sumDataColumn(data = regionData, commonColumn = commonColumn, sumColumn = sumColumn)
    splicedData <- cbind(splicedData, analyteRegion = regionData$sum)
    
    # Wash region
    regionData <- ticq::filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion$wash, retentionTime = retentionTime)
    regionData <- ticq::sumDataColumn(data = regionData, commonColumn = commonColumn, sumColumn = sumColumn)
    splicedData <- cbind(splicedData, washRegion = regionData$sum)
  }
  
  return(splicedData)
}