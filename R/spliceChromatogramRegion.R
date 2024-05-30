#' spliceChromatogramRegion
#'
#' Splice chromatogram regions and sum values of a specific data column together.
#'
#' @export
#' @param data A data frame containing spectral data: data frame
#' @param commonColumn Column names of data common for each unique sample: character vector
#' @param spectrumCount Spectrum count column name: character
#' @param chromatogramRegion A list containing the start and end time points of the different chromatogram regions of interest (prewash, maass calibration, analyte and wash) (Default: NULL): NULL or list
#' @param retentionTime Retention time column name: character
#' @param sumBy Column name of the data to be summed: character
#' @returns A data frame containing the summed values for each chromatogram regions of interest
spliceChromatogramRegion <- function(data,
                                     commonColumn,
                                     spectrumCount,
                                     chromatogramRegion = NULL,
                                     retentionTime,
                                     sumBy) {
  # Base data frame to append spliced chromatogram region data
  splicedData <- ticq::countSpectrum(data = data,
                                     commonColumn = commonColumn,
                                     spectrumCount = spectrumCount)
  
  # Splice overall chromatogram
  tmp <- ticq::sumDataColumn(data = data,
                             commonColumn = commonColumn,
                             sumBy = sumBy)
  splicedData <- cbind(splicedData, overallRegion = tmp$sum)
  
  # Splice chromatogram regions of interest
  if (!is.null(chromatogramRegion)) {
    # Prewash region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$prewash,
                                          retentionTime = retentionTime)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, prewashRegion = tmp$sum)
    
    # Mass calibration region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$massCal,
                                          retentionTime = retentionTime)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, massCalRegion = tmp$sum)
    
    # Analyte region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$analyte,
                                          retentionTime = retentionTime)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, analyteRegion = tmp$sum)
    
    # Wash region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$wash,
                                          retentionTime = retentionTime)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, washRegion = tmp$sum)
  }
  
  return(splicedData)
}