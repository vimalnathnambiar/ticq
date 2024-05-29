#' spliceChromatogramRegion
#'
#' Splice chromatogram regions and sum values of a specific data column together.
#'
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn Column names that are common across all samples: character vector
#' @param spectrumCount Name of the column that represents spectrum count: character
#' @param chromatogramRegion List that represents the different chromatogram regions of interest (Default: NULL): NULL or list
#' @param filterBy Name of the column that represents the retention time to be filtered: character
#' @param sumBy Name of the column that represents the data to be summed for each chromatogram region: character
#' @returns Data frame containing the sum values of the spliced chromatogram regions
spliceChromatogramRegion <- function(data,
                                     commonColumn,
                                     spectrumCount,
                                     chromatogramRegion = NULL,
                                     filterBy,
                                     sumBy) {
  # Data frame with common columns to append spliced chromatogram region data
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
                                          filterBy = filterBy)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, prewashRegion = tmp$sum)
    
    # Mass calibration region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$massCal,
                                          filterBy = filterBy)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, massCalRegion = tmp$sum)
    
    # Analyte region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$analyte,
                                          filterBy = filterBy)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, analyteRegion = tmp$sum)
    
    # Wash region
    tmp <- ticq::filterChromatogramRegion(data = data,
                                          regionOfInterest = chromatogramRegion$wash,
                                          filterBy = filterBy)
    tmp <- ticq::sumDataColumn(data = tmp,
                               commonColumn = commonColumn,
                               sumBy = sumBy)
    splicedData <- cbind(splicedData, washRegion = tmp$sum)
  }
  
  return(splicedData)
}