#' spliceChromatogramRegion
#' 
#' Splice chromatogram regions and sum values of a specific data column together for each sample. 
#' 
#' @export
#' @param data Data frame that contains spectral information of samples: data frame
#' @param commonColumn A vector of column names that are common across all samples: character vector
#' @param spectrumCount The name of the column that represents spectrum count: character
#' @param chromatogramRegion A list that represent the different chromatogram regions (Default: NULL): NULL or list
#' @param filterBy The name of the column that represents the retention time to be filtered: character
#' @param sumBy The name of the column that represents the data to be summed for each chromatogram region: character
#' @returns A data frame containing the sum values of the spliced regions of the chromatogram
spliceChromatogramRegion <- function(data, 
                                     commonColumn, 
                                     spectrumCount, 
                                     chromatogramRegion = NULL, 
                                     filterBy, 
                                     sumBy) {
  # splicedData (A temporary data frame) with common columns for data append
  splicedData <- ticq::countSpectrum(data = data, 
                                     commonColumn = commonColumn, 
                                     spectrumCount = spectrumCount)
  
  # Splice overall chromatogram
  tmp <- ticq::sumDataColumn(data = data,
                             commonColumn = commonColumn,
                             sumBy = sumBy)
  splicedData <- cbind(splicedData, overallRegion = tmp$sum)
  
  # If spliceRegion is NOT NULL
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