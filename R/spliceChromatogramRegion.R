#' Splice Chromatogram Region
#'
#' Splice spectral data by summing the values of a data column by the respective start and end time points of the different chromatogram regions of interest.
#'
#' @export
#' @param data A data frame containing spectral data.
#' @param commonColumn A character vector representing names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param chromatogramRegion A list of lists representing the different chromatogram regions of interest and their respective start and end time points. (Default: `NULL`; Options: Use `ticq::configureChromatogramRegion()`)
#' @param retentionTime A character string representing the name of the retention time column.
#' @param sumColumn A character string representing the name of the data column to be summed.
#' @returns A data frame containing a summarised listing of samples along with the spliced data of the different chromatogram regions of interest.
spliceChromatogramRegion <- function(data, commonColumn, spectrumCount, chromatogramRegion = NULL, retentionTime, sumColumn) {
  # Validate parameters
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Invalid 'data': Empty data frame")
  }
  
  parameter <- list(commonColumn = commonColumn, spectrumCount = spectrumCount, chromatogramRegion = chromatogramRegion, retentionTime = retentionTime, sumColumn = sumColumn)
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVector(parameterName = i, parameterValue = parameter[[i]])
    } else if (i == "chromatogramRegion" && !is.null(parameter[[i]]) && !validateChromatogramRegion(parameterName = i, parameterValue = parameter[[i]])) {
      stop(paste0("Invalid '", i, "': Must either be NULL or contain a valid list of chromatogram region data"))
    } else if (i != "commonColumn" && i != "chromatogramRegion") {
      validateCharacterString(parameterName = i, parameterValue = parameter[[i]])
    }
  }
  
  parameter <- c(commonColumn, spectrumCount, retentionTime, sumColumn)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to splice chromatogram region: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
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