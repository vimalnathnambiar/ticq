#' Splice Chromatogram Region
#'
#' Splice MS spectral data of each data group by summing the values of a data column using the chromatogram region data of interests (start and end time points).
#'
#' @export
#' @param data A data frame containing MS spectral data.
#' @param commonColumn A character vector representing the names of the common data columns to be used for data grouping.
#' @param spectrumCount A character string representing the name of the spectrum count column.
#' @param chromatogramRegion A list representing the chromatogram region data of interests. (Default: `NULL`; Options: `configureChromatogramRegion()`)
#' @param retentionTime A character string representing the name of the retention time column.
#' @param sumColumn A character string representing the name of the data column with values to be summed.
#' @returns A data frame of MS spectral data summary grouped by their common data columns and spliced data of each chromatogram regions of interest.
spliceChromatogramRegion <- function(data, commonColumn, spectrumCount, chromatogramRegion = NULL, retentionTime, sumColumn) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    commonColumn = commonColumn,
    spectrumCount = spectrumCount,
    chromatogramRegion = chromatogramRegion,
    retentionTime = retentionTime,
    sumColumn = sumColumn
  )
  for (i in names(parameter)) {
    if (i == "commonColumn") {
      validateCharacterVectorElement(name = i, value = parameter[[i]])
    } else if (i == "spectrumCount" || i == "retentionTime" || i == "sumColumn") {
      validateCharacterStringValue(name = i, value = parameter[[i]])
    } else if (i == "chromatogramRegion" && !is.null(parameter[[i]]) && !validateChromatogramRegion(name = i, value = parameter[[i]])) {
      stop(paste0(
        "Invalid '", i, "': Must either be NULL or a list of the chromatogram region data of interests"
      ))
    }
  }
  
  parameter <- c(commonColumn, spectrumCount, retentionTime, sumColumn)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to splice chromatogram region: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Splice chromatogram regions
  splicedChromatogramData <- countSpectrum(data = data, commonColumn = commonColumn, spectrumCount = spectrumCount) %>%
    mutate(overallRegion = sumDataColumn(data = data, commonColumn = commonColumn, sumColumn = sumColumn)$sum)
  
  if (!is.null(chromatogramRegion)) {
    pattern <- c("prewash", "massCalibration", "analyte", "wash")
    for (i in pattern) {
      splicedChromatogramData[[paste0(i, "Region")]] <- (
        filterChromatogramRegion(data = data, regionOfInterest = chromatogramRegion[[i]], retentionTime = retentionTime) %>%
        sumDataColumn(data = ., commonColumn = commonColumn, sumColumn = sumColumn)
      )$sum
    }
  }
  
  return(splicedChromatogramData)
}