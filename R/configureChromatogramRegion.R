#' Configure Chromatogram Region
#'
#' Configure the start and end time points of the different chromatogram regions of interest:
#' - Prewash region
#' - Mass calibration region
#' - Analyte region
#' - Wash region
#'
#' @export
#' @param massCalStart A numeric value that represents the starting time point of the mass calibration region. (Default: `0`)
#' @param massCalEnd A numeric value that represents the end time point of the mass calibration region.
#' @param analyteStart A numeric value that represents the starting time point of the analyte region. (Default: Value used for `massCalEnd`)
#' @param analyteEnd A numeric value that represents the end time point of the analyte region. (Default: Value used for `washStart`)
#' @param washStart A numeric value that represents the starting time point of the wash region.
#' @param washEnd A numeric value that represents the end time point of the wash region. (Default: `NULL`)
#' @returns A list of lists representing the different chromatogram regions of interest and their respective start and end time points.
#'
#' @examples
#' # Example 1: Using default values for massCalibrationStart, analyteStart, analyteEnd and washEnd
#' configureChromatogramRegion(massCalibrationEnd = 0.3, washStart = 5)
#'
#' # Example 2: Specifying values for analyteStart, analyteEnd and washEnd
#' configureChromatogramRegion(massCalibrationStart = 0, massCalibrationEnd = 0.3, analyteStart = 2, analyteEnd = 4, washStart = 5, washEnd = 6)
configureChromatogramRegion <- function(massCalibrationStart = 0,
                                        massCalibrationEnd,
                                        analyteStart = massCalibrationEnd,
                                        analyteEnd = washStart,
                                        washStart,
                                        washEnd = NULL) {
  # Validate parameters
  parameter <- list(massCalibrationStart = massCalibrationStart, massCalibrationEnd = massCalibrationEnd, analyteStart = analyteStart, analyteEnd = analyteEnd,
                    washStart = washStart, washEnd = washEnd)
  for (i in names(parameter)) {
    if (i == "washEnd" && !is.null(parameter[[i]]) && (length(parameter[[i]]) != 1 || !is.numeric(parameter[[i]]))) {
      stop(paste0("Invalid '", i, "': Must either be NULL or a numeric value of length 1"))
    } else if (i != "washEnd") {
      validateNumericValue(parameterName = i, parameterValue = parameter[[i]])
    }
  }
  
  # Configure start and end time points of each chromatogram region of interest
  return(
    list(
      prewash = list(start = 0, end = washStart),
      massCalibration = list(start = massCalibrationStart, end = massCalibrationEnd),
      analyte = list(start = analyteStart, end = analyteEnd),
      wash = list(start = washStart, end = washEnd)
    )
  )
}