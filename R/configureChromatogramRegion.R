#' Configure Chromatogram Region
#'
#' Configure the start and end time points of the chromatogram regions of interest:
#' - Prewash
#' - Mass calibration
#' - Analyte
#' - Wash
#'
#' @export
#' @param massCalStart A numeric value representing the start of the mass calibration region. (Default: `0`)
#' @param massCalEnd A numeric value representing the end of the mass calibration region.
#' @param analyteStart A numeric value representing the start of the analyte region. (Default: `massCalEnd`)
#' @param analyteEnd A numeric value representing the end of the analyte region. (Default: `washStart`)
#' @param washStart A numeric value representing the start of the wash region.
#' @param washEnd A numeric value representing the end of the wash region. (Default: `NULL`)
#' @returns A list of chromatogram region data of interests.
#'
#' @examples
#' # Example 1: Using default values
#' configureChromatogramRegion(massCalibrationEnd = 0.3, washStart = 5)
#'
#' # Example 2: Specifying values
#' configureChromatogramRegion(massCalibrationStart = 0, massCalibrationEnd = 0.3, analyteStart = 2, analyteEnd = 4, washStart = 5, washEnd = 6)
configureChromatogramRegion <- function(massCalibrationStart = 0,
                                        massCalibrationEnd,
                                        analyteStart = massCalibrationEnd,
                                        analyteEnd = washStart,
                                        washStart,
                                        washEnd = NULL) {
  # Validate parameters
  parameter <- list(
    massCalibrationStart = massCalibrationStart,
    massCalibrationEnd = massCalibrationEnd,
    analyteStart = analyteStart,
    analyteEnd = analyteEnd,
    washStart = washStart,
    washEnd = washEnd
  )
  for (i in names(parameter)) {
    if (i == "washEnd") {
      validateNullableNumericValue(name = i, value = parameter[[i]])
    } else {
      validateNumericValue(name = i, value = parameter[[i]])
    }
  }
  
  # Configure chromatogram region data
  return(
    list(
      prewash = list(start = 0, end = washStart),
      massCalibration = list(start = massCalibrationStart, end = massCalibrationEnd),
      analyte = list(start = analyteStart, end = analyteEnd),
      wash = list(start = washStart, end = washEnd)
    )
  )
}