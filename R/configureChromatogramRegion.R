#' Configure Chromatogram Region
#'
#' Configure the start and end time points of the different chromatogram regions of interest:
#' - Prewash region
#' - Mass calibration region
#' - Analyte region
#' - Wash region
#'
#' @export
#' @param massCalStart Start of mass calibration cycle (Default: 0): numeric
#' @param massCalEnd End of mass calibration cycle: numeric
#' @param analyteStart Start of feature detection cycle (Default: End of mass calibration cycle): numeric
#' @param analyteEnd End of feature detection cycle (Default: Start of wash cycle): numeric
#' @param washStart Start of wash cycle: numeric
#' @param washEnd End of wash cycle (Default: NULL): NULL or numeric
#' @returns A list containing the start and end time points of the different chromatogram regions of interest
#'
#' @examples
#' # Example 1: Using default values for massCalibrationStart, analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalibrationEnd = 0.3, washStart = 5)
#' print(chromatogramRegion)
#'
#' # Example 2: Specifying values for analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalibrationStart = 0, massCalibrationEnd = 0.3,
#'                                                         analyteStart = 2, analyteEnd = 4,
#'                                                         washStart = 5, washEnd = 6)
#' print(chromatogramRegion)
configureChromatogramRegion <- function(massCalibrationStart = 0,
                                        massCalibrationEnd,
                                        analyteStart = massCalibrationEnd,
                                        analyteEnd = washStart,
                                        washStart,
                                        washEnd = NULL) {
  # Validate parameters
  parameter <- list(massCalibrationStart = massCalibrationStart, massCalibrationEnd = massCalibrationEnd, analyteStart = analyteStart,
                    analyteEnd = analyteEnd, washStart = washStart, washEnd = washEnd)
  for (i in names(parameter)) {
    parameterValue <- parameter[[i]]
    if (i == "washEnd") {
      if (!is.null(parameterValue) && (length(parameterValue) != 1 || !is.numeric(parameterValue))) {
        stop(paste0("Invalid '", i, "': Must be NULL or numerical value of length 1"))
      }
    } else if (length(parameterValue) != 1 || !is.numeric(parameterValue)) {
      stop(paste0("Invalid '", i, "': Must be numerical value of length 1"))
    }
  }
  
  # Configure start and end time points of each region of interest
  return(
    list(
      prewash = list(start = 0, end = washStart),
      massCalibration = list(start = massCalibrationStart, end = massCalibrationEnd),
      analyte = list(start = analyteStart, end = analyteEnd),
      wash = list(start = washStart, end = washEnd)
    )
  )
}