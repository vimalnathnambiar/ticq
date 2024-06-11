#' Configure Chromatogram Region
#'
#' Configure the start and end time points of the different chromatogram regions of interest:
#' - Prewash region
#' - Mass calibration region
#' - Analyte region
#' - Wash region
#'
#' @export
#' @param massCalStart Start of mass calibration cycle (Default: 0): double
#' @param massCalEnd End of mass calibration cycle: double
#' @param analyteStart Start of feature detection cycle (Default: End of mass calibration cycle): double
#' @param analyteEnd End of feature detection cycle (Default: Start of wash cycle): double
#' @param washStart Start of wash cycle: double
#' @param washEnd End of wash cycle (Default: NULL): NULL or double
#' @returns A list containing the start and end time points of the different chromatogram regions of interest (prewash, mass calibration, analyte, and wash)
#'
#' @examples
#' # Example 1: Using default values for analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalibrationStart = 0, massCalibrationEnd = 0.3, washStart = 5)
#' print(chromatogramRegion)
#'
#' # Example 2: Defining values for analyteStart, analyteEnd and washEnd
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
  # Configure the start and end time points for prewash, mass calibration, analyte, and wash regions
  return(
    list(
      prewash = list(start = 0, end = washStart),
      massCalibration = list(start = massCalibrationStart, end = massCalibrationEnd),
      analyte = list(start = analyteStart, end = analyteEnd),
      wash = list(start = washStart, end = washEnd)
    )
  )
}