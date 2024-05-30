#' configureChromatogramRegion
#'
#' Configure the start and end time points of the different chromatogram regions of interest: prewash, mass calibration, analyte and wash. 
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
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.3,
#'                                                         washStart = 5)
#' print(chromatogramRegion)
#'
#' # Example 2: Defining values for analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.3,
#'                                                         analyteStart = 2, analyteEnd = 4
#'                                                         washStart = 5, washEnd = 6)
#' print(chromatogramRegion)
configureChromatogramRegion <- function(massCalStart = 0,
                                        massCalEnd,
                                        analyteStart = massCalEnd,
                                        analyteEnd = washStart,
                                        washStart,
                                        washEnd = NULL) {
  # Configure the start and end time points for prewash, mass calibration, analyte, and wash regions
  return(list(prewash = list(start = 0, end = washStart),
              massCal = list(start = massCalStart, end = massCalEnd),
              analyte = list(start = analyteStart, end = analyteEnd),
              wash = list(start = washStart, end = washEnd)))
}