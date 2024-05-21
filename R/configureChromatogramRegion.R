#' configureChromatogramRegion
#' 
#' Configure chromatogram regions based on specific time points.
#' 
#' @export
#' @param massCalStart Fixed time point when the MS instrument starts its mass calibration cycle (Default: 0): double
#' @param massCalEnd Fixed time point that the MS instrument ends its mass calibration cycle: double
#' @param analyteStart Fixed time point when the MS instrument starts its cycle to scan for targeted features (Default: End of mass calibration cycle): double
#' @param analyteEnd Fixed time point when the MS instrument ends its cycle to scan for targeted features (Default: Start of wash cycle): double
#' @param washStart Fixed time point that the MS instrument starts its wash cycle: double
#' @param washEnd Fixed time point that the MS instrument ends its wash cycle (Default: NULL): NULL or double
#' @returns A list containing the time points representing the different regions of the chromatogram
#' 
#' @examples
#' # Example 1: Using default values for analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.3, 
#'                                                         washStart = 5)
#'
#' # Example 2: Defining values for analyteStart, analyteEnd and washEnd
#' chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.3,
#'                                                         analyteStart = 2, analyteEnd = 4
#'                                                         washStart = 5, washEnd = 6)
configureChromatogramRegion <- function(massCalStart = 0, 
                                        massCalEnd, 
                                        analyteStart = massCalEnd, 
                                        analyteEnd = washStart, 
                                        washStart, 
                                        washEnd = NULL) {
  return(list(prewash = list(start = 0, end = washStart),
              massCal = list(start = massCalStart, end = massCalEnd),
              analyte = list(start = analyteStart, end = analyteEnd),
              wash = list(start = washStart, end = washEnd)))
}