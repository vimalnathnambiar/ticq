#' Validate Chromatogram Region
#'
#' Validate chromatogram region data of interests (prewash, mass calibration, analyte and wash).
#'
#' @param chromatogramRegion A list representing the different chromatogram regions of interest (Options: ticq::configureChromatogramRegion()): list
#' @returns Validation status (TRUE or FALSE)
validateChromatogramRegion <- function(chromatogramRegion) {
  # Chromatogram region data patterns
  regionPattern <- c("prewash", "massCalibration", "analyte", "wash")
  timePattern <- c("start", "end")
  
  # Validate chromatogram region data
  if (length(chromatogramRegion) != 4 || !is.list(chromatogramRegion) || !all(regionPattern %in% names(chromatogramRegion))) {
    message("Invalid 'chromatogramRegion': Must be a list containing 4 chromatogram region data (prewash, massCalibration, analyte and wash)")
    if (!all(regionPattern %in% names(chromatogramRegion))) {
      message(paste0(
        "Missing one or more region data (",
        paste(regionPattern[!regionPattern %in% names(chromatogramRegion)], collapse = ", "),
        ")"
      ))
    }
    return(FALSE)
  } else {
    for (i in regionPattern) {
      if (!is.list(chromatogramRegion[[i]]) || !all(timePattern %in% names(chromatogramRegion[[i]]))) {
        message(paste0("Invalid '", i, "' region: Must be a list containing 'start' and 'end' time points"))
        if (!all(timePattern %in% names(chromatogramRegion[[i]]))) {
          message(paste0(
            "Missing one or more time points (",
            paste(timePattern[!timePattern %in% names(chromatogramRegion[[i]])], collapse = ", "),
            ")"
          ))
        }
        return(FALSE)
      } else {
        if (length(chromatogramRegion[[i]][[timePattern[[1]]]]) != 1 || !is.numeric(chromatogramRegion[[i]][[timePattern[[1]]]])) {
          message(paste("Invalid 'start' for", i, "region: Must be a numerical value of length 1"))
          return(FALSE)
        }
        
        if ((i == "wash" && !is.null(chromatogramRegion[[i]][[timePattern[[2]]]]) &&
             (length(chromatogramRegion[[i]][[timePattern[[2]]]]) != 1 || !is.numeric(chromatogramRegion[[i]][[timePattern[[2]]]]))) ||
            (i != "wash" &&
             (length(chromatogramRegion[[i]][[timePattern[[2]]]]) != 1 || !is.numeric(chromatogramRegion[[i]][[timePattern[[2]]]])))) {
          if (i == "wash") {
            message(paste("Invalid 'end' for", i, "region: Must be NULL or a numerical value of length 1"))
          } else {
            message(paste("Invalid 'end' for", i, "region: Must be a numerical value of length 1"))
          }
          return (FALSE)
        }
      }
    }
  }
  
  return(TRUE)
}