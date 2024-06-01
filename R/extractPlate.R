#' Extract Plate
#'
#' Extract plate number.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#' @param input ANPC sample file name or path: character
#' @returns Plate number
#'
#' @examples
#' plate <- ticq::extractPlate(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(plate)
extractPlate <- function(input) {
  # Plate number patterns
  pattern <- c("[pP][0-9]+", "PLATE[0-9]+", "PLASMA[0-9]+")
  
  # Loop through patterns
  for (p in pattern) {
    # If pattern matches
    if (grepl(p, input)) {
      # Extract pattern
      plate <- stringr::str_extract(input, p)
      
      # Remove non-numeric characters
      plate <- stringr::str_replace(plate, "[a-zA-Z]+", "")
      
      # Remove leading zeros
      plate <- stringr::str_replace(plate, "0+", "")
      
      # If plate number string is empty after removing non-numeric characters and leading 0s
      if (plate == "") {
        plate <- "0"
      }
      
      return(plate)
    }
  }
  
  return(NA_character_)
}