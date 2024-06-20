#' Extract Plate
#'
#' Extract plate number.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#' @import stringr
#'
#' @param input ANPC sample file name or path: character
#' @returns Plate number
extractPlate <- function(input) {
  # Plate number patterns
  pattern <- "[pP][0-9]+|PLATE[0-9]+|PLASMA[0-9]+"
  
  # Identify plate number
  plate <- stringr::str_extract(input, pattern) %>%
    stringr::str_replace_all("^[a-zA-Z0]+", "")
  
  plate[is.na(plate) | plate == ""] <- NA_character_
  
  return(plate)
}