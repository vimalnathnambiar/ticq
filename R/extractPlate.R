#' Extract Plate
#'
#' Extract plate number.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#' @import stringr
#'
#' @param input A character vector representing ANPC sample file names or paths.
#' @returns A character vector representing plate numbers.
extractPlate <- function(input) {
  # Identify and extract plate number
  pattern <- "[pP][0-9]+|PLATE[0-9]+|PLASMA[0-9]+"
  plate <- stringr::str_extract(input, pattern) %>%
    stringr::str_replace_all("^[a-zA-Z0]+", "")
  
  # Replace NA or empty character strings
  plate[is.na(plate) | plate == ""] <- NA_character_
  
  return(plate)
}