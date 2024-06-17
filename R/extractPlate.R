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
#' @param input ANPC sample file name or path: character or character vector
#' @returns Plate number
#'
#' @examples
#' plate <- ticq::extractPlate(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(plate)
extractPlate <- function(input) {
  # Check plate number patterns
  pattern <- c("[pP][0-9]+", "PLATE[0-9]+", "PLASMA[0-9]+")
  
  # Validate parameters
  if (is.null(input) || !is.character(input)) {
    message("Invalid 'input': Must not be a non-NULL character string or vector (Setting default to empty character string)")
    input <- ""
  }
  
  # Extract plate number
  pattern <- paste(pattern, collapse = "|")
  plate <- stringr::str_extract(input, pattern) %>%
    stringr::str_replace_all("^[a-zA-Z0]+", "")
  
  plate[is.na(plate) | plate == ""] <-  "0"
  
  return(plate)
}