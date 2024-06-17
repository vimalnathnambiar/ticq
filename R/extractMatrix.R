#' Extract Matrix
#'
#' Extract matrix type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param input ANPC sample file name or path: character or character vector
#' @returns Matrix type
#'
#' @examples
#' matrix <- ticq::extractMatrix(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(matrix)
extractMatrix <- function(input) {
  # Validate parameters
  if (is.null(input) || !is.character(input)) {
    message("Invalid 'input': Must not be a non-NULL character string or vector (Setting default to empty character string)")
    input <- ""
  }
  
  # Extract matrix type
  return(
    dplyr::case_when(
      grepl("PLASMA|PLA", input) ~ "Plasma",
      grepl("SERUM|SER", input) ~ "Serum",
      grepl("URINE|URI", input) ~ "Urine",
      TRUE ~ NA_character_
    )
  )
}