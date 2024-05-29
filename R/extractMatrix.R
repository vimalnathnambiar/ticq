#' extractMatrix
#'
#' Extract matrix type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param input Input to extract matrix type: character
#' @returns Matrix type
#'
#' @examples
#' matrix <- ticq::extractMatrix(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(matrix)
extractMatrix <- function(input) {
  matrix <- dplyr::case_when(
    grepl("PLASMA|PLA", input) ~ "Plasma",
    grepl("SERUM|SER", input) ~ "Serum",
    grepl("URINE|URI", input) ~ "Urine",
    TRUE ~ NA_character_
  )
  
  return(matrix)
}