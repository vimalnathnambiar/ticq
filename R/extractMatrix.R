#' Extract Matrix
#'
#' Extract matrix type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input ANPC sample file name or path: character
#' @returns Matrix type
extractMatrix <- function(input) {
  # Matrix type patterns
  return(
    dplyr::case_when(
      grepl("PLASMA|PLA", input) ~ "Plasma",
      grepl("SERUM|SER", input) ~ "Serum",
      grepl("URINE|URI", input) ~ "Urine",
      TRUE ~ NA_character_
    )
  )
}