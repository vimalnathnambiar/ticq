#' Extract Matrix
#'
#' Extract matrix type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC sample file names or paths.
#' @returns A character vector of matrix types.
extractMatrix <- function(input) {
  return(
    dplyr::case_when(
      grepl("PLASMA|PLA", input) ~ "Plasma",
      grepl("SERUM|SER", input) ~ "Serum",
      grepl("URINE|URI", input) ~ "Urine",
      TRUE ~ NA_character_
    )
  )
}