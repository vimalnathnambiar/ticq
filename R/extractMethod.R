#' Extract Method
#'
#' Extract method name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @returns A character vector of the associated method names.
extractMethod <- function(input) {
  return(
    dplyr::case_when(
      grepl("MS-AA-POS|MS-AApos|MS-AA|MS_AA|AAA|AA", input) ~ "MS-AA-POS",
      grepl("MS-AI-HILPOS|MS-HIL-POS|MS-HILpos", input) ~ "MS-HIL-POS",
      grepl("MS-AI-HILNEG|MS-HIL-NEG|MS-HILneg", input) ~ "MS-HIL-NEG",
      grepl("MS-AI-RPPOS|MS-RP-POS|MS-RPpos", input) ~ "MS-RP-POS",
      grepl("MS-AI-RPNEG|MS-RP-NEGMS-RPneg", input) ~ "MS-RP-NEG",
      TRUE ~ NA_character_
    )
  )
}