#' Extract Method
#'
#' Extract method name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input ANPC sample file name or path: character
#' @returns Method name
extractMethod <- function(input) {
  # Method name patterns
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