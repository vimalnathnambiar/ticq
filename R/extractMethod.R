#' extractMethod
#'
#' Extract method name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param input ANPC sample file name or path: character
#' @returns Method name
#'
#' @examples
#' method <- ticq::extractMethod(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(method)
extractMethod <- function(input) {
  # Method name patterns
  method <- dplyr::case_when(
    grepl("MS-AA-POS|MS-AApos|MS-AA|MS_AA|AAA|AA", input) ~ "MS-AA-POS",
    grepl("MS-AI-HILPOS|MS-HIL-POS|MS-HILpos", input) ~ "MS-HIL-POS",
    grepl("MS-AI-HILNEG|MS-HIL-NEG|MS-HILneg", input) ~ "MS-HIL-NEG",
    grepl("MS-AI-RPPOS|MS-RP-POS|MS-RPpos", input) ~ "MS-RP-POS",
    grepl("MS-AI-RPNEG|MS-RP-NEGMS-RPneg", input) ~ "MS-RP-NEG",
    TRUE ~ NA_character_
  )
  
  return(method)
}