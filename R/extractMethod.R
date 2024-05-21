#' extractMethod
#'
#' Extract method name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param input Input to extract method name from: character
#' @returns Method name
#'
#' @examples
#' method <- ticq::extractMethod(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
extractMethod <- function(input) {
  method <- dplyr::case_when(
    grepl("MS-AA-POS|MS-AApos|MS-AA|MS_AA|AAA|AA", input) ~ "MS-AA-POS",
    grepl("MS-HIL-POS|MS-HILpos", input) ~ "MS-HIL-POS",
    grepl("MS-HIL-NEG|MS-HILneg", input) ~ "MS-HIL-NEG",
    grepl("MS-RP-POS|MS-RPpos", input) ~ "MS-RP-POS",
    grepl("MS-RP-NEGMS-RPneg", input) ~ "MS-RP-NEG",
    TRUE ~ NA_character_
  )
  
  return(method)
}