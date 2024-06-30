#' Extract Instrument
#'
#' Extract instrument name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @returns A character vector of the associated instrument names.
extractInstrument <- function(input) {
  return(
    dplyr::case_when(
      grepl("PAI01|PAI-01", input) ~ "PAI01",
      grepl("PAI02|PAI-02", input) ~ "PAI02",
      grepl("PAI03|PAI-03", input) ~ "PAI03",
      grepl("PAI04|PAI-04", input) ~ "PAI04",
      grepl("PAI05|PAI-05", input) ~ "PAI05",
      grepl("PAI06|PAI-06", input) ~ "PAI06",
      grepl("PAI07|PAI-07", input) ~ "PAI07",
      grepl("PAI08|PAI-08", input) ~ "PAI08",
      grepl("RAI01|RAI-01", input) ~ "RAI01",
      grepl("RAI02|RAI-02", input) ~ "RAI02",
      grepl("RAI03|RAI-03", input) ~ "RAI03",
      grepl("RAI04|RAI-04", input) ~ "RAI04",
      grepl("PAT01|PAT-01", input) ~ "PAT01",
      grepl("PAT02|PAT-02", input) ~ "PAT02",
      grepl("TIMS01|TIMS-01", input) ~ "TIMS01",
      grepl("TIMS01|TIMS-02", input) ~ "TIMS02",
      grepl("MRMS01|MRMS-02", input) ~ "MRMS01",
      grepl("EVOQ01|EVOQ-01", input) ~ "EVOQ01",
      grepl("REIMS01|REIMS-01", input) ~ "REIMS01",
      grepl("DESI01|DESI-01", input) ~ "DESI01",
      grepl("RAT01|RAT-01", input) ~ "RAT01",
      grepl("RAT02|RAT-02", input) ~ "RAT02",
      grepl("PLIP01|PLIP-01", input) ~ "PLIP01",
      grepl("RLIP01|RLIP-01", input) ~ "RLIP01",
      TRUE ~ NA_character_
    )
  )
}