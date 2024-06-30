#' Extract Sample Type
#'
#' Extract sample type.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @param matrix A character vector representing associated matrix types.
#' @returns A character vector of the associated sample types.
extractSampleType <- function(input, matrix) {
  return(
    dplyr::case_when(
      grepl("DLTR|dLTR|dltr", input) ~ ifelse(is.na(matrix), NA_character_, paste("dLTR", matrix)),
      grepl("SLTR|sLTR|sltr", input) ~ ifelse(is.na(matrix), NA_character_, paste("sLTR", matrix)),
      grepl("LTR|ltr", input) ~ ifelse(is.na(matrix), NA_character_, paste("LTR", matrix)),
      grepl("DPQC|dPQC|dpqc", input) ~ ifelse(is.na(matrix), NA_character_, paste("dPQC", matrix)),
      grepl("PQC|pqc", input) ~ ifelse(is.na(matrix), NA_character_, paste("PQC", matrix)),
      grepl("DSAM|dSAM|dsam", input) ~ ifelse(is.na(matrix), NA_character_, paste("dSample", matrix)),
      grepl("QC01|QC 1|QC1", input) ~ "QC01",
      grepl("QC02|QC 2|QC2", input) ~ "QC02",
      grepl("QC03|QC 3|QC3", input) ~ "QC03",
      grepl("QC04|QC 4|QC4", input) ~ "QC04",
      grepl("CAL01|Cal 1|Cal1", input) ~ "CAL01",
      grepl("CAL02|Cal 2|Cal2", input) ~ "CAL02",
      grepl("CAL03|Cal 3|Cal3", input) ~ "CAL03",
      grepl("CAL04|Cal 4|Cal4", input) ~ "CAL04",
      grepl("CAL05|Cal 5|Cal5", input) ~ "CAL05",
      grepl("CAL06|Cal 6|Cal6", input) ~ "CAL06",
      grepl("CAL07|Cal 7|Cal7", input) ~ "CAL07",
      grepl("CAL08|Cal 8|Cal8", input) ~ "CAL08",
      grepl("Blank_Single|Single Blank|SingleBlank", input) ~ "Single Blank",
      grepl("Blank_Double|Double Blank|DoubleBlank", input) ~ "Double Blank",
      grepl("QC|CAL|Cal|Blank|BLK", input) ~ NA_character_,
      TRUE ~ ifelse(is.na(matrix), NA_character_, paste("Sample", matrix))
    )
  )
}