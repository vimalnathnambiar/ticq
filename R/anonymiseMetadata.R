#' Anonymise Metadata
#'
#' Anonymise metadata associated with Australian National Phenome Centre (ANPC)-specific data files including:
#' - Sample ID
#' - Project name
#' - Cohort name
#' - Method name
#' - Instrument name
#' - Plate number
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param data A data frame of the MS spectral data.
#' @param sampleID A character string representing the name of the sample ID column.
#' @param project A character string representing the name of the project column.
#' @param cohort A character string representing the name of the cohort column.
#' @param projectCohort A character string representing the name of the combined project and cohort column.
#' @param method A character string representing the name of the method column.
#' @param instrument A character string representing the name of the instrument column.
#' @param plate A character string representing the name of the plate column.
#' @returns A data frame of the MS spectral data and its associated anonymised metadata in respective data columns.
#'
#' @examples
#' data <- data.frame(sampleID = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29", project = "COVID-19", cohort = "Heidelberg",
#'                    projectCohort = "COVID-19 Heidelberg", method = "MS-AA-POS", instrument = "PAI05", plate = "88")
#' anonymiseMetadata(data = data, sampleID = "sampleID", project = "project", cohort = "cohort", projectCohort = "projectCohort",
#'                   method = "method", instrument = "instrument", plate = "plate")
anonymiseMetadata <- function(data, sampleID, project, cohort, projectCohort, method, instrument, plate) {
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("Invalid 'data': Must be a data frame")
  }
  
  parameter <- list(
    sampleID = sampleID,
    project = project,
    cohort = cohort,
    projectCohort = projectCohort,
    method = method,
    instrument = instrument,
    plate = plate
  )
  for (i in names(parameter)) {
    validateCharacterStringValue(name = i, value = parameter[[i]])
  }
  
  parameter <- c(sampleID, project, cohort, projectCohort, method, instrument, plate)
  if (!all(parameter %in% colnames(data))) {
    stop(paste0("Unable to anonymise metadata: Missing one or more data column (", paste(parameter[!parameter %in% colnames(data)], collapse = ", "), ")"))
  }
  
  # Anonymise metadata
  return(
    data %>%
      dplyr::mutate(
        !!sampleID := paste0("sample", match(.data[[sampleID]], unique(.data[[sampleID]]))),
        !!project := dplyr::case_when(
          is.na(.data[[project]]) ~ "P0",
          .data[[project]] == "COVID-19" ~ "P1",
          .data[[project]] == "Barwon" ~ "P2",
          .data[[project]] == "Coeliac" ~ "P3",
          .data[[project]] == "COVVAC" ~ "P4",
          .data[[project]] == "CABIN" ~ "P5",
          .data[[project]] == "Mouse Flu" ~ "P6",
          .data[[project]] == "Prolifica" ~ "P7",
          .data[[project]] == "Omniheart" ~ "P8",
          .data[[project]] == "ROCIT" ~ "P9",
          .data[[project]] == "Parkinsons" ~ "P10",
          .data[[project]] == "Quality" ~ "P11",
          .data[[project]] == "Gemma" ~ "P12",
          .data[[project]] == "Biolog" ~ "P13",
          .data[[project]] == "Myositis" ~ "P14",
          .data[[project]] == "Busselton" ~ "P15",
          .data[[project]] == "Microbiome" ~ "P16",
          .data[[project]] == "IPAC" ~ "P17",
          .data[[project]] == "HIMS" ~ "P18",
          .data[[project]] == "CKI" ~ "P19",
          .data[[project]] == "MUMS" ~ "P20",
          .data[[project]] == "Pediatric Burns" ~ "P21",
          .data[[project]] == "Penang" ~ "P22",
          .data[[project]] == "Fruits" ~ "P23",
          .data[[project]] == "Systic Fibrosis" ~ "P24",
          .data[[project]] == "Diabetes" ~ "P25",
          .data[[project]] == "STREP" ~ "P26",
          .data[[project]] == "Normal Control Bank" ~ "P27",
          .data[[project]] == "Comet Toxicology Project" ~ "P28",
          .data[[project]] == "Human Performance Project" ~ "P29",
          .data[[project]] == "Bioreactor" ~ "P30",
          .data[[project]] == "Intersalt" ~ "P31",
          .data[[project]] == "Student Projects" ~ "P32",
        ),
        !!cohort := dplyr::case_when(
          is.na(.data[[cohort]]) ~ "C1",
          .data[[cohort]] == "System Suitability" ~ "C9",
          .data[[cohort]] == "Cambridge Follow Up Part 2" ~ "C10",
          .data[[cohort]] == "Cambridge Follow Up" ~ "C11",
          .data[[cohort]] == "Cambridge" ~ "C12",
          .data[[cohort]] == "Biogune C2" ~ "C13",
          .data[[cohort]] == "Biogune External 2" ~ "C14",
          .data[[cohort]] == "Biogune External" ~ "C15",
          .data[[cohort]] == "Biogune" ~ "C16",
          .data[[cohort]] == "Harvard C2" ~ "C17",
          .data[[cohort]] == "Harvard" ~ "C18",
          .data[[cohort]] == "Mauritius" ~ "C19",
          .data[[cohort]] == "Healthy Bank Follow Up" ~ "C20",
          .data[[cohort]] == "Healthy Bank" ~ "C21",
          .data[[cohort]] == "WA Follow Up" ~ "C22",
          .data[[cohort]] == "Heidelberg" ~ "C23",
          .data[[cohort]] == "Pathwest Follow Up" ~ "C24",
          .data[[cohort]] == "Pathwest" ~ "C25",
          .data[[cohort]] == "Mass Gen" ~ "C26",
          .data[[cohort]] == "Malaysia" ~ "C27",
          .data[[cohort]] == "Joondalup" ~ "C28",
          .data[[cohort]] == "Bio Mood" ~ "C29",
          .data[[cohort]] == "Eggs" ~ "C30",
          .data[[cohort]] == "Fat Biopsy" ~ "C31",
          .data[[cohort]] == "Gut Flora" ~ "C32",
          .data[[cohort]] == "Jess" ~ "C33",
          .data[[cohort]] == "Maddison" ~ "C34",
          .data[[cohort]] == "Anderton" ~ "C35",
          .data[[cohort]] == "Goac Crypto" ~ "C36",
          .data[[cohort]] == "Jayden 2" ~ "C37",
          .data[[cohort]] == "Jayden" ~ "C38",
          .data[[cohort]] == "Maria" ~ "C39",
          .data[[cohort]] == "Exercise" ~ "C40",
          .data[[cohort]] == "Saudi Arabia Diabetes" ~ "C41",
          .data[[cohort]] == "Fremantle" ~ "C42",
          .data[[cohort]] == "CVD Umar" ~ "C43",
          .data[[cohort]] == "Swimmers Metabolome" ~ "C44",
          .data[[cohort]] == "Preserve" ~ "C45",
          .data[[cohort]] == "IPF" ~ "C46",
          .data[[cohort]] == "Colchicin Chile" ~ "C47",
          .data[[cohort]] == "C1" ~ "C1",
          .data[[cohort]] == "C2" ~ "C2",
          .data[[cohort]] == "C3" ~ "C3",
          .data[[cohort]] == "C4" ~ "C4",
          .data[[cohort]] == "C5" ~ "C5",
          .data[[cohort]] == "C6" ~ "C6",
          .data[[cohort]] == "C7" ~ "C7",
          .data[[cohort]] == "C8" ~ "C8"
        ),
        !!projectCohort := paste0(.data[[project]], .data[[cohort]]),
        !!method := ifelse(is.na(.data[[method]]), NA_character_, .data[[method]]),
        !!instrument := dplyr::case_when(
          is.na(.data[[instrument]]) ~ "I0",
          .data[[instrument]] == "PAI01" ~ "I1",
          .data[[instrument]] == "PAI02" ~ "I2",
          .data[[instrument]] == "PAI03" ~ "I3",
          .data[[instrument]] == "PAI04" ~ "I4",
          .data[[instrument]] == "PAI05" ~ "I5",
          .data[[instrument]] == "PAI06" ~ "I6",
          .data[[instrument]] == "PAI07" ~ "I7",
          .data[[instrument]] == "PAI08" ~ "I8",
          .data[[instrument]] == "RAI01" ~ "I9",
          .data[[instrument]] == "RAI02" ~ "I10",
          .data[[instrument]] == "RAI03" ~ "I11",
          .data[[instrument]] == "RAI04" ~ "I12",
          .data[[instrument]] == "PAT01" ~ "I13",
          .data[[instrument]] == "PAT02" ~ "I14",
          .data[[instrument]] == "TIMS01" ~ "I15",
          .data[[instrument]] == "TIMS02" ~ "I16",
          .data[[instrument]] == "MRMS01" ~ "I17",
          .data[[instrument]] == "EVOQ01" ~ "I18",
          .data[[instrument]] == "REIMS01" ~ "I19",
          .data[[instrument]] == "DESI01" ~ "I20",
          .data[[instrument]] == "RAT01" ~ "I21",
          .data[[instrument]] == "RAT02" ~ "I22",
          .data[[instrument]] == "PLIP01" ~ "I23",
          .data[[instrument]] == "RLIP02" ~ "I24"
        ),
        !!plate := ifelse(is.na(.data[[plate]]), "0", .data[[plate]])
      )
  )
}