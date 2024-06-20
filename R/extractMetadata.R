#' Extract Metadata
#'
#' Extract sample metadata including project name, cohort name, matrix type, sample type, acquisition method, instrument name, and plate number.
#'
#' Applicable only to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @export
#' @param input ANPC sample file name or path: character
#' @returns List containing the input associated metadata
#'
#' @examples
#' metadata <- ticq::extractMetadata(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
#' print(metadata)
extractMetadata <- function(input) {
  # Metadata components
  project <- extractProject(input = input)
  cohort <- extractCohort(input = input)
  projectCohort <- ifelse(is.na(project), ifelse(is.na(cohort), NA_character_, cohort), ifelse(is.na(cohort), project, paste(project, cohort)))
  matrix <- extractMatrix(input = input)
  sampleType <- extractSampleType(input = input, matrix = matrix)
  method <- extractMethod(input = input)
  instrument <- extractInstrument(input = input)
  plate <- extractPlate(input = input)
  
  return(
    list(
      project = project,
      cohort = cohort,
      projectCohort = projectCohort,
      matrix = matrix,
      sampleType = sampleType,
      method = method,
      instrument = instrument,
      plate = plate
    )
  )
}