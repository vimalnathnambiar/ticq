#' Extract Metadata
#'
#' Extract metadata from Australian National Phenome Centre (ANPC)-specific data files including:
#' - Project name
#' - Cohort name
#' - Matrix type
#' - Sample type
#' - Method name
#' - Instrument name
#' - Plate number
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @export
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @returns A list of character vectors of the associated metadata.
#'
#' @examples
#' extractMetadata(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
extractMetadata <- function(input) {
  # Validate parameters
  validateCharacterVector(name = "input", value = input)
  
  # Extract metadata
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