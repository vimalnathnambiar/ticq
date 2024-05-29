#' extractMetadata
#'
#' Extract metadata. This includes project name, cohort name, matrix type, sample type, acquisition method, instrument name, and plate number.
#'
#' Applicable only to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @export
#' @param input Input to extract metadata: character
#' @returns List containing the metadata associated to the data
#'
#' @examples
#' metadata <- ticq::extractMetadata(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
extractMetadata <- function(input) {
  # Extract project
  project <- ticq::extractProject(input = input)
  
  # Extract cohort
  cohort <- ticq::extractCohort(input = input)
  
  # Combine project and cohort into a single variable
  projectCohort <- ifelse(is.na(project),
                          ifelse(is.na(cohort), NA_character_, cohort),
                          ifelse(is.na(cohort), project, paste(project, cohort)))
  
  # Extract matrix type
  matrix <- ticq::extractMatrix(input = input)
  
  # Extract sample type
  sampleType <- ticq::extractSampleType(input = input, matrix = matrix)
  
  # Extract method
  method <- ticq::extractMethod(input = input)
  
  # Extract instrument
  instrument <- ticq::extractInstrument(input = input)
  
  # Extract plate number
  plate <- ticq::extractPlate(input = input)
  
  return(list(project = project,
              cohort = cohort,
              projectCohort = projectCohort,
              matrix = matrix,
              sampleType = sampleType,
              method = method,
              instrument = instrument,
              plate = plate))
}