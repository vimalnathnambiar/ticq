#' Extract ExfilMS
#'
#' Extract spectral data and retrieve metadata from JSON files generated using \href{https://github.com/vmalnathnambiar/exfilms#readme}{ExfilMS}.
#'
#' Metadata extraction only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import jsonlite
#'
#' @export
#' @param inputPath A character string representing an existing directory or file path.
#' @returns A list containing two data frames (`passedData` containing the extracted spectral data and `failedData` with file names that failed the extraction process).
extractExfilMS <- function(inputPath) {
  # Validate parameters
  inputFiles <- retrieveFileName(inputPath = inputPath, fileExtension = "JSON")
  
  # Extract spectral data from JSON files
  passedData <- data.frame()
  fileName <- character(0)
  if (length(inputFiles) > 0) {
    for (i in inputFiles) {
      # Parse JSON file
      filePath <- if (file.info(inputPath)$isdir) paste0(inputPath, i) else inputPath
      sample <- tryCatch(
        jsonlite::fromJSON(filePath),
        warning = function(w) {
          message(paste0("Unable to extract ExfilMS: ", i, " (", w, ")"))
          NULL
        },
        error = function(e) {
          message(paste0("Unable to extract ExfilMS: ", i, " (", e, ")"))
          NULL
        }
      )
      
      # Extract metadata and spectral data
      if (!is.null(sample)) {
        tryCatch({
          sampleInfo <- extractMetadata(input = filePath)
          passedData <- rbind(
            passedData,
            data.frame(
              sampleID = sample$id,
              project = sampleInfo$project,
              cohort = sampleInfo$cohort,
              projectCohort = sampleInfo$projectCohort,
              matrix = sampleInfo$matrix,
              sampleType = sampleInfo$sampleType,
              method = sampleInfo$method,
              instrument = sampleInfo$instrument,
              plate = sampleInfo$plate,
              date = sample$date,
              time = sample$time,
              timestamp = as.POSIXct(paste(sample$date, sample$time)),
              spectrumCount = sample$spectrumCount,
              spectrum = sample$spectrum
            )
          )
        }, warning = function(w) {
          message(paste0("Unable to extract ExfilMS: ", i, " (", w, ")"))
          sample <- NULL
        }, error = function(e) {
          message(paste0("Unable to extract ExfilMS: ", i, " (", e, ")"))
          sample <- NULL
        })
      }
      
      # Record file names of failed extraction
      if (is.null(sample)) {
        fileName <- c(fileName, i)
      }
    }
  } else {
    message("Unable to extract ExfilMS: No available JSON file was found at the specified path")
  }
  
  return(list(passedData = passedData, failedData = as.data.frame(fileName)))
}