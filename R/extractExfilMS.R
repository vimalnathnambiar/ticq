#' Extract ExfilMS
#'
#' Extract MS spectral data along with its associated metadata from \href{https://github.com/vmalnathnambiar/exfilms#readme}{ExfilMS} extracted JSON files.
#'
#' Metadata extraction only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import jsonlite
#'
#' @export
#' @param inputPath A character string representing a directory or file path containing ExfilMS extracted JSON files.
#' @returns A list with two data frames (data that passed and failed the extraction process).
extractExfilMS <- function(inputPath) {
  # Validate parameters
  inputFiles <- retrieveFileName(inputPath = inputPath, fileExtension = "JSON")
  
  # Extract spectral and metadata
  passedData <- data.frame()
  fileName <- character(0)
  
  if (length(inputFiles) > 0) {
    for (i in inputFiles) {
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
      
      if (is.null(sample)) {
        fileName <- c(fileName, i)
      }
    }
  } else {
    message("Unable to extract ExfilMS: No available JSON file was found at the specified path")
  }
  
  return(list(passedData = passedData, failedData = as.data.frame(fileName)))
}