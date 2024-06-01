#' Extract Spectrum
#'
#' Extract spectral data from JSON files generated using ExfilMS (https://github.com/vmalnathnambiar/exfilms). Also includes retrievable metadata from sample name or file path. 
#'
#' Metadata extraction only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import jsonlite
#'
#' @export
#' @param input Input path (Either a directory containing JSON files or a single file path): character
#' @param inputFiles JSON input files to be extracted: character vector
#' @returns A list containing two data frames (passed and failed data)
extractSpectrum <- function(input, inputFiles) {
  # Defaults
  passedData <- data.frame()
  failedFiles <- character(0)
  
  # If input is defined and there are input files for extraction
  if (!is.null(input) && length(inputFiles) > 0) {
    # Extract file information
    fileInfo <- file.info(input)
    
    # Loop through input files
    for (i in inputFiles) {
      # Check and determine file path
      if (fileInfo$isdir) {
        filePath <- paste0(input, i)
      } else {
        filePath <- input
      }
      
      # Extract spectral data
      sample <- tryCatch(jsonlite::fromJSON(filePath),
                         warning = function(w) NULL,
                         error = function(e) NULL)
      
      # Check if error / warning occurred
      if (!is.null(sample)) {
        # Extract metadata
        sampleInfo <- ticq::extractMetadata(input = filePath)
        
        # Append data to data frame
        passedData <- rbind(passedData,
                            data.frame(sampleID = sample$id,
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
                                       spectrum = sample$spectrum))
      } else {
        # Add filename to failed files
        failedFiles <- c(failedFiles, i)
      }
    }
  }
  
  return(list(passedData = passedData, failedData = as.data.frame(failedFiles)))
}