#' Render TICQ
#' 
#' Generate a HTML report of TICQ assessment performed on MS spectral data acquired from JSON files generated using \href{https://github.com/vmalnathnambiar/exfilms}{ExfilMS}.
#' 
#' 1. Metadata extraction and anonymisation only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#' 
#' 2. To perform a targeted assessment of extracted ions (targeted m/z values) with TICQ, spectral data extracted using ExfilMS should have undergone targeted spectra filtering during the extraction process using a target file or an in-house ANPC method library.
#' The target file or ANPC method library used should then also be specified in TICQ for a comprehensive assessment.
#' 
#' 3. To perform comparison with a historical spectral data, the mentioned data should have also undergo the same extraction process as the current spectral data being assessed.
#' 
#' @import httr
#' @import rmarkdown
#' 
#' @export
#' @param inputPath A character string representing an existing directory or file path.
#' @param outputDirectoryPath A character string representing a directory path to store output files (HTML report and RDS formatted spectral data). (Default: "`r paste0(getwd(), "/output/")`")
#' @param historicalDataPath A character string representing a RDS file containing historical spectral data associated to the current spectral data being assessed. (Default: `NULL`; Options: `"/path/to/local/file.rds"`)
#' @param targetFilePath A character string representing an existing TSV target file path. (Default: `NULL`; Options: `"https://published-to-web-URL&output=tsv"` or `"/path/to/local/file.tsv"`)
#' @param anpcMethodLibrary A character string representing an ANPC method library. (Default: `NULL`; Options: `"MS-AA-POS"`, `"MS-HIL-POS"`, `"MS-HIL-NEG"`, `"MS-RP-POS"`, or `"MS-RP-NEG"`)
#' @param chromatogramRegion A list of lists representing the different chromatogram regions of interest and their respective start and end time points. (Default: `NULL`; Options: Use `ticq::configureChromatogramRegion()`)
#' @param metadataAnonymiser A logical value representing the metadata anonymiser status to anonymising metadata acquired from sample data. (Default: `FALSE`; Options: `TRUE` or `FALSE`)
#' @returns A character string representing the rendered HTML output file path.
renderTICQ <- function(inputPath, outputDirectoryPath = paste0(getwd(), "/output/"), historicalDataPath = NULL, targetFilePath = NULL, anpcMethodLibrary = NULL, chromatogramRegion = NULL, roundDecimalPlace = NULL, metadataAnonymiser = FALSE) {
  # ANPC method library associated chromatogram region data
  anpcChromatogramRegionData <- list(
    "MS-AA-POS" = list(massCalibrationEnd = 0.3, washStart = 5),
    "MS-HIL-POS" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-HIL-NEG" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-RP-POS" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-RP-NEG" = list(massCalibrationEnd = 0.7, washStart = 11)
  )
  
  # Validate input path
  message("\nValidating rendering parameters")
  message("---")
  message("1. Input path")
  if (length(retrieveFileName(inputPath = inputPath, fileExtension = "JSON")) == 0) {
    stop("Invalid 'inputPath': No available JSON file was found at the specified path")
  }
  inputMetadata <- extractMetadata(input = inputPath)
  print(inputPath)
  
  # Validate output path
  message("\n2. Output directory path")
  if (length(outputDirectoryPath) == 0) {
    message(paste0("Setting default path"))
    outputDirectoryPath <- paste0(getwd(), "/output/")
  }
  createDirectoryPath(parameterName = "outputDirectoryPath", parameterValue = outputDirectoryPath)
  print(outputDirectoryPath)
  
  # Validate historical data path
  message("\n3. Historical data path")
  if (!is.null(historicalDataPath)) {
    if (length(parameterValue) != 1 || !is.character(parameterValue) || is.na(parameterValue) || parameterValue == "") {
      stop("Invalid 'historicalDataPath: Must either be NULL or a non-NA, non-empty character string of length 1")
    } else if (!checkFileExtension(parameterName = 'historicalDataPath', parameterValue = historicalDataPath, fileExtension = "RDS")) {
      stop("Invalid 'historicalDataPath': No available RDS file found at the specified path")
    }
  }
  print(historicalDataPath)
  
  # Validate target file path
  message("\n4. Target file path")
  if (!is.null(targetFilePath) && is.null(extractTargetFile(targetFilePath = targetFilePath))) {
    stop("Invalid 'targetFilePath': No available target file data")
  }
  print(targetFilePath)
    
  # Validate ANPC method library
  message("\n5. ANPC method library")
  if (is.null(anpcMethodLibrary)) {
    if (inputMetadata$method %in% names(anpcChromatogramRegionData)) {
      message("Attempting to set method library based on input metadata")
      if (!is.null(extractTargetFile(anpcMethodLibrary = inputMetadata$method))) {
        message("Setting new value")
        anpcMethodLibrary <- inputMetadata$method
      } else {
        message("Unsuccessful attempt: No available target file data")
      }
    }
  } else if (!is.null(anpcMethodLibrary) && is.null(extractTargetFile(anpcMethodLibrary = anpcMethodLibrary))) {
    stop("Invalid 'anpcMethodLibrary': No available target file data")
  }
  print(anpcMethodLibrary)
    
  # Validate chromatogram region data
  message("\n6. Chromatogram region")
  if (!is.null(chromatogramRegion)) {
    if (!validateChromatogramRegion(parameterName = "chromatogramRegion", parameterValue = chromatogramRegion)) {
      message("Setting default value")
      chromatogramRegion <- NULL
    }
  }
  
  if (is.null(chromatogramRegion) && !is.null(anpcMethodLibrary) && anpcMethodLibrary %in% names(anpcChromatogramRegionData)) {
    message(paste("Setting new region data (based on ANPC", anpcMethodLibrary, "method library)"))
    chromatogramRegion <- configureChromatogramRegion(
      massCalibrationEnd = anpcChromatogramRegionData[[anpcMethodLibrary]]$massCalibrationEnd,
      washStart = anpcChromatogramRegionData[[anpcMethodLibrary]]$washStart
    )
  }
  print(chromatogramRegion)
  
  # Validate round decimal place value
  message("\n7. Round decimal place")
  if (!is.null(roundDecimalPlace) && (length(roundDecimalPlace) != 1 || !is.numeric(roundDecimalPlace))) {
    stop(paste0("Invalid 'roundDecimalPlace': Must either be NULL or a numeric value of length 1"))
  }
  print(roundDecimalPlace)
  
  # Validate metadata anonymiser status
  message("\n8. Metadata anonymiser")
  validateLogicalValue(parameterName = "metadataAnonymiser", parameterValue = metadataAnonymiser)
  print(metadataAnonymiser)
  message("---\n")

  # Render TICQ R markdown script
  message("Rendering TICQ...")
  title <- gsub("\\.json$", "", basename(inputPath), ignore.case = TRUE)
  rmarkdown::render(
    input = system.file("notebooks", "ticq.Rmd", package = "ticq"),
    output_format = "html_document",
    output_dir = outputDirectoryPath,
    output_file = paste0(title, ".html"),
    params = list(
      title = title,
      inputPath = inputPath,
      outputDirectoryPath = outputDirectoryPath,
      historicalDataPath = historicalDataPath,
      targetFilePath = targetFilePath,
      anpcMethodLibrary = anpcMethodLibrary,
      chromatogramRegion = chromatogramRegion,
      roundDecimalPlace = roundDecimalPlace,
      metadataAnonymiser = metadataAnonymiser
    )
  )
  message("Rendering complete!")
}