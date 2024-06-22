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
#' @param inputPath A character string representing an existing directory or file path. Should not be NA or an empty character string.
#' @param outputDirectoryPath A character string representing a directory path to store output files (HTML report and RDS formatted spectral data). (Default: "`r paste0(getwd(), "/output/")`").
#' @param historicalDataPath A character string representing a RDS file containing historical spectral data associated to the current spectral data being assessed. (Default: `NULL`; Options: `"/path/to/local/file.rds"`)
#' @param targetFilePath A character string representing an existing TSV target file path. (Default: `NULL`; Options: `"https://published-to-web-URL&output=tsv"` or `"/path/to/local/file.tsv"`)
#' @param anpcMethodLibrary ANPC method library (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param chromatogramRegion A list containing the start and end time points of the different chromatogram regions of interest (Default: NULL, Options: ticq::configureChromatogramRegion()): NULL or list
#' @param roundDecimalPlace Number of decimal places for precision value rounding (Default: NULL): NULL or numeric
#' @param metadataAnonymiser Toggle to anonymise metadata (Default: FALSE, Options: TRUE or FALSE): logical
#' @returns Path to the rendered output file.
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
  if (length(ticq::retrieveFileList(inputPath = inputPath, fileExtension = "JSON")) == 0) {
    stop("Invalid 'inputPath': No available JSON file(s) was found at the specified path.")
  }
  inputMetadata <- ticq::extractMetadata(input = inputPath)
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
    if (!checkFileExtension(parameterName = "historicalDataPath", parameterValue = historicalDataPath, fileExtension = "RDS")) {
      stop("Invalid 'historicalDataPath': No available RDS file found at the specified path.")
    }
  }
  print(historicalDataPath)
  
  # Validate target file path
  message("\n4. Target file path")
  if (!is.null(targetFilePath) && is.null(ticq::extractTargetFile(targetFilePath = targetFilePath))) {
    stop("Invalid 'targetFilePath': No available target file data")
  }
  print(targetFilePath)
    
  # Validate ANPC method library
  message("\n5. ANPC method library")
  if (is.null(anpcMethodLibrary)) {
    if (inputMetadata$method %in% names(anpcChromatogramRegionData)) {
      message("Attempting to set method library based on input metadata")
      if (!is.null(ticq::extractTargetFile(anpcMethodLibrary = inputMetadata$method))) {
        message(paste0("Setting new value (", inputMetadata$metadata, ")"))
        anpcMethodLibrary <- inputMetadata$method
      } else {
        message("Unsuccessful attempt: No available target file data")
      }
    }
  } else if (!is.null(anpcMethodLibrary) && is.null(ticq::extractTargetFile(anpcMethodLibrary = anpcMethodLibrary))) {
    stop("Invalid 'anpcMethodLibrary': No available target file data")
  }
  print(anpcMethodLibrary)
    
  # Validate chromatogram region data
  message("\n6. Chromatogram region")
  if (!is.null(chromatogramRegion)) {
    if (!validateChromatogramRegion(chromatogramRegion = chromatogramRegion)) {
      message("Setting default value (NULL)")
      chromatogramRegion <- NULL
    }
  }
  
  if (is.null(chromatogramRegion) && !is.null(anpcMethodLibrary) && anpcMethodLibrary %in% names(anpcChromatogramRegionData)) {
    message(paste("Setting new region data (based on ANPC", anpcMethodLibrary, "method library)"))
    chromatogramRegion <- ticq::configureChromatogramRegion(
      massCalibrationEnd = anpcChromatogramRegionData[[anpcMethodLibrary]]$massCalibrationEnd,
      washStart = anpcChromatogramRegionData[[anpcMethodLibrary]]$washStart
    )
  }
  print(chromatogramRegion)
  
  # Validate round decimal place value
  message("\n7. Round decimal place")
  if (!is.null(roundDecimalPlace)) {
    validateNumericValue(parameterName = "roundDecimalPlace", parameterValue = roundDecimalPlace)
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
