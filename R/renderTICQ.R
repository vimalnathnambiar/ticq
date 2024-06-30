#' Render TICQ
#' 
#' Render a HTML report of the TICQ assessment performed on MS spectral data retrieved from 
#' \href{https://github.com/vmalnathnambiar/exfilms}{ExfilMS} extracted JSON files.
#' 
#' 1. Metadata extraction and anonymisation only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#' 
#' 2. To perform a targeted assessment of extracted ions (targeted m/z) with TICQ, spectral data extracted using ExfilMS must have undergone 
#' targeted spectra filtering during the extraction process using a target file or an in-house ANPC method library. The same target file or 
#' ANPC method library should then be specified in TICQ for a comprehensive assessment.
#' 
#' 3. To perform comparison with a historical spectral data, the data should have also undergone the same extraction process as
#' the current spectral data being assessed.
#' 
#' @import httr
#' @import rmarkdown
#' 
#' @export
#' @param inputPath A character string representing a directory or file path.
#' @param outputDirectoryPath A character string representing an output directory path. (Default: "`r paste0(getwd(), "/output/")`")
#' @param historicalDataPath A character string representing a RDS file containing historical MS spectral data for comparison. 
#' (Default: `NULL`; Options: `"/path/to/local/file.rds"`)
#' @param targetFilePath A character string representing a TSV target file path.
#' (Default: `NULL`; Options: `"https://published-to-web-URL&output=tsv"` or `"/path/to/local/file.tsv"`)
#' @param anpcMethodLibrary A character string representing an ANPC method library.
#' (Default: `NULL`; Options: `"MS-AA-POS"`, `"MS-HIL-POS"`, `"MS-HIL-NEG"`, `"MS-RP-POS"`, or `"MS-RP-NEG"`)
#' @param chromatogramRegion A list representing the chromatogram region data of interests. (Default: `NULL`; Options: `configureChromatogramRegion()`)
#' @param roundDecimalPlace A numeric value representing the number of decimal places to be used for precision value rounding. (Default: `NULL`)
#' @param metadataAnonymiser A logical value representing the status of the metadata anonymiser to anonymise acquired metadata from ANPC-specific data files.
#' (Default: `FALSE`; Options: `TRUE` or `FALSE`)
#' @returns A character string of the rendered HTML TICQ assessment report file path.
renderTICQ <- function(inputPath,
                       outputDirectoryPath = paste0(getwd(), "/output/"),
                       historicalDataPath = NULL,
                       targetFilePath = NULL,
                       anpcMethodLibrary = NULL,
                       chromatogramRegion = NULL,
                       roundDecimalPlace = NULL,
                       metadataAnonymiser = FALSE) {
  # Associated ANPC method library chromatogram region data
  anpcChromatogramRegionData <- list(
    "MS-AA-POS" = list(massCalibrationEnd = 0.3, washStart = 5),
    "MS-HIL-POS" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-HIL-NEG" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-RP-POS" = list(massCalibrationEnd = 0.7, washStart = 11),
    "MS-RP-NEG" = list(massCalibrationEnd = 0.7, washStart = 11)
  )
  
  message("\nValidating rendering parameters")
  message("---")
  
  # Validate input path
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
  createDirectoryPath(name = "outputDirectoryPath", value = outputDirectoryPath)
  print(outputDirectoryPath)
  
  # Validate historical data path
  message("\n3. Historical data path")
  if (!is.null(historicalDataPath) && 
      validateNullableCharacterStringValue(name = "historicalDataPath", value = historicalDataPath) &&
      !validateFileExtensionStatus(name = 'historicalDataPath', value = historicalDataPath, fileExtension = "RDS")) {
    stop("Invalid 'historicalDataPath': No available RDS file found at the specified path")
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
  if (is.null(anpcMethodLibrary) && inputMetadata$method %in% names(anpcChromatogramRegionData)) {
    message("Attempting to set method library based on input metadata")
    if (!is.null(extractTargetFile(anpcMethodLibrary = inputMetadata$method))) {
      message("Setting new value")
      anpcMethodLibrary <- inputMetadata$method
    } else {
      message("Unsuccessful attempt: No available target file data")
    }
  } else if (!is.null(anpcMethodLibrary) && is.null(extractTargetFile(anpcMethodLibrary = anpcMethodLibrary))) {
    stop("Invalid 'anpcMethodLibrary': No available target file data")
  }
  print(anpcMethodLibrary)
    
  # Validate chromatogram region data
  message("\n6. Chromatogram region")
  if (!is.null(chromatogramRegion) && !validateChromatogramRegion(name = "chromatogramRegion", value = chromatogramRegion)) {
    message("Setting default value")
    chromatogramRegion <- NULL
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
  validateNullableNumericValue(name = "roundDecimalPlace", value = roundDecimalPlace)
  print(roundDecimalPlace)
  
  # Validate metadata anonymiser status
  message("\n8. Metadata anonymiser")
  validateLogicalValue(name = "metadataAnonymiser", value = metadataAnonymiser)
  print(metadataAnonymiser)
  
  message("---\n")

  # Render TICQ Report
  title <- gsub("\\.json$", "", basename(inputPath), ignore.case = TRUE)
  
  message("Rendering TICQ...")
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