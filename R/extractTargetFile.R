#' Extract Target File
#'
#' Extract target m/z data using a TSV formatted target file (published to web URL or local file path) or an in-house method library used at the Australian National Phenome Centre (ANPC).
#' 
#' Target file specified should follow a pre-determined layout used by \href{https://github.com/vmalnathnambiar/exfilms/blob/main/docs/how-to-create-a-target-file.md}{ExfilMS}.
#'
#' If both target file path and ANPC method library is specified, the target file path will be used as the first point of extraction. If no data is found, ANPC method library is then used.
#'
#' @import httr
#' @import readr
#'
#' @export
#' @param targetFilePath Target file path (Default: NULL, Options: Published to web URL or local path of a TSV file): NULL or character
#' @param anpcMethodLibrary ANPC method library (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param roundDecimalPlace Number of decimal places for precision value rounding (Default: NULL): NULL or numeric
#' @returns A data frame representing the target file
#'
#' @examples
#' # Example 1: Specifying a target file with no rounding of decimal places for precision values
#' targetFilePath <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv"
#' targetFile <- ticq::extractTargetFile(targetFilePath = targetFilePath, anpcMethodLibrary = NULL, roundDecimalPlace = NULL)
#' print(targetFile)
#'
#' # Example 2: Specifying an ANPC method library with rounding of precision values up to 4 decimal places
#' targetFile <- ticq::extractTargetFile(targetFilePath = NULL, anpcMethodLibrary = "MS-AA-POS", roundDecimalPlace = 4)
#' print(targetFile)
extractTargetFile <- function(targetFilePath = NULL, anpcMethodLibrary = NULL, roundDecimalPlace = NULL) {
  # ANPC method library and associated URL paths
  anpcMethodLibraryURL <- list(
    "MS-AA-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
    "MS-HIL-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv",
    "MS-HIL-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv",
    "MS-RP-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv",
    "MS-RP-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv"
  )
  
  # Validate parameters
  if (!is.null(targetFilePath) && validateCharacterString(parameterName = "targetFilePath", parameterValue = targetFilePath)) {
    if (grepl("\\.tsv$", targetFilePath, ignore.case = TRUE) &&
        validateDirectoryFileExist(parameterName = "targetFilePath", parameterValue = targetFilePath, pathType = "file")) {
      isPathURL <- FALSE
    } else if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFilePath, ignore.case = TRUE)) {
      isPathURL <- TRUE
    } else {
      stop("Invalid 'targetFilePath': No available TSV file found at the specified path.")
    }
  }
  
  if (!is.null(anpcMethodLibrary) && validateCharacterString(parameterName = "anpcMethodLibrary", parameterValue = anpcMethodLibrary) &&
      !anpcMethodLibrary %in% names(anpcMethodLibraryURL)) {
    stop("Invalid 'anpcMethodLibrary': Must either be NULL, 'MS-AA-POS', 'MS-HIL-POS', 'MS-HIL-NEG', 'MS-RP-POS', or 'MS-RP-NEG'")
  }

  if (!is.null(roundDecimalPlace)) {
    validateNumericValue(parameterName = "roundDecimalPlace", parameterValue = roundDecimalPlace)
  }
  
  # Check availability of target file path and method library
  if (!is.null(targetFilePath) && !is.null(anpcMethodLibrary)) {
    message("Both target file path and ANPC method library specified: Using target file path as first point of extraction")
  } else if (is.null(targetFilePath) && is.null(anpcMethodLibrary)) {
    message("Unable to extract target file: Requires either a target file path or ANPC method library for extraction")
    return(NULL)
  }
  
  # Extract target file
  targetFile <- if (!is.null(targetFilePath)) {
    tryCatch({
      if (isPathURL) {
        response <- httr::GET(targetFilePath)
        statusCode <- httr::status_code(response)
        if (statusCode == 200) {
          readr::read_tsv(rawToChar(response$content), show_col_types = FALSE)
        } else {
          message(paste0("Unable to extract target file: Unsuccessful HTTP request (Status code: ", statusCode, ")"))
          NULL
        }
      } else {
        readr::read_tsv(targetFilePath, show_col_types = FALSE)
      }
    }, warning = function(w) {
      message(paste("Unable to extract target file:", w))
      NULL
    }, error = function(e) {
      message(paste("Unable to extract target file:", e))
      NULL
    })
  } else {
    NULL
  }
  
  targetFile <- if (is.null(targetFile) && !is.null(anpcMethodLibrary)) {
    tryCatch({
      response <- httr::GET(anpcMethodLibraryURL[[anpcMethodLibrary]])
      statusCode <- httr::status_code(response)
      if (statusCode == 200) {
        readr::read_tsv(rawToChar(response$content), show_col_types = FALSE)
      } else {
        message(paste0("Unable to extract target file: Unsuccessful HTTP request (Status code: ", statusCode, ")"))
        NULL
      }
    }, warning = function(w) {
      message(paste("Unable to extract target file:", w))
      NULL
    }, error = function(e) {
      message(paste("Unable to extract target file:", e))
      NULL
    })
  } else {
    targetFile
  }
    
  # Validate target file
  pattern <- c("compoundName", "compoundType", "mzValue", "retentionTime", "msLevel", "internalStandard", "product")
  if (!is.null(targetFile) && !all(pattern %in% colnames(targetFile))) {
    message(paste0(
      "Unable to extract target file: Missing one or more data columns (",
      paste(pattern[!pattern %in% colnames(targetFile)], collapse = ", "),
      ")"
    ))
    targetFile <- NULL
  }
  
  if (is.null(targetFile)) {
    return(targetFile)
  }

  # Round decimal places of precision values
  if (!is.null(roundDecimalPlace)) {
    message(paste("Note: Precision values rounded up to", roundDecimalPlace, "decimal places"))
    targetFile$mzValue <- round(targetFile$mzValue, digits = roundDecimalPlace)
    targetFile$retentionTime <- round(targetFile$retentionTime, digits = roundDecimalPlace)
  }

  return(targetFile)
}