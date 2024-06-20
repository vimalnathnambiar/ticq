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
#' @param targetFilePath Target file path (Default: NULL, Options: Published to web URL or local file path of a TSV file): NULL or character
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
  # Validate parameters
  anpcMethodLibraryURL <- list(
    "MS-AA-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
    "MS-HIL-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv",
    "MS-HIL-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv",
    "MS-RP-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv",
    "MS-RP-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv"
  )
  
  if (!is.null(targetFilePath)) {
    if (length(targetFilePath) != 1 || !is.character(targetFilePath) ||
        !grepl("^(?:http|https)://[^ \"]+&output=tsv$|\\.tsv$", targetFilePath, ignore.case = TRUE)) {
      stop("Invalid 'targetFilePath': Must be NULL or non-empty character string of length 1 representing a published to web URL or local TSV file path")
    } else if (grepl("\\.tsv$", targetFilePath, ignore.case = TRUE) && !file.exists(targetFilePath)) {
      stop("Invalid 'targetFilePath': No available TSV file")
    }
  }

  if (!is.null(anpcMethodLibrary) &&
      (length(anpcMethodLibrary) != 1 || !is.character(anpcMethodLibrary) || !anpcMethodLibrary %in% names(anpcMethodLibraryURL))) {
    stop("Invalid 'anpcMethodLibrary': Must be NULL or non-empty character string of length 1 matching one of the ANPC method libraries")
  }

  if (!is.null(roundDecimalPlace) && (length(roundDecimalPlace) != 1 || !is.numeric(roundDecimalPlace))) {
    stop("Invalid 'roundDecimalPlace': Must be NULL or numerical value of length 1")
  }
  
  # Check availability of both target file path / method library
  if (!is.null(targetFilePath) && !is.null(anpcMethodLibrary)) {
    message("Both target file path and ANPC method library specified: Using target file path as first point of extraction")
  } else if (is.null(targetFilePath) && is.null(anpcMethodLibrary)) {
    message("Unable to extract target file: Requires either a target file path or ANPC method library for extraction")
    return(NULL)
  }
  
  # Extract target file
  targetFile <- if (!is.null(targetFilePath)) {
    tryCatch({
      if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFilePath, ignore.case = TRUE)) {
        response <- httr::GET(targetFilePath)
        statusCode <- httr::status_code(response)
        if (statusCode == 200) {
          readr::read_tsv(rawToChar(response$content), show_col_types = FALSE)
        } else {
          message(paste0("Unable to extract target file: Unsuccessful HTTP request (Status code: ", statusCode, ")"))
          NULL
        }
      } else if (grepl("\\.tsv$", targetFilePath, ignore.case = TRUE)) {
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