#' Extract Target File
#'
#' Extract investigated m/z data of interest from target file (published to web URL or local TSV document) or in-house method library used at the Australian National Phenome Centre (ANPC).
#'
#' To assess MS spectral data acquired using ExfilMS post-targeted spectra filtering, ensure the same target file or in-house method library is defined in TICQ.
#'
#' If both target file and in-house method library is defined, only target file will be checked for. The target file should follow a pre-determined layout used by ExfilMS (https://github.com/vmalnathnambiar/exfilms/blob/main/docs/how-to-create-a-target-file.md).
#'
#' @import httr
#' @import readr
#'
#' @export
#' @param targetFile Published to web or local TSV document (Default: NULL, Options: Published to web URL or local file path): NULL or character
#' @param anpcMethodLibrary ANPC in-house method library (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param roundDecimal Number of decimal places for precision value rounding (Default: NULL): NULL or double
#' @returns A data frame representing the target file
#'
#' @examples
#' # Example 1: Defining a target file with no rounding of precision values
#' targetFile <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv"
#' targetFile <- ticq::extractTargetFile(targetFile = targetFile, anpcMethodLibrary = NULL, roundDecimal = NULL)
#' print(targetFile)
#'
#' # Example 2: Defining an in-house method library with rounding of precision values to 4 decimal places
#' targetFile <- ticq::extractTargetFile(targetFile = NULL, anpcMethodLibrary = "MS-AA-POS", roundDecimal = 4)
#' print(targetFile)
extractTargetFile <- function(targetFile = NULL, anpcMethodLibrary = NULL, roundDecimal = NULL) {
  # ANPC in-house method libraries URL paths
  urlList <- list(
    "MS-AA-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
    "MS-HIL-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv",
    "MS-HIL-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv",
    "MS-RP-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv",
    "MS-RP-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv"
  )
    
  # Column headers
  columnHeader <- c("compoundName", "compoundType", "mzValue", "retentionTime", "msLevel", "internalStandard", "product")
  
  # Check target file / method library
  targetFile <- if (!is.null(targetFile)) {
    # Check published to web URL / local file path
    if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFile, ignore.case = TRUE)) {
      # Parse target file
      response <- tryCatch(httr::GET(targetFile), warning = function(w) NULL, error = function(e) NULL)
      
      if (!is.null(response) && httr::status_code(response) == 200) {
        tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE), warning = function(w) NULL, error = function(e) NULL)
      } else {
        print(paste("Unable to extract target file: Unsuccessful HTTP request -", targetFile))
        NULL
      }
    } else if (grepl("\\.tsv$", targetFile, ignore.case = TRUE)) {
      # Parse target file
      if (file.exists(targetFile)) {
        tryCatch(readr::read_tsv(targetFile, show_col_types = FALSE), warning = function(w) NULL, error = function(e) NULL)
      } else {
        print(paste("Unable to extract target file: Path not found -", targetFile))
        NULL
      }
    }else {
      print("Unable to extract target file: Invalid target file type")
      NULL
    }
  } else if (!is.null(anpcMethodLibrary)) {
    # Retrieve URL path to method library
    if (anpcMethodLibrary %in% names(urlList)) {
      # Parse target file
      response <- tryCatch(httr::GET(urlList[[anpcMethodLibrary]]), warning = function(w) NULL, error = function(e) NULL)
      
      if (!is.null(response) && httr::status_code(response) == 200) {
        # Parse target file
        tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE), warning = function(w) NULL, error = function(e) NULL)
      } else {
        print(paste("Unable to extract target file: Unsuccessful HTTP request -", urlList[[anpcMethodLibrary]]))
        NULL
      }
    } else {
      print("Unable to extract target file: Invalid in-house ANPC method library")
      NULL
    }
  } else {
    print("Unable to extract target file: Target file / ANPC method library not defined")
    NULL
  }
  
  # Check column headers
  if (!is.null(targetFile)) {
    missingColumn <- columnHeader[!columnHeader %in% colnames(targetFile)]
    if (length(missingColumn) > 0) {
      # Reset
      targetFile <- NULL
      print(paste("Unable to extract target file: Missing column headers -", missingColumn))
    }
  }
  
  # Round decimal place
  if (!is.null(targetFile) && !is.null(roundDecimal)) {
    print(paste("NOTE: Precision values (m/z and retention time) rounded to", roundDecimal, "decimal place(s)"))
    targetFile$mzValue <- round(targetFile$mzValue, digits = roundDecimal)
    targetFile$retentionTime <- round(targetFile$retentionTime, digits = roundDecimal)
  }
  
  return(targetFile)
}