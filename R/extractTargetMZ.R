#' Extract Target m/z
#'
#' Extract target m/z data from a target file (published to web URL or local TSV document) or an in-house method library used at the Australian National Phenome Centre (ANPC).
#' 
#' To assess targeted MS spectral data acquired using ExfilMS post-targeted spectra filtering, ensure the same target file or in-house method library is defined in TICQ.
#'
#' If both target file and in-house method library is defined, only target file will be checked for. The target file should follow a pre-determined layout used by ExfilMS (https://github.com/vmalnathnambiar/exfilms/blob/main/docs/how-to-create-a-target-file.md).
#'
#' @import httr
#' @import readr
#'
#' @export
#' @param targetFile Published to web or local TSV document containing target m/z data used by ExfilMS for spectra filtering (Default: NULL, Options: Published to web URL or local file path): NULL or character
#' @param anpcMethodLibrary ANPC in-house method library (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param roundDecimal Number of decimal places for precision value rounding (Default: NULL): NULL or double
#' @returns A data frame containing target m/z data
#'
#' @examples
#' # Example 1: Defining a target file with no rounding of precision values
#' targetMZ <- ticq::extractTargetMZ(targetFile = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
#'                                   anpcMethodLibrary = NULL,
#'                                   roundDecimal = NULL)
#' print(targetMZ)
#'
#' # Example 2: Defining an in-house method library with rounding of precision values to 4 decimal places
#' targetMZ <- ticq::extractTargetMZ(targetFile = NULL,
#'                                   anpcMethodLibrary = "MS-AA-POS",
#'                                   roundDecimal = 4)
#' print(targetMZ)
extractTargetMZ <- function(targetFile = NULL,
                            anpcMethodLibrary = NULL,
                            roundDecimal = NULL) {
  # Defaults
  targetMZ <- NULL
  
  # Check if either a target file or an ANPC in-house method library is defined
  if (!is.null(targetFile)) {
    # Check for published to web URL or local file path
    if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFile, ignore.case = TRUE)) {
      # Perform HTTP request
      response <- tryCatch(httr::GET(targetFile),
                           warning = function(w) NULL,
                           error = function(e) NULL)
      
      # If HTTP request is successful
      if (!is.null(response) && httr::status_code(response) == 200) {
        # Parse and read target m/z data
        targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
                             warning = function(w) NULL,
                             error = function(e) NULL)
      }
    } else if (grepl("\\.tsv$", targetFile, ignore.case = TRUE)) {
      # Parse and read target m/z data
      targetMZ <- tryCatch(readr::read_tsv(targetFile, show_col_types = FALSE),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    }
  } else if (!is.null(anpcMethodLibrary)) {
    urlList <- list("MS-AA-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
                    "MS-HIL-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv",
                    "MS-HIL-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv",
                    "MS-RP-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv",
                    "MS-RP-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv")
    
    # Check if a valid ANPC in-house method library is defined
    if (anpcMethodLibrary %in% names(urlList)) {
      # Perform HTTP request
      response <- tryCatch(httr::GET(urlList[[anpcMethodLibrary]]),
                           warning = function(w) NULL,
                           error = function(e) NULL)
      
      # If HTTP request is successful
      if (!is.null(response) && httr::status_code(response) == 200) {
        # Parse and read target m/z data
        targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
                             warning = function(w) NULL,
                             error = function(e) NULL)
      }
    }
  }
  
  # Check target m/z data for required headers
  if (!is.null(targetMZ) &&
      !all(c("analyteName",
             "analyteType",
             "mzValue",
             "retentionTime",
             "msLevel",
             "internalStandard",
             "product") %in% colnames(targetMZ))) {
    targetMZ <- NULL
  }
  
  # Round precision values in target m/z data if defined
  if (!is.null(targetMZ) && !is.null(roundDecimal)) {
    # m/z value
    targetMZ$mzValue <- round(targetMZ$mzValue, digits = roundDecimal)
    
    # Retention time
    targetMZ$retentionTime <- round(targetMZ$retentionTime, digits = roundDecimal)
  }
  
  return(targetMZ)
}