#' extractTargetMZ
#'
#' Extract target m/z data from a target file (published to web URL or local TSV document) or an in-house method library used at the Australian National Phenome Centre (ANPC).
#'
#' @import httr
#' @import readr
#'
#' @export
#' @param targetFile TSV document that contains target m/z data (Default: NULL, Options: Published to web URL or local file path): NULL or character
#' @param anpcMethodLibrary In-house method library used at the ANPC (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param roundDecimal Number of decimal places to round target m/z precision values (Default: NULL): NULL or double
#' @returns Data frame containing the target m/z data
#'
#' @examples
#' # Example 1: Specify a target file and perform no rounding of m/z precision values
#' targetMZ <- ticq::extractTargetMZ(targetFile = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
#'                                   anpcMethodLibrary = NULL,
#'                                   roundDecimal = NULL)
#'
#' # Example 2: Specify an in-house method library and round the m/z precision values to 4 decimal places
#' targetMZ <- ticq::extractTargetMZ(targetFile = NULL,
#'                                   anpcMethodLibrary = "MS-AA-POS",
#'                                   roundDecimal = 4)
extractTargetMZ <- function(targetFile = NULL,
                            anpcMethodLibrary = NULL,
                            roundDecimal = NULL) {
  # Defaults
  targetMZ <- NULL
  
  # Check if either target file or anpc method library is specified
  if (!is.null(targetFile)) {
    # Check for published to web URL or local file path (TSV document)
    if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFile, ignore.case = TRUE)) {
      # Perform HTTP request
      response <- tryCatch(httr::GET(targetFile), 
                           warning = function(w) NULL,
                           error = function(e) NULL)
      
      # If URL response is NOT NULL & status code = 200
      if (!is.null(response) && httr::status_code(response) == 200) {
        # Parse TSV data
        targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
                             warning = function(w) NULL,
                             error = function(e) NULL)
      }
    } else if (grepl("\\.tsv$", targetFile, ignore.case = TRUE)) {
      # Parse TSV data
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
    
    # Check if one of the ANPC method libraries is specified
    if (anpcMethodLibrary %in% names(urlList)) {
      # Perform HTTP request
      response <- tryCatch(httr::GET(urlList[[anpcMethodLibrary]]),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } else {
      response <- NULL
    }
    
    # If URL response is NOT NULL & status code = 200
    if (!is.null(response) && httr::status_code(response) == 200) {
      # Parse TSV data
      targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    }
  }
  
  # If target m/z data is NOT NULL & is missing appropriate header columns
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
  
  # If target m/z data is NOT NULL & roundDecimal is specified
  if (!is.null(targetMZ) && !is.null(roundDecimal)) {
    targetMZ$mzValue <- round(targetMZ$mzValue, digits = roundDecimal)
  }
  
  return(targetMZ)
}