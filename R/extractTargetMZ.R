#' extractTargetMZ
#' 
#' Extract target m/z data from target file (published to web URL or local TSV document) or in-house method library used at the Australian National Phenome Centre (ANPC).
#' 
#' @import httr
#' @import readr
#' 
#' @export
#' @param targetFile TSV document that contains target m/z data (Default: NULL, Options: Published to web URL link or local file path): NULL or character
#' @param anpcMethodLibrary ANPC method library (Default: NULL, Options: "MS-AA-POS", "MS-HIL-POS", "MS-HIL-NEG", "MS-RP-POS" or "MS-RP-NEG"): NULL or character
#' @param roundDecimal The number of decimal places to round the target m/z values to for analysis (Default: NULL): NULL or double
#' @returns A data frame containing the extracted target m/z data
#' 
#' @examples
#' # Example 1: Specifying a target file with no rounding of decimal places for the target m/z values
#' targetMZ <- ticq::extractTargetMZ(targetFile = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv", 
#'                                   anpcMethodLibrary = NULL, 
#'                                   roundDecimal = NULL)
#' 
#' # Example 2: Choosing an in-house method library used at the Australian National Phenome Centre (ANPC) and rounding the target m/z values to 4 decimal places
#' targetMZ <- ticq::extractTargetMZ(targetFile = NULL, 
#'                                   anpcMethodLibrary = "MS-AA-POS", 
#'                                   roundDecimal = 4)
extractTargetMZ <- function(targetFile = NULL, 
                            anpcMethodLibrary = NULL, 
                            roundDecimal = NULL) {
  # Set defaults
  targetMZ <- NULL
  
  # Check if either target file or anpc method library is specified
  if (!is.null(targetFile)) {
    # Check for published to web URL or local file path (TSV document)
    if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFile, ignore.case = TRUE)) {
      response <- tryCatch(httr::GET(targetFile),
                           warning = function(w) NULL,
                           error = function(e) NULL)
      
      # If URL response is NOT NULL & status code = 200
      if (!is.null(response) && httr::status_code(response) == 200) {
        # Parse TSV data
        targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content),
                                             show_col_types = FALSE),
                             warning = function(w) NULL,
                             error = function(e) NULL)
      }
    } else if (grepl("\\.tsv$", targetFile, ignore.case = TRUE)) {
      # Parse TSV data
      targetMZ <- tryCatch(readr::read_tsv(targetFile,
                                           show_col_types = FALSE),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    }
  } else if (!is.null(anpcMethodLibrary)) {
    # Check if one of the ANPC method libraries is specified
    if (anpcMethodLibrary == "MS-AA-POS") {
      response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } else if (anpcMethodLibrary == "MS-HIL-POS") {
      response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } else if (anpcMethodLibrary == "MS-HIL-NEG") {
      response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } else if (anpcMethodLibrary == "MS-RP-POS") {
      response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } else if (anpcMethodLibrary == "MS-RP-NEG") {
      response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv"),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    } 
    # else if (anpcMethodLibrary == "MS-UT-POS") {
    #   response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=662696694&single=true&output=tsv"),
    #                        warning = function(w) NULL,
    #                        error = function(e) NULL)
    # } else if (anpcMethodLibrary == "MS-UT-NEG") {
    #   response <- tryCatch(httr::GET("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1106522204&single=true&output=tsv"),
    #                        warning = function(w) NULL,
    #                        error = function(e) NULL)
    # } 
    else {
      response <- NULL
    }
    
    # If URL response is NOT NULL & status code = 200
    if (!is.null(response) && httr::status_code(response) == 200) {
      # Parse TSV data
      targetMZ <- tryCatch(readr::read_tsv(rawToChar(response$content),
                                           show_col_types = FALSE),
                           warning = function(w) NULL,
                           error = function(e) NULL)
    }
  }
  
  # If target m/z data is NOT NULL & is missing appropriate header columns
  if (!is.null(targetMZ) 
      && !all(c("analyteName", "analyteType", "mzValue", "retentionTime", "msLevel", "internalStandard", "product") %in% colnames(targetMZ))) {
    targetMZ <- NULL
  }
  
  # If target m/z data is NOT NULL & roundDecimal is specified
  if (!is.null(targetMZ) && !is.null(roundDecimal)) {
    targetMZ$mzValue <- round(x = targetMZ$mzValue, 
                              digits = roundDecimal)
  }
  
  return(targetMZ)
}