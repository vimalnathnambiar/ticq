#' Extract Target File
#'
#' Extract target m/z data from a TSV formatted target file (published to web URL or local file path) or an in-house method library used at the Australian National Phenome Centre (ANPC).
#'
#' If both target file path and ANPC method library is specified, the target file path will be used as default extraction. If no data is found, ANPC method library is then used.
#' 
#' When specifying a target file path, please ensure the TSV document follows a pre-determined layout used by ExfilMS (https://github.com/vmalnathnambiar/exfilms/blob/main/docs/how-to-create-a-target-file.md).
#'
#' @import httr
#' @import readr
#'
#' @export
#' @param targetFilePath Published to web URL or local path of the TSV formatted target file (Default: NULL, Options: Published to web URL or local file path): NULL or character
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
  # ANPC method library URL paths
  anpcMethodLibraryURL <- list(
    "MS-AA-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=0&single=true&output=tsv",
    "MS-HIL-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1628071829&single=true&output=tsv",
    "MS-HIL-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1082422270&single=true&output=tsv",
    "MS-RP-POS" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=1641228752&single=true&output=tsv",
    "MS-RP-NEG" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSeo31hlruA3QuwoESz5IDJ9Nu6ndSAgLTRn3uc45rOPO4BlksfHzh9xtNB22Oes9JOxhEbI4NK-zxl/pub?gid=311622861&single=true&output=tsv"
  )
    
  # Target file column header patterns
  pattern <- c("compoundName", "compoundType", "mzValue", "retentionTime", "msLevel", "internalStandard", "product")
  
  # Validate parameters
    # Target file path
    if (!is.null(targetFilePath)) {
      if (!is.character(targetFilePath) || length(targetFilePath) != 1 || targetFilePath == "" ||
          !(grepl("^(?:http|https)://[^ \"]+&output=tsv$|\\.tsv$", targetFilePath, ignore.case = TRUE))) {
        stop("Invalid 'targetFilePath': Must be NULL or a non-empty character string of a published to web URL or local TSV file path")
      } else if (grepl("\\.tsv$", targetFilePath, ignore.case = TRUE) && !file.exists(targetFilePath)) {
        stop("Invalid 'targetFilePath': Must be an existing TSV file")
      }
    }
  
    # ANPC method library
    if (!is.null(anpcMethodLibrary) &&
        (!is.character(anpcMethodLibrary) || length(anpcMethodLibrary) != 1 || !(anpcMethodLibrary %in% names(anpcMethodLibraryURL)))) {
      stop("Invalid 'anpcMethodLibrary': Must be NULL or a non-empty character string that matches one of the ANPC method library options")
    }
  
    # Round decimal place
    if (!is.null(roundDecimalPlace) && (!is.numeric(roundDecimalPlace) || length(roundDecimalPlace) != 1)) {
      stop("Invalid 'roundDecimalPlace': Must be NULL or a single numeric value")
    }
  
  # Check availability of both target file path / method library
  if (!is.null(targetFilePath) && !is.null(anpcMethodLibrary)) {
    message("Both target file path and ANPC method library specified: Using target file path as first point of extraction")
  } else if (is.null(targetFilePath) && is.null(anpcMethodLibrary)) {
    message("Unable to extract target file: No available target file path or ANPC method library specified")
    return(NULL)
  }
  
  # Extract target file from target file path
    # Target file path
    targetFile <- if (!is.null(targetFilePath)) {
      # Check published to web URL / local file path
      if (grepl("^(?:http|https)://[^ \"]+&output=tsv$", targetFilePath, ignore.case = TRUE)) {
        # Parse target file
        response <- tryCatch(
          httr::GET(targetFilePath),
          warning = function(w) {
            message(paste("Unable to extract target file:", w))
            NULL
          },
          error = function(e) {
            message(paste("Unable to extract target file:", e))
            NULL
          }
        )
        
        if (!is.null(response) && httr::status_code(response) == 200) {
          tryCatch(
            readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
            warning = function(w) {
              message(paste("Unable to extract target file:", w))
              NULL
            },
            error = function(e) {
              message(paste("Unable to extract target file:", e))
              NULL
            }
          )
        } else {
          message("Unable to extract target file: Unsuccessful HTTP request")
          NULL
        }
      } else if (grepl("\\.tsv$", targetFilePath, ignore.case = TRUE)) {
        # Parse target file
        tryCatch(
          readr::read_tsv(targetFilePath, show_col_types = FALSE),
          warning = function(w) {
            message(paste("Unable to extract target file:", w))
            NULL
          },
          error = function(e)
          {
            message(paste("Unable to extract target file:", e))
            NULL
          }
        )
      }
    } else {
      NULL
    }
  
    # Method library
    targetFile <- if (is.null(targetFile) && !is.null(anpcMethodLibrary)) {
      # Parse target file
      response <- tryCatch(
        httr::GET(anpcMethodLibraryURL[[anpcMethodLibrary]]),
        warning = function(w) {
          message(paste("Unable to extract target file:", w))
          NULL
        },
        error = function(e) {
          message(paste("Unable to extract target file:", e))
          NULL
        }
      )
    
      if (!is.null(response) && httr::status_code(response) == 200) {
        tryCatch(
          readr::read_tsv(rawToChar(response$content), show_col_types = FALSE),
          warning = function(w) {
            message(paste("Unable to extract target file:", w))
            NULL
          },
          error = function(e) {
            message(paste("Unable to extract target file:", e))
            NULL
          }
        )
      } else {
        message("Unable to extract target file: Unsuccessful HTTP request")
        NULL
      }
    } else {
      targetFile
    }
  
  # Check column headers
  if (!is.null(targetFile) && !all(pattern %in% colnames(targetFile))) {
    message(paste0(
      "Unable to extract target file: Missing one or more data columns (",
      paste(pattern[!pattern %in% colnames(targetFile)], collapse = ", "),
      ")"
    ))
    targetFile <- NULL
  }

  # Round decimal places of precision values
  if (!is.null(targetFile) && !is.null(roundDecimalPlace)) {
    # m/z value
    targetFile$mzValue <- round(targetFile$mzValue, digits = roundDecimalPlace)

    # Retention time
    targetFile$retentionTime <- round(targetFile$retentionTime, digits = roundDecimalPlace)

    message(paste("Note: Precision values rounded up to", roundDecimalPlace, "decimal places"))
  }

  return(targetFile)
}