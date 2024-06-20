#' Retrieve File List
#'
#' Retrieve a list of file names matching a specified file extension type (i.e., "JSON", "XML").
#'
#' @export
#' @param inputPath Input path: character
#' @param fileExtension File extension type (Default: NULL): NULL or character
#' @returns A list of file names matching the specified file extension type
retrieveFileList <- function(inputPath, fileExtension = NULL) {
  # Validate parameters
  if (length(inputPath) != 1 || !is.character(inputPath) || is.na(inputPath) || inputPath == "") {
    stop("Invalid 'inputPath': Must be non-empty character string of length 1 representing a directory or file path")
  } else if (!dir.exists(inputPath) && !file.exists(inputPath)) {
    stop("Invalid 'inputPath': No available directory or file at the specified path")
  }

  if (!is.null(fileExtension) && (length(fileExtension) != 1 || !is.character(fileExtension) || is.na(fileExtension) || fileExtension == "")) {
    stop("Invalid 'fileExtension': Must be NULL or non-empty character string of length 1 representing a file extension format")
  }
  
  # Retrieve list of file names
  if (file.info(inputPath)$isdir) {
    fileList <- list.files(
      path = inputPath,
      pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL,
      ignore.case = TRUE
    )
    return(if (length(fileList) == 0) character(0) else fileList)
  } else if (!is.null(fileExtension) && !grepl(paste0("\\.", fileExtension, "$"), inputPath, ignore.case = TRUE)) {
    return(character(0))
  } else {
    return(basename(inputPath))
  }
}