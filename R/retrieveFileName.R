#' Retrieve File Name
#'
#' Retrieve file names matching a specified file extension type.
#'
#' @export
#' @param inputPath A character string representing an existing directory or file path.
#' @param fileExtension A character string representing a file extension type (e.g., "JSON"). (Default: `NULL`)
#' @returns A character vector of file names matching the specified file extension type.
retrieveFileName <- function(inputPath, fileExtension = NULL) {
  # Validate parameters
  validateDirectoryFileExist(parameterName = "inputPath", parameterValue = inputPath, pathType = "both")
  if (!is.null(fileExtension) && (length(fileExtension) != 1 || !is.character(fileExtension) || is.na(fileExtension) || fileExtension == "")) {
    stop("Invalid 'fileExtension': Must either be NULL or a non-NA, non-empty character string of length 1")
  }
  
  # Retrieve list of file names
  if (file.info(inputPath)$isdir) {
    fileList <- list.files(path = inputPath, pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL, ignore.case = TRUE)
    return(if (length(fileList) == 0) character(0) else fileList)
  } else if (!is.null(fileExtension) && !checkFileExtension(parameterName = "inputPath", parameterValue = inputPath, fileExtension = fileExtension)) {
    return(character(0))
  } else {
    return(basename(inputPath))
  }
}