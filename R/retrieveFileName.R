#' Retrieve File Name
#'
#' Retrieve file names from a directory or file path that matches a specific file extension pattern.
#'
#' @export
#' @param inputPath A character string representing a directory or file path.
#' @param fileExtension A character string representing a file extension pattern (e.g., "JSON"). (Default: `NULL`)
#' @returns A character vector of the file names matching the specified file extension pattern.
retrieveFileName <- function(inputPath, fileExtension = NULL) {
  # Validate parameters
  validateDirectoryFileExist(name = "inputPath", value = inputPath, pathType = "both")
  validateNullableCharacterStringValue(name = "fileExtension", value = fileExtension)
  
  # Retrieve file names
  if (file.info(inputPath)$isdir) {
    fileList <- list.files(path = inputPath, pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL, ignore.case = TRUE)
    return(if (length(fileList) == 0) character(0) else fileList)
  } else if (!is.null(fileExtension) && !validateFileExtensionStatus(name = "inputPath", value = inputPath, fileExtension = fileExtension)) {
    return(character(0))
  } else {
    return(basename(inputPath))
  }
}