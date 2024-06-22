#' Retrieve File List
#'
#' Retrieve a list of file names matching a specified file extension type.
#'
#' @export
#' @param inputPath A character string representing an existing directory or file path. Should not be NA or an empty character string.
#' @param fileExtension A character string representing a file extension type (e.g., "JSON"). (Default: `NULL`)
#' @returns A list of file names matching the specified file extension type.
retrieveFileList <- function(inputPath, fileExtension = NULL) {
  # Validate parameters
  validateDirectoryFileExist(parameterName = "inputPath", parameterValue = inputPath, pathType = "both")
  if (!is.null(fileExtension)) {
    validateCharacterString(parameterName = "fileExtension", parameterValue = fileExtension)
  }
  
  # Retrieve list of file names
  if (file.info(inputPath)$isdir) {
    fileList <- list.files(
      path = inputPath,
      pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL,
      ignore.case = TRUE
    )
    return(if (length(fileList) == 0) character(0) else fileList)
  } else if (!is.null(fileExtension) && !checkFileExtension(parameterName = "inputPath", parameterValue = inputPath, fileExtension = fileExtension)) {
    return(character(0))
  } else {
    return(basename(inputPath))
  }
}