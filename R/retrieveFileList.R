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
    # Input path
    if (is.null(inputPath) || !is.character(inputPath) || length(inputPath) != 1 || inputPath == "") {
      stop("Invalid 'inputPath': Must be a non-NULL and non-empty character string of a directory or file path")
    }
  
    if (!dir.exists(inputPath) && !file.exists(inputPath)) {
      stop("Invalid 'inputPath': Must be an existing directory or file")
    }
  
    # File extension
    if (!is.null(fileExtension) && (!is.character(fileExtension) || length(fileExtension) !=1 || fileExtension == "")) {
      stop("Invalid 'fileExtension': Must be NULL or a non-empty character string of a file extension format")
    }
  
  # Check if directory or file and retrieve file names of matching extension type accordingly
  if (file.info(inputPath)$isdir) {
    fileList <- list.files(
      path = inputPath,
      pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL,
      ignore.case = TRUE
    )
    
    if (length(fileList) == 0) {
      return(character(0))
    }
    return(fileList)
  } else if (!is.null(fileExtension) && !grepl(paste0("\\.", fileExtension, "$"), inputPath, ignore.case = TRUE)) {
    return(character(0))
  } else {
    return(basename(inputPath))
  }
}