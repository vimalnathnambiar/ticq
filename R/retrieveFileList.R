#' Retrieve File List
#'
#' Retrieve a list of file names matching a defined file extension type (i.e., "JSON", "XML").
#'
#' @export
#' @param inputPath Input path: character
#' @param fileExtension File extension type (Default: NULL): NULL or character
#' @returns A list of file names matching the defined file extension type: character or character vector
retrieveFileList <- function(inputPath, fileExtension = NULL) {
  # Validate parameters
    # Input path
    if (is.null(inputPath) || !is.character(inputPath)) {
      message("Unable to retrieve file list: Invalid input path")
      return(character(0))
    }
  
    if (!dir.exists(inputPath) && !file.exists(inputPath)) {
      message("Unable to retrieve file list: Input path does not exists")
      return(character(0))
    }
  
    # File extension
    if (!is.null(fileExtension) && (!is.character(fileExtension) || length(fileExtension) !=1)) {
      message("Invalid file extension: Setting default (NULL)")
      fileExtension <- NULL
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