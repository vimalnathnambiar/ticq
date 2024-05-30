#' retrieveFilename
#'
#' Filter and retrieve names of files of a specific extension type stored within an input path.
#'
#' @export
#' @param input Input path containing files of interest: character
#' @param fileExtension File extension to filter for (Default: NULL): NULL or character
#' @returns File names matching the file extension specified: character vector
retrieveFilename <- function(input, fileExtension = NULL) {
  # Check if input exists (either as a directory or a file)
  if (dir.exists(input) || file.exists(input)) {
    # Extract file information
    fileInfo <- file.info(input)
    
    # Retrieve file list (either from a directory or a file)
    if (fileInfo$isdir) {
      # Check if file extension is specified
      if (!is.null(fileExtension)) {
        inputFiles <- list.files(path = input,
                                 pattern = paste0("\\.", fileExtension, "$"),
                                 ignore.case = TRUE)
      } else {
        inputFiles <- list.files(path = input)
      }
    } else {
      # Check if file extension is specified
      if (!is.null(fileExtension)) {
        if (grepl(paste0("\\.", fileExtension, "$"), input, ignore.case = TRUE)) {
          inputFiles <- basename(input)
        } else {
          inputFiles <- character(0)
        }
      } else {
        inputFiles <- basename(input)
      }
    }
  } else {
    print("Input not found")
    inputFiles <- character(0)
  }
  
  return(inputFiles)
}