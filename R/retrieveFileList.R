#' retrieveFileList
#'
#' Filter and retrieve file list of a specified extension type stored within an input path.
#'
#' @export
#' @param input Input path containing a list of files to retrieve: character
#' @param fileExtension File extension to filter for (Default: NULL): NULL or character
#' @returns List of files stored within the input path that matches the file extension specified: character vector
#'
#' @examples
#' inputFiles <- ticq::retrieveFileList(input = "/path/to/input/directory/", fileExtension = "JSON")
retrieveFileList <- function(input, fileExtension = NULL) {
  # Check if input specified exists (either as a directory or a file)
  if (dir.exists(input) || file.exists(input)) {
    # Extract file information
    fileInfo <- file.info(input)
    
    # Check if input is a directory or a file, and retrieve file list
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