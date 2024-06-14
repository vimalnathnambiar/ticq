#' Retrieve File Name
#'
#' Retrieve file names of a specific extension type stored within an input path.
#'
#' @export
#' @param inputPath Input path: character
#' @param fileExtension File extension for file name retrieval (Default: NULL): NULL or character
#' @returns File names with matching file extension: character vector
retrieveFileName <- function(inputPath, fileExtension = NULL) {
  # Retrieve file names
  return(
    if (!is.null(inputPath) && (dir.exists(inputPath) || file.exists(inputPath))) {
      if (file.info(inputPath)$isdir) {
        list.files(
          path = inputPath,
          pattern = if (!is.null(fileExtension)) paste0("\\.", fileExtension, "$") else NULL,
          ignore.case = TRUE
        )
      } else if (!is.null(fileExtension) && !grepl(paste0("\\.", fileExtension, "$"), inputPath, ignore.case = TRUE)) {
        character(0)
      } else {
        basename(inputPath)
      }
    } else {
      print(paste("Unable to retrieve file names: Input path not found -", inputPath))
      character(0)
    }
  )
}