#' Validate Character Vector
#' 
#' A helper function to validate a character vector. 
#' It ensures that the vector has a length greater than 0, is a character type, and does not contain NA or empty strings.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character vector to be validated.
#' @returns Invisible TRUE (logical) if the parameter is valid; otherwise, it stops with an error message.
validateCharacterVector <- function(parameterName, parameterValue) {
  if (length(parameterValue) == 0 || !is.character(parameterValue) || any(is.na(parameterValue)) || any(parameterValue == "")) {
    stop(paste0("Invalid '", parameterName, "': Must be a non-NA, non-empty character vector with a minimum length of 1."))
  }
  return(invisible(TRUE))
}

#' Validate Character String
#' 
#' A helper function to validate a character string.
#' It ensures that the string has a length of 1, is a character type, and is not NA or an empty character string.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string to be validated.
#' @returns Invisible TRUE (logical) if the parameter is valid; otherwise, it stops with an error message.
validateCharacterString <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.character(parameterValue) || is.na(parameterValue) || parameterValue == "") {
    stop(paste0("Invalid '", parameterName, "': Must be a non-NA, non-empty character string of length 1."))
  }
  return(invisible(TRUE))
}

#' Validate Numeric Value
#' 
#' A helper function to validate a numeric value.
#' It ensures that the value has a length of 1 and is a numeric type.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A numeric value to be validated.
#' @returns Invisible TRUE (logical) if the parameter is valid; otherwise, it stops with an error message.
validateNumericValue <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.numeric(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': Must be a numeric value of length 1."))
  }
  return(invisible(TRUE))
}

#' Validate Logical Value
#' 
#' A helper function to validate a numeric value.
#' It ensures that the value has a length of 1 and is a logical type.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A logical value to be validated.
#' @returns Invisible TRUE (logical) if the parameter is valid; otherwise, it stops with an error message.
validateLogicalValue <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.logical(parameterValue) || is.na(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': Must be a logical value of length 1."))
  }
  return(invisible(TRUE))
}

#' Validate Directory and File Path
#' 
#' A helper function to validate a character string representing either a directory or file path.
#' It ensures that the path specified has a length of 1, is a character type, is not NA or an empty character string,
#' and checks if the path exists based on a path type.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing either a directory or file path to be validated.
#' @param pathType A character string representing the path type to validate for. (Default: `"both"`; Options: `"directory"`, `"file"`, `"both"`)
#' @returns Invisible TRUE (logical) if the parameter is valid; otherwise, it stops with an error message.
validateDirectoryFileExist <- function(parameterName, parameterValue, pathType = "both") {
  validateCharacterString(parameterName, parameterValue)
  if (!pathType %in% c("directory", "file", "both")) {
    stop("Invalid 'pathType': Must either be 'directory', 'file', or 'both'.")
  }
  
  if (pathType == "directory" && !dir.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No directory was found at the specified path."))
  } else if (pathType == "file" && !file.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No file was found at the specified path."))
  } else if (pathType == "both" && !dir.exists(parameterValue) && !file.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No directory or file was found at the specified path."))
  }
  return(invisible(TRUE))
}

#' Check File Extension
#' 
#' A helper function to validate a character string representing a file path, and check if it matches a specific file extension..
#' It ensures that the file has a length of 1, is a character type, is not NA or an empty character string, is a valid file at the specified path,
#' and checks if the file contains the file extension specified.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing a file path to match to a file extension specified.
#' @param fileExtension A character string representing a file extension (e.g., "JSON").
#' @returns Invisible TRUE (logical) if the file path is valid and matches the file extension specified; otherwise returns invisible FALSE (logical)
checkFileExtension <- function(parameterName, parameterValue, fileExtension) {
  validateCharacterString(parameterName, parameterValue)
  validateCharacterString(parameterName = "fileExtension", parameterValue = fileExtension)
  validateDirectoryFileExist(parameterName, parameterValue, pathType = "file")
  if (!grepl(paste0("\\.", fileExtension, "$"), inputPath, ignore.case = TRUE)) return(invisible(FALSE)) else return(invisible(TRUE))
}

#' Validate Directory Path Format
#' 
#' A helper function to validate and standardize the format of a character string representing a directory path. 
#' It ensures that the directory path has a length of 1, is a character type, is not NA or an empty character string, and ends with a forward slash (/).
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing the directory path to be validated.
#' @returns A character string representing the validated and standardised directory path with a forward slash (/) (character); otherwise, it stops with an error message.
validateDirectoryPathFormat <- function(parameterName, parameterValue) {
  validateCharacterString(parameterName, parameterValue)
  if (substring(parameterValue, nchar(parameterValue)) != "/") {
    return(paste0(parameterValue, "/"))
  }
  return(parameterValue)
}

#' Create Directory Path
#' 
#' A helper function that helps create a directory using a character string representing a directory path.
#' It ensures that the directory path has a length of 1, is a character type, is not NA or an empty character string, ends with a forward slash (/),
#' and creates the directory at the specified path.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing a directory path to be created.
#' @returns Invisible TRUE (logical) if the path is created; otherwise, it stops with an error message.
createDirectoryPath <- function(parameterName, parameterValue) {
  parameterValue <- validateDirectoryPathFormat(parameterName = parameterName, parameterValue = parameterValue)
  tryCatch({
    if (!dir.exists(parameterValue)) {
      dir.create(parameterValue, recursive = TRUE)
    }
    return(invisible(TRUE))
  }, warning = function(w) {
    stop(paste("Unable to create directory path:", w))
  }, error = function(e) {
    stop(paste("Unable to create directory path:", e))
  })
}