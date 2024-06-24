#' Validate Character Vector
#' 
#' A helper function to validate a character vector. 
#' It ensures that the vector specified has a length greater than 0, is a character type, and does not contain NA or empty strings.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character vector to be validated.
#' @returns Invisible TRUE if the character vector is valid; otherwise, it stops with an error message.
validateCharacterVector <- function(parameterName, parameterValue) {
  if (length(parameterValue) == 0 || !is.character(parameterValue) || any(is.na(parameterValue)) || any(parameterValue == "")) {
    stop(paste0("Invalid '", parameterName, "': Must be a non-NA, non-empty character vector with a minimum length of 1"))
  }
  return(invisible(TRUE))
}

#' Validate Character String
#' 
#' A helper function to validate a character string.
#' It ensures that the string specified has a length of 1, is a character type, and is not NA or an empty character string.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string to be validated.
#' @returns Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
validateCharacterString <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.character(parameterValue) || is.na(parameterValue) || parameterValue == "") {
    stop(paste0("Invalid '", parameterName, "': Must be a non-NA, non-empty character string of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Numeric Value
#' 
#' A helper function to validate a numeric value.
#' It ensures that the value specified has a length of 1 and is a numeric type.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A numeric value to be validated.
#' @returns Invisible TRUE if the numeric value is valid; otherwise, it stops with an error message.
validateNumericValue <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.numeric(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': Must be a numeric value of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Logical Value
#' 
#' A helper function to validate a logical value.
#' It ensures that the value specified has a length of 1 and is a logical type.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A logical value to be validated.
#' @returns Invisible TRUE if the logical value is valid; otherwise, it stops with an error message.
validateLogicalValue <- function(parameterName, parameterValue) {
  if (length(parameterValue) != 1 || !is.logical(parameterValue) || is.na(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': Must be a logical value of length 1 (TRUE or FALSE)"))
  }
  return(invisible(TRUE))
}

#' Validate Directory and File Path
#' 
#' A helper function to validate a character string representing either a directory or file path.
#' It ensures that the path specified has a length of 1, is a character type, is not NA or an empty character string,
#' and checks if the path exists based on a path type specified.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing either a directory or file path to be validated.
#' @param pathType A character string representing the path type to validate for. (Default: `"both"`; Options: `"directory"`, `"file"`, or `"both"`)
#' @returns Invisible TRUE if the directory or file path is valid; otherwise, it stops with an error message.
validateDirectoryFileExist <- function(parameterName, parameterValue, pathType = "both") {
  validateCharacterString(parameterName, parameterValue)
  if (!pathType %in% c("directory", "file", "both")) {
    stop("Invalid 'pathType': Must either be 'directory', 'file', or 'both'")
  } else if (pathType == "directory" && !dir.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No directory was found at the specified path"))
  } else if (pathType == "file" && !file.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No file was found at the specified path"))
  } else if (pathType == "both" && !dir.exists(parameterValue) && !file.exists(parameterValue)) {
    stop(paste0("Invalid '", parameterName, "': No directory or file was found at the specified path"))
  }
  return(invisible(TRUE))
}

#' Check File Extension
#' 
#' A helper function to validate a character string representing a file path, and check if it matches a specific file extension.
#' It ensures that the file specified has a length of 1, is a character type, is not NA or an empty character string, is a valid file at the specified path,
#' and checks if the file matches the file extension specified.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing a file path to be checked.
#' @param fileExtension A character string representing a file extension to check for.
#' @returns Invisible TRUE if the file path is valid and matches the file extension specified; otherwise returns invisible FALSE.
checkFileExtension <- function(parameterName, parameterValue, fileExtension) {
  validateCharacterString(parameterName, parameterValue)
  validateCharacterString(parameterName = "fileExtension", parameterValue = fileExtension)
  validateDirectoryFileExist(parameterName, parameterValue, pathType = "file")
  return(invisible(grepl(paste0("\\.", fileExtension, "$"), parameterValue, ignore.case = TRUE)))
}

#' Validate Directory Path Format
#' 
#' A helper function to validate and standardize the format of a character string representing a directory path. 
#' It ensures that the directory path specified has a length of 1, is a character type, is not NA or an empty character string,
#' and ends with a forward slash (/).
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing the directory path to be validated.
#' @returns A character string representing the validated and standardised directory path with a forward slash (/); otherwise, it stops with an error message.
validateDirectoryPathFormat <- function(parameterName, parameterValue) {
  validateCharacterString(parameterName, parameterValue)
  if (substring(parameterValue, nchar(parameterValue)) != "/") {
    return(paste0(parameterValue, "/"))
  }
  return(parameterValue)
}

#' Create Directory Path
#' 
#' A helper function to create a directory using a character string representing a directory path.
#' It ensures that the directory path specified has a length of 1, is a character type, is not NA or an empty character string, ends with a forward slash (/),
#' and creates the directory at the specified path.
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A character string representing a directory path to be created.
#' @returns Invisible TRUE if the directory exists or is created at the specified path; otherwise, it stops with an error message.
createDirectoryPath <- function(parameterName, parameterValue) {
  parameterValue <- validateDirectoryPathFormat(parameterName = parameterName, parameterValue = parameterValue)
  tryCatch({
    if (!dir.exists(parameterValue)) {
      dir.create(parameterValue, recursive = TRUE)
    }
    return(invisible(TRUE))
  }, 
  warning = function(w) stop(paste("Unable to create directory path:", w)), 
  error = function(e) stop(paste("Unable to create directory path:", e)))
}

#' Validate Chromatogram Region
#' 
#' A helper function to validate a list containing lists of chromatogram region data (start and end time points).
#' It ensures that the chromatogram region data specified is a list and has a valid length representing all regions of interests.
#' The function also ensures the data of each chromatogram region is also stored within a list and has a valid length representing their respective time points,
#' with each being a numeric value of length 1 (or in some, being NULL).
#' 
#' @param parameterName A character string representing the name of the parameter being validated.
#' @param parameterValue A list of lists representing the different chromatogram regions of interest and their respective start and end time points.
#' @returns Invisible TRUE if all chromatogram region data is valid; otherwise, invisible FALSE along with an error message indicating the cause of invalidity.
validateChromatogramRegion <- function(parameterName, parameterValue) {
  # Chromatogram region and time point patterns
  region <- c("prewash", "massCalibration", "analyte", "wash")
  timepoint <- c("start", "end")
  
  # Chromatogram region list
  if (length(parameterValue) !=4 || !is.list(parameterValue)) {
    message(paste0("Invalid '", parameterName, "': Must be a list containing data of 4 chromatogram regions (prewash, massCalibration, analyte, wash)"))
    return(invisible(FALSE))
  } else if (!all(region %in% names(parameterValue))) {
    message(paste0("Invalid '", parameterName, "': Missing one or more region data (", paste(region[!region %in% names(parameterValue)], collapse = ", "), ")" ))
    return(invisible(FALSE))
  }
  
  # Region data list
  for (i in region) {
    if (length(parameterValue[[i]]) != 2 || !is.list(parameterValue[[i]])) {
      message(paste0("Invalid '", i, "' region data: Must be a list containing 2 time points (start, end)"))
      return(invisible(FALSE))
    } else if (!all(timepoint %in% names(parameterValue[[i]]))) {
      message(paste0("Invalid '", i, "' region data: Missing one or more time point (", paste(timepoint[!timepoint %in% names(parameterValue[[i]])], collapse = ", "), ")"))
      return(invisible(FALSE))
    } else if (length(parameterValue[[i]][[timepoint[[1]]]]) != 1 || !is.numeric(parameterValue[[i]][[timepoint[[1]]]])) {
      message(paste("Invalid 'start' for", i, "region data: Must be a numeric value of length 1"))
      return(invisible(FALSE))
    } else if (((i == "wash" && !is.null(parameterValue[[i]][[timepoint[[2]]]])) || i != "wash") &&
               (length(parameterValue[[i]][[timepoint[[2]]]]) != 1 || !is.numeric(parameterValue[[i]][[timepoint[[2]]]]))) {
      if (i == "wash") {
        message(paste("Invalid 'end' for", i, "region data: Must either be NULL or a numeric value of length 1"))
      } else {
        message(paste("Invalid 'end' for", i, "region data: Must be a numeric value of length 1"))
      }
      return(invisible(FALSE))
    }
  }
  
  return(invisible(TRUE))
}