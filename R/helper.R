#' Validate Character Vector
#' 
#' Validate that a vector has a length greater than 0 and is a character type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character vector.
#' @returns Invisible TRUE if the character vector is valid; otherwise, it stops with an error message.
validateCharacterVector <- function(name, value) {
  if (length(value) == 0 || !is.character(value)) {
    stop(paste0("Invalid '", name, "': Must be a character vector with a minimum length of 1"))
  }
  return(invisible(TRUE))
}

#' Validate Character Vector Element
#' 
#' Validate that a vector has a length greater than 0, is a character type, and does not contain any NA or empty string elements.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character vector.
#' @returns Invisible TRUE if the character vector is valid; otherwise, it stops with an error message.
validateCharacterVectorElement <- function(name, value) {
  if (length(value) == 0 || !is.character(value) || any(is.na(value)) || any(value == "")) {
    stop(paste0("Invalid '", name, "': Must be a character vector with a minimum length of 1 and does not contain any NA or empty string elements"))
  }
  return(invisible(TRUE))
}

#' Validate Character String
#' 
#' Validate that a string has a length of 1 and is a character type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string.
#' @returns Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
validateCharacterString <- function(name, value) {
  if (length(value) != 1 || !is.character(value)) {
    stop(paste0("Invalid '", name, "': Must be a character string of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Nullable Character String
#' 
#' Validate that a string is either NULL or has a length of 1 and is a character type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string.
#' @returns Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
validateNullableCharacterString <- function(name, value) {
  if (!is.null(value) && (length(value) != 1 || !is.character(value))) {
    stop(paste0("Invalid '", name, "': Must either be NULL or a character string of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Character String Value
#' 
#' Validate that a string has a length of 1, is a character type, and is not NA or empty.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string.
#' @returns Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
validateCharacterStringValue <- function(name, value) {
  if (length(value) != 1 || !is.character(value) || is.na(value) || value == "") {
    stop(paste0("Invalid '", name, "': Must be a non-NA, non-empty character string of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Nullable Character String Value
#' 
#' Validate that a string is either NULL or has a length of 1, is a character type, and is not NA or empty.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string.
#' @returns Invisible TRUE if the character string is valid; otherwise, it stops with an error message.
validateNullableCharacterStringValue <- function(name, value) {
  if (!is.null(value) && (length(value) != 1 || !is.character(value) || is.na(value) || value == "")) {
    stop(paste0("Invalid '", name, "': Must either be NULL or a non-NA, non-empty character string of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Numeric Value
#' 
#' Validate that a value has a length of 1 and is a numeric type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric value.
#' @returns Invisible TRUE if the numeric value is valid; otherwise, it stops with an error message.
validateNumericValue <- function(name, value) {
  if (length(value) != 1 || !is.numeric(value)) {
    stop(paste0("Invalid '", name, "': Must be a numeric value of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Nullable Numeric Value
#' 
#' Validate that a value is either NULL or has a length of 1 and is a numeric type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric value.
#' @returns Invisible TRUE if the numeric value is valid; otherwise, it stops with an error message.
validateNullableNumericValue <- function(name, value) {
  if (!is.null(value) && (length(value) != 1 || !is.numeric(value))) {
    stop(paste0("Invalid '", name, "': Must either be NULL or a numeric value of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Missing Numeric Value
#' 
#' Validate that a value is either NA (missing) or has a length of 1 and is a numeric type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric value.
#' @returns Invisible TRUE if the numeric value is valid; otherwise, it stops with an error message.
validateMissingNumericValue <- function(name, value) {
  if (!is.na(value) && (length(value) != 1 || !is.numeric(value))) {
    stop(paste0("Invalid '", name, "': Must either be NA or a numeric value of length 1"))
  }
  return(invisible(TRUE))
}

#' Validate Numeric Vector
#' 
#' Validate that a vector has a length greater than 0 and is a numeric type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric vector.
#' @returns Invisible TRUE if the numeric vector is valid; otherwise, it stops with an error message.
validateNumericVector <- function(name, value) {
  if (length(value) == 0 || !is.numeric(value)) {
    stop(paste0("Invalid '", name, "': Must be a numeric vector with a minimum length of 1"))
  }
  return(invisible(TRUE))
}

#' Validate Numeric Vector Element
#' 
#' Validate that a vector has a length greater than 0, is a numeric type, and does not contain any NA elements.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric vector.
#' @returns Invisible TRUE if the numeric vector is valid; otherwise, it stops with an error message.
validateNumericVectorElement <- function(name, value) {
  if (length(value) == 0 || !is.numeric(value) || any(is.na(value))) {
    stop(paste0("Invalid '", name, "': Must be a numeric vector with a minimum length of 1 and does not contain any NA elements"))
  }
  return(invisible(TRUE))
}

#' Validate Statistical Numeric Vector Element
#' 
#' Validate that a vector has a length greater than 1, is a numeric type, and does not contain any NA elements.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A numeric vector.
#' @returns Invisible TRUE if the numeric vector is valid; otherwise, it stops with an error message.
validateStatisticalNumericVectorElement <- function(name, value) {
  if (length(value) < 2 || !is.numeric(value) || any(is.na(value))) {
    stop(paste0("Invalid '", name, "': Must be a numeric vector with a minimum length of 2 and does not contain any NA elements"))
  }
  return(invisible(TRUE))
}

#' Validate Logical Value
#' 
#' Validate that a value has a length of 1 and is a logical type, and is not NA.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A logical value.
#' @returns Invisible TRUE if the logical value is valid; otherwise, it stops with an error message.
validateLogicalValue <- function(name, value) {
  if (length(value) != 1 || !is.logical(value) || is.na(value)) {
    stop(paste0("Invalid '", name, "': Must be a non-NA logical value of length 1 (TRUE or FALSE)"))
  }
  return(invisible(TRUE))
}

#' Validate Directory and File Path
#' 
#' Validate that a character string representing a directory or file path exists based on a specific path type check (directory, file, or both).
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string representing either a directory or file path.
#' @param pathType A character string representing the path type to check for. (Default: `"both"`; Options: `"directory"`, `"file"`, or `"both"`)
#' @returns Invisible TRUE if the directory or file path exists; otherwise, it stops with an error message.
validateDirectoryFileExist <- function(name, value, pathType = "both") {
  # Validate parameters
  validateCharacterStringValue(name = name, value = value)
  if (!pathType %in% c("directory", "file", "both")) {
    stop("Invalid 'pathType': Must either be 'directory', 'file', or 'both'")
  } 
  
  # Validate directory / file existence
  if (pathType == "directory" && !dir.exists(value)) {
    stop(paste0("Invalid '", name, "': No directory was found at the specified path"))
  } else if (pathType == "file" && !file.exists(value)) {
    stop(paste0("Invalid '", name, "': No file was found at the specified path"))
  } else if (pathType == "both" && !dir.exists(value) && !file.exists(value)) {
    stop(paste0("Invalid '", name, "': No directory or file was found at the specified path"))
  }
  return(invisible(TRUE))
}

#' Validate File Extension Status
#' 
#' Validate that a character string representing a file path matches a specific file extension pattern.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string representing a file path.
#' @param fileExtension A character string representing a file extension pattern.
#' @returns Invisible TRUE if the file matches the specified file extension pattern; otherwise, it returns invisible FALSE.
validateFileExtensionStatus <- function(name, value, fileExtension) {
  # Validate parameters
  validateDirectoryFileExist(name = name, value = value, pathType = "file")
  validateCharacterStringValue(name = "fileExtension", value = fileExtension)
  
  # Validate file extension pattern
  return(invisible(grepl(paste0("\\.", fileExtension, "$"), value, ignore.case = TRUE)))
}

#' Format Directory Path
#' 
#' Validate that a character string representing a directory path ends with a forward slash (/).
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string representing a directory path.
#' @returns A character string of the validated directory path with a forward slash (/); otherwise, it stops with an error message.
formatDirectoryPath <- function(name, value) {
  # Validate parameters
  validateCharacterStringValue(name = name, value = value)
  
  # Format directory path
  return(if (substring(value, nchar(value)) != "/") paste0(value, "/") else value)
}

#' Create Directory Path
#' 
#' Validate and create a directory based on a character string representing a directory path.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A character string representing a directory path.
#' @returns Invisible TRUE if the directory exist or is successfully created at the specified path; otherwise, it stops with an error message.
createDirectoryPath <- function(name, value) {
  # Validate parameters
  value <- formatDirectoryPath(name = name, value = value)
  
  # Create directory
  tryCatch({
    if (!dir.exists(value)) {
      dir.create(value, recursive = TRUE)
    }
    return(invisible(TRUE))
  }, 
  warning = function(w) stop(paste("Unable to create directory path:", w)), 
  error = function(e) stop(paste("Unable to create directory path:", e)))
}

#' Validate Chromatogram Region
#' 
#' Validate that a list representing the chromatogram region data of interests (prewash, mass calibration, analyte, and wash) has a length of 4;
#' that each region of interest is also a list and has a length of 2 representing values of its respective time points (start and end);
#' and that each time point value has a length of 1 and is a numeric type (except 'washEnd' with the acceptance of a NULL value).
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A list representing the chromatogram region data of interests.
#' @returns Invisible TRUE if the data of the chromatogram regions of interest are valid; otherwise, invisible FALSE along with an error message.
validateChromatogramRegion <- function(name, value) {
  # Chromatogram region data patterns
  region <- c("prewash", "massCalibration", "analyte", "wash")
  timepoint <- c("start", "end")
  
  # Validate chromatogram region data
  if (length(value) !=4 || !is.list(value)) {
    message(paste0("Invalid '", name, "': Must be a list of 4 chromatogram region data of interests (prewash, massCalibration, analyte, wash)"))
    return(invisible(FALSE))
  } else if (!all(region %in% names(value))) {
    message(paste0("Invalid '", name, "': Missing one or more region (", paste(region[!region %in% names(value)], collapse = ", "), ")" ))
    return(invisible(FALSE))
  }
  
  for (i in region) {
    if (length(value[[i]]) != 2 || !is.list(value[[i]])) {
      message(paste0("Invalid '", i, "' region: Must be a list of 2 time points (start, end)"))
      return(invisible(FALSE))
    } else if (!all(timepoint %in% names(value[[i]]))) {
      message(paste0(
        "Invalid '", i, "' region: Missing one or more time point (", paste(timepoint[!timepoint %in% names(value[[i]])], collapse = ", "), ")"
      ))
      return(invisible(FALSE))
    }
    
    if (length(value[[i]][[timepoint[[1]]]]) != 1 || !is.numeric(value[[i]][[timepoint[[1]]]])) {
      message(paste0("Invalid '", timepoint[[1]], "' for ", i, " region: Must be a numeric value of length 1"))
      return(invisible(FALSE))
    }
    
    if ((i != "wash" || (i == "wash" && !is.null(value[[i]][[timepoint[[2]]]]))) &&
        (length(value[[i]][[timepoint[[2]]]]) != 1 || !is.numeric(value[[i]][[timepoint[[2]]]]))) {
      if (i == "wash") {
        message(paste0("Invalid '", timepoint[[2]], "' for ", i, " region: Must either be NULL or a numeric value of length 1"))
      } else {
        message(paste0("Invalid '", timepoint[[2]], "' for ", i, " region: Must be a numeric value of length 1"))
      }
      return(invisible(FALSE))
    }
  }
  
  return(invisible(TRUE))
}

#' Validate Region of Interest
#' 
#' Validate that a list representing the time points of a chromatogram region of interest has a length of 2 (start and end) and
#' that each time point value is either NULL or has a length of 1 and is a numeric type.
#' 
#' @param name A character string representing the name of the object being validated.
#' @param value A list representing the start and end time points of a chromatogram region of interest.
#' @returns Invisible TRUE if data of the chromatogram region of interest is valid; otherwise, invisible FALSE along with an error message.
validateRegionOfInterest <- function(name, value) {
  # Time point patterns
  timepoint <- c("start", "end")
  
  # Validate chromatogram region data
  if (length(value) != 2 || !is.list(value)) {
    message(paste0("Invalid '", name, "': Must be a list of 2 time points (start, end)"))
    return(invisible(FALSE))
  } else if (!all(timepoint %in% names(value))) {
    message(paste0(
      "Invalid '", name, "': Missing one or more time point (", paste(timepoint[!timepoint %in% names(value)], collapse = ", "), ")"
    ))
    return(invisible(FALSE))
  }
  
  for (i in timepoint) {
    if (!is.null(value[[i]]) && (length(value[[i]]) != 1 || !is.numeric(value[[i]]))) {
      message(paste0("Invalid '", i, "' for ", name, ": Must either be NULL or a numeric value of length 1"))
      return(invisible(FALSE))
    }
  }
  
  return(invisible(TRUE))
}