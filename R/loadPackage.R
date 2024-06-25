#' Load Package
#'
#' Identify, install, and load packages to use.
#'
#' @export
#' @param packageName A character vector representing the package names to be installed and loaded.
#' @returns A logical vector indicating whether each package specified was successfully loaded.
#' 
#' @examples
#' # Example 1: Loading a single package
#' loadPackage(packageName = "DT")
#'
#' # Example 2: Loading multiple packages
#' loadPackage(packageName = c("DT", "dplyr"))
loadPackage <- function(packageName) {
  # Validate parameters
  validateCharacterVector(parameterName = "packageName", parameterValue = packageName)
  
  # Identify and install missing packages
  missingPackage <- packageName[!(packageName %in% installed.packages()[, "Package"])]
  if (length(missingPackage) > 0) {
    install.packages(missingPackage)
  }
  
  # Load packages
  return(sapply(packageName, require, character.only = TRUE))
}