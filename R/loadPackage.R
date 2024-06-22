#' Load Package
#'
#' Identify, install, and load packages to use.
#'
#' @export
#' @param packageName A character vector representing the package names to be installed and loaded. Should not contain NA or empty character strings.
#' @returns A logical vector indicating whether each package specified was successfully loaded.
#' 
#' @examples
#' # Example 1: Loading a single package
#' ticq::loadPackage(packageName = "DT")
#'
#' # Example 2: Loading multiple packages
#' ticq::loadPackage(packageName = c("DT", "dplyr"))
loadPackage <- function(packageName) {
  # Validate parameters
  validateCharacterVector(parameterName = "packageName", parameterValue = packageName)
  
  # Identify, install and load missing packages
  missingPackage <- packageName[!(packageName %in% installed.packages()[, "Package"])]
  if (length(missingPackage) > 0) {
    install.packages(missingPackage)
  }
  
  return(sapply(packageName, require, character.only = TRUE))
}