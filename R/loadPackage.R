#' Load Package
#'
#' Identify, install and load packages to use.
#'
#' @export
#' @param packageName Package name: character
#'
#' @examples
#' # Example 1: Loading a single package
#' ticq::loadPackage(packageName = "DT")
#'
#' # Example 2: Loading multiple packages
#' ticq::loadPackage(packageName = c("DT", "dplyr"))
loadPackage <- function(packageName) {
  # Validate parameters
  if (is.null(packageName) || length(packageName) == 0 || !is.character(packageName) || any(is.na(packageName)) || any(packageName == "")) {
    stop("Invalid 'packageName': Must be non-empty character string of length 1 or more")
  }
  
  # Identify, install and load missing packages
  missingPackage <- packageName[!(packageName %in% installed.packages()[, "Package"])]
  if (length(missingPackage) > 0) {
    install.packages(missingPackage)
  }

  lapply(packageName, require, character.only = TRUE)
}