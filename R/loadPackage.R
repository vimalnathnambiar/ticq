#' Load Package
#'
#' Identify, install and load packages to use.
#'
#' @export
#' @param packageName Package names to be loaded: character vector
#'
#' @examples
#' # Example 1: Loading a single package
#' ticq::loadPackage(packageName = "DT")
#'
#' # Example 2: Loading multiple packages
#' ticq::loadPackage(packageName = c("DT", "dplyr"))
loadPackage <- function(packageName) {
  # Identify and install missing packages
  missingPackage <- packageName[!(packageName %in% installed.packages()[, "Package"])]
  if (length(missingPackage) > 0) {
    install.packages(missingPackage)
  }
  
  # Load packages
  lapply(packageName, require, character.only = TRUE)
}