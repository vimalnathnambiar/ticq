#' loadPackage
#'
#' Load packages for usage. Automatically identifies and installs missing packages.
#'
#' @export
#' @param packageList Package names: character vector
#'
#' @examples
#' # Example 1: Load a single package
#' ticq::loadPackage(packageList = c("DT"))
#'
#' # Example 2: Load multiple packages
#' ticq::loadPackage(packageList = c("DT", "dplyr"))
loadPackage <- function(packageList) {
  # Identify and install missing packages
  missingPackage <- packageList[!(packageList %in% installed.packages()[, "Package"])]
  if (length(missingPackage)) {
    install.packages(missingPackage)
  }
  
  # Load packages
  lapply(packageList, require, character.only = TRUE)
}