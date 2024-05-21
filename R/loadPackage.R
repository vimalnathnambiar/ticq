#' loadPackage
#'
#' Identify, install and load packages for usage.
#'
#' @export
#' @param packageList A vector of package names: character vector
#'
#' @examples
#' # Example 1: Loading a single package
#' ticq::loadPackage(packageList = c("DT"))
#'
#' # Example 2: Loading multiple packages
#' ticq::loadPackage(packageList = c("DT", "dplyr"))
loadPackage <- function(packageList) {
  # Identify and install missing packages
  missingPackage <- packageList[!(packageList %in% installed.packages()[, "Package"])]
  if (length(missingPackage)) {
    install.packages(missingPackage)
  }
  
  # Load packages for usage
  lapply(packageList, require, character.only = TRUE)
}