#' loadPackage
#'
#' Identify (and install missing) and load packages to use.
#'
#' @export
#' @param packageList Package names to be loaded: character vector
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
  
  # Load packages
  lapply(packageList, require, character.only = TRUE)
}