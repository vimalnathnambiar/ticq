#' Extract Project
#'
#' Extract project name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @returns A character vector of the associated project names.
extractProject <- function(input) {
  return(
    dplyr::case_when(
      grepl("covid19|covid|COV", input) ~ "COVID-19",
      grepl("barwon|barwin20|BARWIN|BAR", input) ~ "Barwon",
      grepl("coeliac|COE", input) ~ "Coeliac",
      grepl("covvac|VAX", input) ~ "COVVAC",
      grepl("cabin|CABIN", input) ~ "CABIN",
      grepl("mouseflu|tkiMouseFlu|MOUSE_FLU", input) ~ "Mouse Flu",
      grepl("prolifica|PRO", input) ~ "Prolifica",
      grepl("omniheart|OIH", input) ~ "Omniheart",
      grepl("rocit|ROCIT", input) ~ "ROCIT",
      grepl("parkinsons|PKS", input) ~ "Parkinsons",
      grepl("quality|QCC", input) ~ "Quality",
      grepl("gemma|GMA", input) ~ "Gemma",
      grepl("biolog", input) ~ "Biolog",
      grepl("myositis", input) ~ "Myositis",
      grepl("busselton|BHAS", input) ~ "Busselton",
      grepl("microbiome", input) ~ "Microbiome",
      grepl("ipac|IPAC", input) ~ "IPAC",
      grepl("hims|HIM", input) ~ "HIMS",
      grepl("cki|CKI", input) ~ "CKI",
      grepl("mums|MUM", input) ~ "MUMS",
      grepl("pediatricBurns|PBU", input) ~ "Pediatric Burns",
      grepl("penang|penan", input) ~ "Penang",
      grepl("fruits", input) ~ "Fruits",
      grepl("cystic|PRE", input) ~ "Systic Fibrosis",
      grepl("diabetes|DWA", input) ~ "Diabetes",
      grepl("strep|MNA", input) ~ "STREP",
      grepl("hbk|HBK", input) ~ "Normal Control Bank",
      grepl("comet|COM", input) ~ "Comet Toxicology Project",
      grepl("performance|HPP", input) ~ "Human Performance Project",
      grepl("bioreactor|BRX", input) ~ "Bioreactor",
      grepl("intersalt|IST", input) ~ "Intersalt",
      grepl("studentProjects|STD", input) ~ "Student Projects",
      TRUE ~ NA_character_
    )
  )
}