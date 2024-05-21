#' extractProject
#'
#' Extract project name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @export
#' @param input Input to extract project name from: character
#' @returns Project name
#'
#' @examples
#' project <- ticq::extractProject(input = "covid19_heidelberg_SER_MS-AA_PAI05_COVp88_261121_QC04_29.json")
extractProject <- function(input) {
  project <- dplyr::case_when(
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
    # grepl("HIL", input) ~ "HILIC",
    # grepl("RP", input) ~ "Reversed Phase",
    grepl("intersalt|IST", input) ~ "Intersalt",
    grepl("studentProjects|STD", input) ~ "Student Projects",
    TRUE ~ NA_character_
  )
  
  return(project)
}