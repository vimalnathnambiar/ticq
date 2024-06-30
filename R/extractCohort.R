#' Extract Cohort
#'
#' Extract cohort name.
#'
#' Only applicable to data acquired at the Australian National Phenome Centre (ANPC).
#'
#' @import dplyr
#'
#' @param input A character vector representing ANPC-specific data file names or paths.
#' @returns A character vector of the associated cohort names.
extractCohort <- function(input) {
  return(
    dplyr::case_when(
      grepl("System_Suitability|System|SS", input) ~ "System Suitability",
      grepl("cambridge_FollowUp_Part2|cambridgeFollowUpPart2", input) ~ "Cambridge Follow Up Part 2",
      grepl("cambridge_FollowUp|cambridgeFollowUp", input) ~ "Cambridge Follow Up",
      grepl("cambridge", input) ~ "Cambridge",
      grepl("bioGuneC2|bioGune_C2", input) ~ "Biogune C2",
      grepl("bioguneExternal2|bioguneExt2", input) ~ "Biogune External 2",
      grepl("bioguneExternal|bioguneExt", input) ~ "Biogune External",
      grepl("biogune|bioGune", input) ~ "Biogune",
      grepl("harvardC2|harvard_C2", input) ~ "Harvard C2",
      grepl("harvard", input) ~ "Harvard",
      grepl("mauritius", input) ~ "Mauritius",
      grepl("healthyBankFollowUp|healthyBank_FollowUp|htybkFollowUp|htybk_FollowUp", input) ~ "Healthy Bank Follow Up",
      grepl("healthyBank|htybk", input) ~ "Healthy Bank",
      grepl("WAFollowUp|WA_FollowUp", input) ~ "WA Follow Up",
      grepl("heidelberg", input) ~ "Heidelberg",
      grepl("pathwestFollowUp", input) ~ "Pathwest Follow Up",
      grepl("pathwest", input) ~ "Pathwest",
      grepl("massGen", input) ~ "Mass Gen",
      grepl("malaysia", input) ~ "Malaysia",
      grepl("jundalup|jondalup|joondalup", input) ~ "Joondalup",
      grepl("bioMood", input) ~ "Bio Mood",
      grepl("Eggs", input) ~ "Eggs",
      grepl("fatBiopsy", input) ~ "Fat Biopsy",
      grepl("gutFlora", input) ~ "Gut Flora",
      grepl("jess", input) ~ "Jess",
      grepl("Maddison", input) ~ "Maddison",
      grepl("anderton", input) ~ "Anderton",
      grepl("goacCrypto", input) ~ "Goac Crypto",
      grepl("jayden2", input) ~ "Jayden 2",
      grepl("jayden", input) ~ "Jayden",
      grepl("maria", input) ~ "Maria",
      grepl("exercise", input) ~ "Exercise",
      grepl("DiaObesity", input) ~ "Saudi Arabia Diabetes",
      grepl("fremantle", input) ~ "Fremantle",
      grepl("umar", input) ~ "CVD Umar",
      grepl("smp1", input) ~ "Swimmers Metabolome",
      grepl("preserve", input) ~ "Preserve",
      grepl("ipf", input) ~ "IPF",
      grepl("colchicin", input) ~ "Colchicin Chile",
      grepl("C1", input) ~ "C1",
      grepl("C2", input) ~ "C2",
      grepl("C3", input) ~ "C3",
      grepl("C4", input) ~ "C4",
      grepl("C5", input) ~ "C5",
      grepl("C6", input) ~ "C6",
      grepl("C7", input) ~ "C7",
      grepl("C8", input) ~ "C8",
      TRUE ~ NA_character_
    )
  )
}