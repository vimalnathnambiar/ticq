# Load package
library(roxygen2)
library(devtools)
roxygen2::roxygenise(getwd())
devtools::load_all(getwd())
ticq::loadPackage(packageList = c("rmarkdown"))

# Input data (Path to directory or JSON file to be processed)
input <- "/run/media/vimalnathnambiar/WD/data/JSON/controlSamples/barwin20_C1_URI_MS-AA_PAI04_BARWINp13_041021_CAL01_108.json"

# Extract metadata (if JSON file)
metadata <- ticq::extractMetadata(input = input)
# metadata$method <- "MS-AA-POS"

# Configure parameters (Dynamically based on input metadata - ANPC specific)
  # Define output path
  outputDir <- paste0(getwd(), "/report/")
  
  # Chromatogram region
  if (metadata$method == "MS-AA-POS") {
    chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.3, 
                                                            washStart = 5)
  } else if (metadata$method == "MS-HIL-POS" | metadata$method == "MS-HIL-NEG" | metadata$method == "MS-RP-POS" | metadata$method == "MS-RP-NEG") {
    chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = 0, massCalEnd = 0.7,
                                                            washStart = 11)
  } else {
    chromatogramRegion <- NULL
  }

# Render TICQ R Markdown script
rmarkdown::render(input = paste0(getwd(), "/nb/ticq.Rmd"),
                  output_format = "html_document",
                  output_dir = outputDir,
                  output_file = paste0(outputDir, gsub("\\.json$", "", basename(input), ignore.case = TRUE), ".html"),
                  params = list(input = input,
                                metadata = metadata,
                                chromatogramRegion = chromatogramRegion,
                                targetFile = NULL,
                                anpcMethodLibrary = metadata$method,
                                roundDecimal = 4,
                                anonymiserStatus = FALSE))
