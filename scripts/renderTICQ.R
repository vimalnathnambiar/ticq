# Load package
ticq::loadPackage(packageList = c("rmarkdown"))

# Input data (Path to directory or JSON file to be processed)
input <- "/path/to/input/directory/or/file.json"

# Extract metadata (if JSON file)
metadata <- ticq::extractMetadata(input = input)
# metadata$method <- "MS-AA-POS"

# Configure parameters (Dynamically based on input metadata - ANPC specific)
  # Define output path
  outputDir <- paste0(getwd(), "/report/")
  
  # Chromatogram region
  timepoint <- list("MS-AA-POS" = list(massCalStart = 0, massCalEnd = 0.3, washStart = 5),
                    "MS-HIL-POS" = list(massCalStart = 0, massCalEnd = 0.7, washStart = 11),
                    "MS-HIL-NEG" = list(massCalStart = 0, massCalEnd = 0.7, washStart = 11),
                    "MS-RP-POS" = list(massCalStart = 0, massCalEnd = 0.7, washStart = 11),
                    "MS-RP-NEG" = list(massCalStart = 0, massCalEnd = 0.7, washStart = 11))
  if (!is.na(metadata$method) && metadata$method %in% names(timepoint)) {
    timepoint <- timepoint[[metadata$method]]
    chromatogramRegion <- ticq::configureChromatogramRegion(massCalStart = timepoint$massCalStart,
                                                            massCalEnd = timepoint$massCalEnd,
                                                            washStart = timepoint$washStart)
  } else {
    chromatogramRegion <- NULL
    metadata$method <- NULL
  }


# Render TICQ R Markdown script
rmarkdown::render(input = paste0(getwd(), "/notebook/ticq.Rmd"),
                  output_format = "html_document",
                  output_dir = outputDir,
                  output_file = paste0(outputDir, 
                                       gsub("\\.json$", "", basename(input), ignore.case = TRUE), 
                                       ".html"),
                  params = list(input = input,
                                chromatogramRegion = chromatogramRegion,
                                targetFile = NULL,
                                anpcMethodLibrary = metadata$method,
                                roundDecimal = 4,
                                anonymiserStatus = FALSE))

# Clean environment
rm(metadata, outputDir, timepoint)
