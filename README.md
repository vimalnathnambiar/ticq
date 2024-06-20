# TICQ: Total Ion Current Quality Assessment of Mass Spectrometry Spectral Data

## Introduction

TICQ is a R package that allows you to quickly assess mass spectrometry (MS) spectral data for sample quality and instrument performance using total ion current (TIC).

<br>

## Features

1. Total Ion Current

   - Total Ion Chromatogram

   - Statistical Analysis

     - Principal Component Analysis

       - Mass calibration, analyte and wash region

     - Time series analysis

       - Prewash region = Mass calibration region + Analyte region

     - Violin/Box plot analysis

       - Mass calibration
       - Analyte

2. Base Peak Intensity

   - Base Peak Chromatogram

3. Extracted Ion Current (Targeted)

   - Mass calibration references, internal standards, endogenous analytes and products

     - Chromatogram

     - Statistical Analysis

       - Time series
       - Mass accuracy

<br>

## Installation

```r
install.packages("remotes")
remotes::install_github("vmalnathnambiar/ticq")
```

## Usage

The package primarily works in relation with [ExfilMS], specifically for its functionality in extracting and filtering MS spectral data into a standardised JSON format.

```r
ticq::renderTICQ(inputPath = "/path/to/input/directory/or/file.json", ...)
```

<br>

## Limitations

<br>

## Citations

If you use TICQ in your work, please cite it using the following:

Nambiar, V., & Nambiar, S. (2024). TICQ: Total Ion Current Quality Assessment of Mass Spectrometry Spectral Data (Version 0.0.0) [Computer software]. https://doi.org/10.5281/zenodo

<br>

## License

Please refer to our license information [here](./LICENSE).

<!-- URLs used in the markdown document -->

[exfilms]: https://github.com/vmalnathnambiar/exfilms
