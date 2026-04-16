# iOrganoAssay

**iOrganoAssay** (images of Organoid Assay) is an R-based interactive 
application for connecting microscopy images with organoid assessment 
assays, including live-dead assays, immunocytochemistry (ICC), 
and drug treatment experiments.

---

## Overview

iOrganoAssay provides a structured platform to systematically manage 
and visualize large-scale organoid microscopy datasets. The App 
interface enables:

- Display of raw microscopy and segmented images
- Daily monitoring of organoid morphological metrics
  (area, perimeter, circularity)
- Longitudinal mean ± SD line plots and violin plots
- Category-based filtering by mouse line, passage(p), day(d), and microwell(W)

<img width="1854" height="1662" alt="image" src="https://github.com/user-attachments/assets/3a30750a-d5ae-497a-9a23-e8a09051203e" />

---

## Dataset

The iOrganoAssay dataset is publicly available on Zenodo:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18627307.svg)](https://doi.org/10.5281/zenodo.18627307)

The dataset includes:
- `1.Microscopy/` — 234 large-area brightfield microscopy images
- `2.Segmentation/` — AI-segmented images (AIVIA, Leica Microsystems)
- `3.Metrics/` — Morphometric data (area μm², perimeter μm, circularity)
- `0.metadata.xlsx` — Excel metafile linking all dataset components

---

## Requirements

- [R](https://www.r-project.org/) (≥ 4.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)
- R packages: `shiny`, `ggplot2`, `readxl`, `dplyr`, `tidyr`

Install required packages:
```r
install.packages(c("shiny", "ggplot2", "readxl", "dplyr", "tidyr"))
```

---

## Usage

1. Download the dataset from 
   [Zenodo](https://zenodo.org/records/18627307)
2. Copy `iOrganoAssay_App.R` to your local directory
3. Open `iOrganoAssay_App.R` in RStudio and run the script
4. In the App interface:
   - Upload the metadata file (`0.metadata.xlsx`)
   - Set paths for Microscopy, Segmentation, and Metrics folders
   - Click **"Apply"** to load images and metrics
   - Filter by mouse line, passage, day, microwell
   - Select morphometric metric and adjust plot parameters

---

## License

This project is licensed under the **MIT License** — 
see [LICENSE](LICENSE) for details.

---

## Contact

Pacian Sung-Wook Nam
📧 nams@knu.ac.kr



