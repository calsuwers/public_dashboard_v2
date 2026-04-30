# California Wastewater Surveillance Dashboard (v2)

This repository contains the source code to reproduce the [official California Wastewater Surveillance Dashboard](https://skylab.cdph.ca.gov/calwws/) developed by the California Department of Public Health (CDPH).

This dashboard is built using **R Shiny**, **Leaflet**, and **Plotly**, offering interactive views of wastewater data at statewide, regional, and individual sewershed levels.

> **ℹ️ Note regarding sewershed map**
> Individual sewershed boundary shapes are not included in this public repository. The sewershed map in this codebase displays only the **approximate location** of each sewershed as an icon. For the full sewershed boundary map, visit the [official dashboard](https://skylab.cdph.ca.gov/calwws/).

> **ℹ️ Note regarding Versioning**
> This is the **updated version (v2)** of the dashboard. The previous release is archived at [calsuwers/public_dashboard_v1](https://github.com/calsuwers/public_dashboard_v1). The main change in v2 is broader pathogen coverage — **Influenza A (including H5), Influenza B, and RSV** are now included alongside SARS-CoV-2, each with its own homepage summary box, maps, and time-series plots.

## Environment & Compatibility

The `renv.lock` file was generated using **R version 4.5.2**. While the project is expected to support newer versions of R, we recommend using version 4.5.2 if you encounter any compatibility issues installing packages.

---

## Pathogen Data Coverage

The dashboard supports the following pathogens:

- **SARS-CoV-2 (COVID-19)**
- **Influenza A (Flu A)** — including the H5 subtype as a toggleable overlay
- **Influenza B (Flu B)**
- **Respiratory Syncytial Virus (RSV)**

> **⚠️ Note regarding sewershed-level metrics**
> For all pathogens, per-sewershed **level** metrics (e.g., Very Low, Low, Moderate, High)
> are **not yet based on real data** at the sewershed level. The wastewater viral activity
> level (WVAL) methodology has not yet been finalized for individual sewersheds, so the
> current dataset (`saveReportMetrics.csv`) uses placeholder values — all
> sewershed threshold columns (q1–q4) are set to 1 and level is set to "Very Low" — to
> avoid displaying misleading metrics.
>
> **Trend metrics, however, are fully available for each sewershed** and accurately reflect
> the 21-day directional trend in wastewater concentrations. Users can rely on the trend
> information displayed on the Sewershed tab. Level metrics will be updated once the
> WVAL-based approach is finalized and validated at the sewershed level.

---

## File structure

```
├── app.R                              # Minimal entry point (calls shinyApp(ui, server))
├── global.R                           # Loads packages and data, defines global objects
├── server.R                           # Server logic (reactives, plots, maps, tables)
├── ui.R                               # User interface layout (dashboardPage)
├── slim_td2.R                         # One-off utility: slims td2_with_wval.RDS to required columns
├── dashboard_v2.Rproj                 # RStudio project file
├── renv.lock                          # Pinned package versions for renv
├── README.md                          # This file
│
├── R/
│   └── functions.R                    # Custom helper functions (sourced by global.R)
│
├── data/                              # NOT included in this repo — provide your own copies (see Step 3 below)
│   ├── td2_with_wval.RDS              # Site-level raw wastewater time series (all pathogens)
│   ├── saveRegionalAggregatesRPHO/
│   │   └── saveRegionalAggregates_wval_rpho.csv   # Regional-level aggregate metrics
│   └── saveReportMetricsRPHO/
│       └── saveReportMetrics.csv                   # Sewershed-level report metrics
│                                                   # (placeholder level values — see note above)
│
├── shape_file/
│   ├── CA_all_sewersheds_centroids.csv  # Sewershed approximate icon locations (lat/lng centroids only; polygon boundaries not shared)
│   ├── saveCA_RPHOCounties.*            # County boundaries
│   └── saveCA_RPHORegions.*             # RPHO region boundaries
│
├── dashboard_update/
│   └── dashboard_update_table.csv     # Dashboard update log
│
└── www/
    ├── analytics.js                   # Web analytics script
    ├── cdph_logo_2024e.png            # CDPH logo
    └── favicon.png                    # Browser tab icon
```

Each R file starts with a commented section outline. Section headers use
the `# N. TITLE ----` convention so that RStudio's document outline
(Ctrl/Cmd+Shift+O) and jump-to-section dropdown pick them up automatically.

## Setup Instructions

Before proceeding with the steps below, please ensure that Git is installed on your computer.
You can find installation instructions here: https://github.com/git-guides/install-git

To verify that Git is properly installed, open your terminal and type:

```bash
git --version
```

### 1. Clone the Repository

In the terminal, navigate to the directory where you'd like to clone this repository and then run the following command.

```bash
git clone https://github.com/calsuwers/public_dashboard_v2.git
```

### 2. Install Required R Packages

This Shiny app uses `renv` to manage package dependencies. The `renv.lock` file pins the exact package versions used in development — you do not need the `renv/` folder itself.

✔️ One-time setup:

1. Open R or RStudio in the project directory (the folder where `app.R` and `renv.lock` are located).
2. Run the following commands in the R console:

```R
setwd("/path to folder where app.R is located/") # Change the working directory in R console to the folder where app.R is located
install.packages("renv")            # Only needed if you haven't installed renv yet
renv::restore()                     # Installs all packages at the versions listed in renv.lock
```

This will download and install all necessary packages into a project-specific library. You only need to run this once, unless the `renv.lock` file changes.

:lock: Note:

- The `renv.lock` file is committed to this repo — it ensures reproducibility across machines.

### 3. Update File Paths

Most file paths in `global.R` use relative paths and will work automatically after cloning. The only paths you need to update are the three data file paths near the top of `global.R`:

```r
td2_path           <- "/path/to/your/data/td2_with_wval.RDS"
region_path        <- "/path/to/your/data/saveRegionalAggregatesRPHO/"
report_metrics_path <- "/path/to/your/data/saveReportMetricsRPHO/"
```

Replace `/path/to/your/data/` with the actual folder on your machine where you have stored the data files.

### 4. Run the app

Once the environment is set up, packages are downloaded, and file paths are updated:

- Simply open `app.R` in RStudio and click **"Run App"**.

---

### :pushpin: Notes

- The `shape_file/` folder includes regional and county boundary shapefiles used for the Region map polygons. Sewershed polygon boundaries are **not included** — the Sewershed map instead uses `CA_all_sewersheds_centroids.csv` to place an approximate icon for each sewershed.
- `renv.lock` pins R package versions only, not the system C++ libraries that `sf` wraps (GEOS, GDAL, PROJ). Check system-library versions with `sf::sf_extSoftVersion()`.
- The app aims for ADA Section 508 compliance: `tags$html(lang="en")` at the page root, keyboard handlers (`onkeydown`) on clickable non-button elements, `tabindex="0"` on info boxes and legend entries, and a JavaScript post-processor injected into every Plotly chart via `htmlwidgets::onRender()` so that range-selector buttons and legend items are focusable and keyboard-activatable.

## Contact

Questions about the data or adaptation of this codebase can be directed to the Cal-SuWers team at [Wastewatersurveillance@cdph.ca.gov](mailto:Wastewatersurveillance@cdph.ca.gov).
