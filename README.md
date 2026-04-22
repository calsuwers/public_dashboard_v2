# California Wastewater Surveillance Dashboard (v2)

This repository contains the source code for the [official California Wastewater Surveillance Dashboard](https://skylab.cdph.ca.gov/calwws/) developed by the California Department of Public Health (CDPH).

This dashboard is built using **R Shiny**, **Leaflet**, and **Plotly**, offering interactive views of wastewater data at statewide, regional, and individual sewershed levels.

> **ℹ️ Note regarding Versioning**
> This is the **updated version (v2)** of the dashboard. The previous release is archived at [calsuwers/public_dashboard_v1](https://github.com/calsuwers/public_dashboard_v1). The main change in v2 is broader pathogen coverage — **Influenza A (including H5), Influenza B, and RSV** are now included alongside SARS-CoV-2, each with its own homepage summary box, maps, and time-series plots.

## Environment & Compatibility

The `renv.lock` file was generated using **R version 4.5.2**. While the project is expected to support newer versions of R, we recommend using version 4.5.2 if you encounter any compatibility issues during `renv::restore()`.

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
├── data/
│   ├── td2_with_wval.RDS              # Site-level raw wastewater time series (all pathogens)
│   ├── saveRegionalAggregatesRPHO/
│   │   └── saveRegionalAggregates_wval_rpho.csv   # Regional-level aggregate metrics
│   └── saveReportMetricsRPHO/
│       └── saveReportMetrics.csv                   # Sewershed-level report metrics
│                                                   # (placeholder level values — see note above)
│
├── shape_file/
│   ├── CA_all_sewersheds.*            # Sewershed polygon boundaries
│   ├── saveCA_RPHOCounties.*          # County boundaries
│   └── saveCA_RPHORegions.*           # RPHO region boundaries
│
├── dashboard_update/
│   └── dashboard_update_table.csv     # Dashboard update log
│
├── www/
│   ├── analytics.js                   # Web analytics script
│   ├── cdph_logo_2024e.png            # CDPH logo
│   └── favicon.png                    # Browser tab icon
│
└── renv/                              # renv environment (do not commit renv/library/)
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

This Shiny app uses `renv` to manage package dependencies. This ensures that the exact versions of R packages used in development are also used when you run the app — no version mismatches, no missing packages.

✔️ One-time setup:

1. Open R or RStudio in the project directory (the folder where `app.R` and `renv.lock` are located).
2. Run the following commands in the R console:

```R
setwd("/path to folder where app.R is located/") # Change the working directory in R console to the folder where app.R is located
install.packages("renv")            # Only needed if you haven't installed renv yet
renv::init()                        # Initializes renv and install packages and select option 1 - Restore the project from the lockfile.
source("renv/activate.R")           # Activates the renv environment
```

This will download and install all necessary packages into a project-specific library managed by `renv`, and you only need to run `source("renv/activate.R")` once unless the `renv.lock` file changes or you delete the local renv library.

:lock: Notes:

- The `renv.lock` file is committed to this repo — it ensures reproducibility.
- The `renv/library/` folder (where packages are installed) is local to your machine and should not be committed to Git. It's listed in `.gitignore`.

### 3. Update File Paths

After cloning this repo, there are **7 file paths** in `global.R` that need to be updated to match the location where you cloned the project on your machine. All paths use the placeholder `/update_your_path/` which must be replaced with your actual local path.

Replace `/update_your_path` with the path to your cloned `dashboard_v2` folder. For example, if you cloned the repo to `/Users/yourname/Documents/`, your path would start with `/Users/yourname/Documents`.

The 7 paths to update are:

| Object created | File |
|---------------|------|
| `dash_update_data` | `dashboard_update/dashboard_update_table.csv` |
| `td2_path` → `td2` | `data/td2_with_wval.RDS` |
| `region_path` → `d1`, `f1` | `data/saveRegionalAggregatesRPHO/` (folder) |
| `report_metrics_path` → `d2`, `f2` | `data/saveReportMetricsRPHO/` (folder) |
| `shape_df` | `shape_file/CA_all_sewersheds.shp` |
| `ca_regions` | `shape_file/saveCA_RPHORegions.shp` |
| `ca_counties` | `shape_file/saveCA_RPHOCounties.shp` |

Example — if your project is located at `/Users/yourname/Documents/dashboard_v2`:

```r
# Before (placeholder):
td2_path <- "/update_your_path/dashboard_v2/data/td2_with_wval.RDS"

# After (your actual path):
td2_path <- "/Users/yourname/Documents/dashboard_v2/data/td2_with_wval.RDS"
```

Apply the same substitution to all 7 paths listed above.

### 4. Run the app

Once the environment is set up, packages are downloaded, and file paths are updated:

- Simply open `app.R` in RStudio and click **"Run App"**.

---

### :pushpin: Notes

- The shapefile used in this dashboard includes **sewershed polygons** for the Sewershed heatmap view, in addition to regional and county boundary layers.
- `renv.lock` only pins R package versions, not the system C++ libraries that `sf` wraps (GEOS, GDAL, PROJ). If `st_centroid()` or Leaflet rendering raises errors on the sewershed polygons, guard the pipeline with `sf::st_make_valid()` and `sf::st_collection_extract("POLYGON")` before computing centroids. Check system-library versions with `sf::sf_extSoftVersion()`.
- The app aims for ADA Section 508 compliance: `tags$html(lang="en")` at the page root, keyboard handlers (`onkeydown`) on clickable non-button elements, `tabindex="0"` on info boxes and legend entries, and a JavaScript post-processor injected into every Plotly chart via `htmlwidgets::onRender()` so that range-selector buttons and legend items are focusable and keyboard-activatable.

## Contact

Questions about the data or adaptation of this codebase can be directed to the Cal-SuWers team at [Wastewatersurveillance@cdph.ca.gov](mailto:Wastewatersurveillance@cdph.ca.gov).
