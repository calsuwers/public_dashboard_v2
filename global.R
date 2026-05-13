# =============================================================================
# FILE: global.R
# PROJECT: Cal-SuWers Public Dashboard v2
# DESCRIPTION:
#   Loaded automatically by Shiny before ui.R and server.R. Contains:
#     - Library imports
#     - Sourcing of custom helper functions (R/functions.R)
#     - Color palettes and display constants
#     - Data file paths and data loading
#     - Data wrangling to produce analysis-ready datasets
#     - UI display vectors (region_choice, js_code_plot, etc.)
#
# NOTE ON FILE PATHS:
#   Update the variables in the "File Paths" section below to point to
#   your local copies of the input data files before running the app.
#
# SECTIONS:
#   1. Libraries
#   2. Source Custom Functions
#   3. Color Palettes & Map Settings
#   4. Display Constants & Reference Tables
#   5. File Paths (UPDATE THESE)
#   6. Data Loading
#   7. Data Wrangling
#   8. Summary / Download Tables
#   9. State-Level Summaries
#  10. UI Globals (region vectors, JS accessibility code)
# =============================================================================

# =============================================================================
# 1. LIBRARIES ----
# =============================================================================
# Load necessary libraries
library(bslib)
library(cowplot)
library(DBI)
library(data.table)
library(dbplyr)
library(dplyr)
library(DT)
library(dygraphs)
library(ggmap)
library(ggpattern)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(kableExtra)
library(knitr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(magrittr)
library(maps)
library(mapproj)
library(plotly)
library(purrr)
library(RColorBrewer)
library(RCurl)
library(readr)
library(readxl)
library(rlang)
library(rintrojs)
library(scales)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(sf)
library(stringr)
library(tidyr)
library(xts)
library(zoo)


# =============================================================================
# 2. SOURCE CUSTOM FUNCTIONS ----
# All helper functions are defined in R/functions.R and sourced here so they
# are available globally to both ui.R and server.R.
# =============================================================================
source("R/functions.R")

# =============================================================================
# 3. COLOR PALETTES & MAP SETTINGS ----
# These named vectors define the fill colors used on the Leaflet heatmaps.
# Each pathogen shares the same 5-level color ramp (Very Low -> Very High).
# The _transparent variants (80% opacity) are used for overlapping polygons.
# =============================================================================

covid_map_colors <- c(
  "Very Low" = "#BAE8DE",
  "Low" = "#B8E5AC",
  "Moderate" = "#FEA82F",
  "High" = "#F45B53",
  "Very High" = "#C15C9C",
  "Not enough data" = "#969696"
)


covid_map_colors_transparent <- c(
  "Very Low" = "#BAE8DE80",
  "Low" = "#B8E5AC80",
  "Moderate" = "#FEA82F80",
  "High" = "#F45B5380",
  "Very High" = "#C15C9C80",
  "Not enough data" = "#96969680"
)

state_threshold_colors = list(
  n = covid_map_colors,
  infa = covid_map_colors,
  infb = covid_map_colors,
  rsv = covid_map_colors
)

sewershed_threshold_colors = lapply(state_threshold_colors, function(x) {
  x[] <- "#d4ac77"
  return(x)
})

state_threshold_colors_transparent = list(
  n = covid_map_colors_transparent,
  infa = covid_map_colors_transparent,
  infb = covid_map_colors_transparent,
  rsv = covid_map_colors_transparent
)

# =============================================================================
# 4. DISPLAY CONSTANTS & REFERENCE TABLES ----
# zoomlist      : Per-region center lat/lng/zoom for the sewershed map.
# target_choice : Ordered list of pathogens shown in the pathogen picker.
# levels_data   : Reference table rendered on the Technical Notes page —
#                 shows the WVAL thresholds that define each level category.
# trends_data   : Reference table mapping 21-day % change to a trend label
#                 and a symbol. `Trend Symbol` is filled in below using
#                 trend_symbol() from R/functions.R.
# upper_y_plot_limit / low_base_value: Plot layout tunables.
# =============================================================================

zoomlist = list(
  "State" = c(-120.3384, 37.06523, 6),
  "ABAHO" = c(-122.2384, 37.70023, 8.4),   # Lower the latitude slightly and adjust zoom
  "RANCHO" = c(-123.1403, 40.80435, 7),    # Keep the same
  "SOCAL" = c(-118.249, 34.97365, 7),      # Keep the same
  "LA" = c(-118.249, 34.97365, 7),      # Keep the same
  "SACRAMENTO" = c(-121.33, 39.6, 8),  
  "SJVC" = c(-120.51, 36.88, 7.8)           # Lower the latitude slightly and adjust zoom
)

target_choice = c("SARS-CoV-2", "Influenza A", "Influenza B", "RSV", "Influenza A (H5)")

levels_data <- data.frame(
  Viruses = c("COVID-19", "Influenza A and B", "RSV"),
  `Very Low` = c("Up to 2.5", "Up to 0.9", "Up to 0.7"),
  Low = c("Greater than 2.5 and up to 9.5", "Greater than 0.9 and up to 7", "Greater than 0.7 and up to 5.4"),
  Moderate = c("Greater than 9.5 and up to 16.5", "Greater than 7 and up to 13", "Greater than 5.4 and up to 10"),
  High = c("Greater than 16.5 and up to 23.4", "Greater than 13 and up to 19", "Greater than 10 and up to 14.7"),
  `Very High` = c("Greater than 23.4", "Greater than 19", "Greater than 14.7"),
  `Not enough data` = c(
    "No samples collected in the past 14 days",
    "No samples collected in the past 14 days",
    "No samples collected in the past 14 days"
  ),
  check.names = FALSE
)

trends_data <- data.frame(
  `21-day Percent Change Estimate` = c(
    "-21% to -99%",
    "-20% to 20%",
    "21% to 99%",
    "100% to 249%",
    "Greater than 250%",
    "Not enough data in the past 21 days*",
    "More than 1 of the past 5 samples below LOD",
    "All 5 of the past 5 samples below LOD"
  ),
  `Trend Category` = c(
    "Decrease",
    "Plateau",
    "Increase",
    "Strong Increase",
    "Very Strong Increase",
    "Not enough data",
    "Sporadic Detections",
    "All Samples Below LOD"
  )
)

trends_data$`Trend Symbol` <- sapply(trends_data$Trend.Category, trend_symbol)
upper_y_plot_limit = 1.2
low_base_value = 50

# =============================================================================
# 5. FILE PATHS  *** UPDATE THESE PATHS BEFORE RUNNING *** ----
# Replace each placeholder string with the real path to your data files.
# =============================================================================

dash_update_data <-
  read.csv("dashboard_update/dashboard_update_table.csv") %>%
  arrange(desc(date))

data_dir            <- "/path/to/your/data/"
td2_path            <- paste0(data_dir, "td2_with_wval.RDS")
region_path         <- paste0(data_dir, "saveRegionalAggregatesRPHO/")
report_metrics_path <- paste0(data_dir, "saveReportMetricsRPHO/")

# =============================================================================
# 6. DATA LOADING ----
# Reads RDS and CSV files from the paths defined above.
# - td2      : Site-level raw wastewater time series (all pathogens)
# - shape_df : Sewershed polygon shapefile (centroid lng/lat computed here)
# - ca_regions / ca_counties: RPHO region and county shapefiles
# - d1, d2  : COVID (SARS-CoV-2) regional aggregates and site metrics
# - f1, f2  : Flu A/B, RSV regional aggregates and site metrics
# =============================================================================
td2 = readRDS(strwrap(td2_path)) %>%
  ungroup() %>% 
  mutate(wwtp_name =  gsub(" $", "", wwtp_name),
         Label_Name = gsub(" $", "", Label_Name)) %>%
  mutate(region = case_when(
    rpho_region == "Greater Sierra Sacramento" ~ "SACRAMENTO",
    rpho_region == "Bay Area" ~ "ABAHO",
    rpho_region == "Southern California" ~ "SOCAL",
    rpho_region == "Central California" ~ "SJVC",
    rpho_region == "Rural North" ~ "RANCHO",
    rpho_region == "Los Angeles" ~ "LA",
    TRUE ~ region  # keep existing value if no match
  )) %>%
  filter(!is.na(rpho_region)) %>% 
  recode_rpho_region(.new_col = "region", .ref_col = "rpho_region") %>%
  select(-rpho_region) 

sf_use_s2(F)

shape_df <- read.csv("shape_file/CA_all_sewersheds_centroids.csv")

ca_regions <- st_read("shape_file/saveCA_RPHORegions.shp")  %>%
  recode_rpho_region(.new_col = "region", .ref_col = "rph_rgn") %>% 
  select(-rph_rgn) %>% 
  mutate(center = st_centroid(geometry)) %>% 
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2]
  ) %>%
  st_transform(crs = 4326) %>%
  st_zm() 

ca_counties <- st_read("shape_file/saveCA_RPHOCounties.shp")  %>%
  recode_rpho_region(.new_col = "region", .ref_col = "rph_rgn") %>% 
  select(-rph_rgn) %>% 
  st_transform(crs = 4326) %>%
  mutate(center = st_centroid(geometry)) %>% 
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2]
  ) %>%
  st_zm() 

region_table = td2 %>% select(region, Label_Name) %>% distinct() %>% rename_region()

d1 <-  get_latest_csv(region_path)  %>%
  filter(pcr_gene_target == "n") %>% 
  recode_rpho_region(.new_col = "region", .ref_col = "rpho_region") %>% 
  select(-rpho_region) %>% 
  mutate(sample_date = as.Date(sample_date)) %>% rename_thresholds()

d2 <- get_latest_csv(report_metrics_path) %>%
  filter(pcr_gene_target == "n") %>% 
  recode_rpho_region(.new_col = "region", .ref_col = "rpho_region") %>% 
  select(-rpho_region) %>% 
  mutate(across(c(Label_Name, wwtp_name), ~ gsub(" $", "", .))) %>% rename_thresholds()

f1 <-  get_latest_csv(region_path)  %>%
  filter(pcr_gene_target != "n") %>% 
  recode_rpho_region(.new_col = "region", .ref_col = "rpho_region") %>% 
  select(-rpho_region) %>% 
  mutate(sample_date = as.Date(sample_date)) %>% rename_thresholds()

f2 <-  get_latest_csv(report_metrics_path) %>%
  filter(pcr_gene_target != "n") %>% 
  recode_rpho_region(.new_col = "region", .ref_col = "rpho_region") %>% 
  select(-rpho_region) %>% 
  mutate(across(c(Label_Name, wwtp_name), ~ gsub(" $", "", .))) %>% mutate(region = gsub(pattern = "state", replacement = "State", x = region)) %>% 
  rename_thresholds()

# =============================================================================
# 7. DATA WRANGLING ----
# Combines COVID and flu/RSV data, normalizes column names, builds spatial
# data frames, and derives helper subsets used throughout the app.
#
# Key objects produced:
#   c1        : Combined regional aggregate data (COVID + flu/RSV)
#   c2        : Combined site-level metrics data
#   c2_wwtp   : Site-level subset (only WWTP rows)
#   c2_state  : State-level subset
#   c2_region : Regional-level subset
#   wdf       : Filtered raw data joined to report metadata
#   w1        : wdf with short/long time windows and y-axis clipping
#   state_df  : Spatial sf object for state-level choropleth map
#   c3/c4/c5  : Spatial + flattened datasets for sewershed map
#   pal / spal: Leaflet colorFactor palettes keyed by pathogen
#   below_LOD_list: Named list of sites below detection limit per pathogen
# =============================================================================

c1 <- (d1 %>% bind_rows(f1))  %>%
  left_join(f2 %>% filter(region == "State") %>% select(pcr_gene_target, q1, q2, q3, q4, level),
            by = "pcr_gene_target", suffix = c("_table1", "_table2")) %>%
  mutate(
    q1 = ifelse(is.na(q1_table1), q1_table2, q1_table1),
    q2 = ifelse(is.na(q2_table1), q2_table2, q2_table1),
    q3 = ifelse(is.na(q3_table1), q3_table2, q3_table1),
    q4 = ifelse(is.na(q4_table1), q4_table2, q4_table1),
    level = ifelse(is.na(level_table1), level_table2, level_table1)
  ) %>%
  select(-starts_with("q1_table1"), -starts_with("q1_table2"),
         -starts_with("q2_table1"), -starts_with("q2_table2"),
         -starts_with("q3_table1"), -starts_with("q3_table2"),
         -starts_with("q4_table1"), -starts_with("q4_table2"),
         -starts_with("level_table1"), -starts_with("level_table2")) %>% 
  filter(region != "RANCHO" | pcr_gene_target != "infb")

c2 <- d2 %>% bind_rows(f2) %>% 
  mutate(trend2 = gsub("Potential ", "", trend),
         trend2 = ifelse(trend2 == "Concentrations too low to call trend",
                         "Sporadic Detections",
                         trend2),
         trend2 = factor(trend2, levels = c("Very Strong Increase", 
                                            "Strong Increase",
                                            "Increase",
                                            "Increase from low levels",
                                            "Plateau",
                                            "Decrease", 
                                            "Not enough data",
                                            "Sporadic Detections",
                                            "All Samples Below LOD")),
         wwtp_name =  gsub(" $", "", wwtp_name),
         Label_Name = gsub(" $", "", Label_Name)
         
  ) %>% 
  mutate_at(c("model_pc", "model_pc_lwr", "model_pc_upr"), ~round(., digits = 0))  %>%
  group_by(Label_Name, pcr_gene_target) %>%
  mutate(report_include = ifelse(n() == 1 & report_include == FALSE, TRUE, report_include)) %>%
  ungroup() %>% 
  filter(!(pcr_gene_target == "infb" & is.na(wwtp_name) & region == "RANCHO"))

c2_wwtp = c2 %>% filter(!is.na(wwtp_name))
c2_state = c2 %>% filter(is.na(wwtp_name) & region == "State")
c2_region = c2 %>% filter(is.na(wwtp_name) & region != "State")

published_date_wwtp = list(
  n = strftime(unique(datafilter(c2_wwtp, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_wwtp, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_wwtp, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_wwtp, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

published_date_state = list(
  n = strftime(unique(datafilter(c2_state, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_state, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_state, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_state, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

published_date_region = list(
  n = strftime(unique(datafilter(c2_region, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_region, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_region, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_region, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

published_date = list("Sewershed" = published_date_wwtp,
                      "State" = published_date_state,
                      "Region" = published_date_region)

wdf = td2 %>%
  filter(pcr_gene_target %in% c("infa", "h5",   "rsv",  "n", "infb")) %>% 
  filter(!is.na(region)) %>%
  left_join(
    c2 %>%
      filter(report_include == T) %>%
      select(wwtp_name, data_source, data_source_short, report_include, pcr_gene_target, most_recent_sample)
  ) %>% filter(report_include == T) 

w1 = wdf %>%
  filter(sample_date > Sys.Date()-60) %>%
  mutate(term = "short") %>%
  bind_rows(
    wdf %>%
      filter(sample_date > Sys.Date()-730) %>%
      mutate(term = "long") 
  ) %>%
  mutate(wwtp_name = Label_Name) %>%
  left_join(c2 %>%
              select(Label_Name, q1, q2, q3, q4,  report_include, pcr_gene_target) %>%
              filter(!is.na(Label_Name), report_include == T) %>%
              rename(wwtp_name = Label_Name)
  ) %>%
  group_by(data_source, wwtp_name, pcr_gene_target) %>%
  mutate(norm_pmmov = ifelse(below_LOD == TRUE & pcr_gene_target %in% "n", 
                             0, ifelse(below_LOD == TRUE & !pcr_gene_target %in% "n", 
                                       0,
                                       norm_pmmov))) %>%
  mutate(
    max_norm_pmmov_ten_rollapply = max(norm_pmmov_ten_rollapply, na.rm = TRUE)
  ) %>%
  mutate(
    norm_pmmov_limit = if_else(
      norm_pmmov > upper_y_plot_limit * max_norm_pmmov_ten_rollapply,
      upper_y_plot_limit * 0.99 * max_norm_pmmov_ten_rollapply,
      norm_pmmov
    ),
    data_type = if_else(
      norm_pmmov > upper_y_plot_limit * max_norm_pmmov_ten_rollapply, 
      "limited", 
      ifelse(below_LOD == TRUE, "below LOD", "regular")
    )
  ) %>%
  ungroup() %>%
  select(-max_norm_pmmov_ten_rollapply)

state_df <- c2 %>%
  filter(is.na(wwtp_name)) %>%
  left_join(ca_regions, by = "region") %>%
  st_as_sf() %>%
  mutate(level = factor(level, levels = c("Very Low", "Low", "Moderate", 
                                          "High", "Very High","Not enough data")),
         wwtp_name = region) %>%
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2]
  ) 

pal <- setNames(
  lapply(names(state_threshold_colors), function(x) {
    colorFactor(
      palette = state_threshold_colors[[x]], 
      levels = names(state_threshold_colors[[x]])
    )
  }), 
  names(state_threshold_colors)
)

spal <- setNames(
  lapply(names(sewershed_threshold_colors), function(x) {
    colorFactor(
      palette = sewershed_threshold_colors[[x]], 
      levels = names(sewershed_threshold_colors[[x]])
    )
  }), 
  names(sewershed_threshold_colors)
)

c3 = c2 %>% filter(!is.na(wwtp_name)) %>%
  left_join(shape_df, by = c("wwtp_name" = "sewershed")) %>%
  mutate_at(c("model_pc", "model_pc_lwr", "model_pc_upr"), ~round(., digits = 0)) %>%
  mutate(wwtp_name = Label_Name)

c4 = c3 %>% 
  as.data.frame() %>%
  ungroup() %>%
  filter(!is.na(wwtp_name), report_include == T) %>%
  select(any_of(c("wwtp_name", "data_source", "region", "level", "trend", "trend2", "model_pc",
                  "model_pc_lwr", "model_pc_upr", "pcr_gene_target")))

c5 = c4 %>% filter(!wwtp_name %in% c(
  "Marin (West Railroad)",                     
  "Santa Clara (Stanford Campus)",             
  "Los Angeles (LAX Airport)",              
  "Alabama (Eastern Mission District)",        
  "Ingalls (Smaller section of the S.Bayview)",
  "Jackson (Chinatown & parts of N. Beach)",   
  "San Francisco (Newhall Fairfax)",           
  "Paris &Persia (Excelsior)",                 
  "Rayland & Rutland (Visitation Valley)",     
  "San Diego (South Bay)")
)

targets <- c("n", "infa", "infb", "rsv")

below_LOD_list <- lapply(c("n", "infa", "infb", "rsv"), function(target) {
  w1 %>%
    filter(pcr_gene_target == target, data_type == "below LOD") %>%
    pull(Label_Name) %>%
    unique()
})

names(below_LOD_list) <- targets

# =============================================================================
# 8. SUMMARY & DOWNLOAD TABLES ----
# download_df1 : Long-format raw wastewater data used by the "Download Data"
#                tab's wastewater-samples table.
# download_df2 : Metrics summary (one row per sewershed × pathogen) used by
#                the metrics download table.
# summary_table         : Sewershed-level summary with level + trend +
#                         % change [95% CI], consumed by sewershed_summary_table
#                         in server.R.
# region_summary_table  : Region + statewide equivalent.
# county_list  : County names grouped by region, used by the county dropdown.
# =============================================================================

# Data summary data and download table ---------------------------------------------

download_df1 = td2 %>% ungroup %>%
  filter(!pcr_gene_target %in% c("s")) %>% 
  mutate(sample_type = case_when(
    data_source_short %in% c("scan", "hcvt") ~ "solid",
    !is.na(data_source_short) ~ "liquid",
    TRUE ~ NA_character_
  )) %>% 
  select(region, County_address, Label_Name, wwtp_name, 
         sample_date, sample_type, pcr_gene_target, pcr_target,
         below_LOD, raw_concentration, raw_concentration_ten_rollapply,
         norm_pmmov, norm_pmmov_ten_rollapply,
         data_source) %>%
  rename(County = County_address,
         `County (City/Utility)` = Label_Name,
         `abbreviated_name` = wwtp_name,
         raw_conc_roll_average = raw_concentration_ten_rollapply,
         norm_pmmov_roll_average = norm_pmmov_ten_rollapply) %>%
  filter(!is.na(`County (City/Utility)`),
         !is.na(pcr_gene_target)) %>% 
  filter(sample_date > "2020-01-01") %>% 
  mutate(pcr_gene_target = ifelse(pcr_gene_target %in% c("n1", "N"),
                                  "n",
                                  ifelse(pcr_gene_target %in% c("InfA1" ,  "infa1",
                                                                "infa1 and infa2 combined"),
                                         "infa",
                                         ifelse(pcr_gene_target %in% c("infb"),
                                                "infb",
                                                ifelse(pcr_gene_target %in% c("RSV-A and RSV-B combined", 
                                                                              "rsv-a and rsv-b combined"),
                                                       "rsv",
                                                       ifelse(pcr_gene_target %in% c("infa_h5 (verily)",
                                                                                     "infa_h5 (cdc)"),
                                                              "h5",
                                                              pcr_gene_target)))))) %>% 
  mutate(pcr_target = ifelse(pcr_target %in% c("sars-cov-2"),
                             "SARS-CoV-2",
                             ifelse(pcr_target %in% c("fluav", "FLUAV"),
                                    "Influenza A",
                                    ifelse(pcr_target %in% c("flubv"),
                                           "Influenza B",
                                           ifelse(pcr_target %in% c("RSV", "rsv"),
                                                  "RSV",
                                                  ifelse(pcr_target %in% c("fluav a h5"),
                                                         "Influenza A (H5)",
                                                         pcr_target)))))) %>% 
  filter(pcr_gene_target %in% sapply(target_choice, rename_pathogen)) %>%
  rename_with(~ .x %>%
                gsub("_", " ", .) %>%      # Replace underscores with spaces
                str_to_title() %>%         # Capitalize each word
                gsub("Pcr", "PCR", .)      # Replace "Pcr" with "PCR" after title case
  )

download1_num_col = str_to_title(gsub("_", " ", c(
  "raw_concentration", "raw_conc_roll_average",
  "norm_pmmov", "norm_pmmov_roll_average")))

summary_table <- c2 %>%
  filter(!is.na(wwtp_name)) %>%
  as.data.frame() %>% 
  select(region, wwtp_name, level, trend2, report_include, data_source, pcr_gene_target, most_recent_sample,
         "model_pc", "model_pc_lwr", "model_pc_upr") %>%
  mutate(`Percent Change [95% CI]` = paste0(model_pc, "% [", model_pc_lwr, "%, ",
                                            model_pc_upr, "%]")) %>%
  mutate(`Percent Change [95% CI]` = ifelse(model_pc > 250, "> 250%",
                                            ifelse(is.na(model_pc), " ", `Percent Change [95% CI]`))) %>%
  select(-c("model_pc", "model_pc_lwr", "model_pc_upr")) %>%
  arrange(region, wwtp_name, `Percent Change [95% CI]`, report_include) %>%
  mutate(report_include = ifelse(report_include == T, "yes", 
                                 ifelse(report_include == F, "no", report_include)),
         most_recent_sample = format(as.Date(most_recent_sample), "%m/%d/%Y")
  ) %>%
  left_join(td2 %>% select(Label_Name, wwtp_name, County_address) %>% distinct(),
            by = c("wwtp_name" = "wwtp_name")) %>%
  rename(Region = region, `County (City/Utility)` = Label_Name,
         Level = level, `21 day Trend` = trend2, `Data Source` = data_source,
         `Data displayed on map` = report_include, County = County_address,
         `PCR Gene Target` = pcr_gene_target, `Most Recent Sample Date` = most_recent_sample) %>% 
  select(-wwtp_name)

no_display_on_map <- td2 %>% select(wwtp_name, Label_Name) %>% 
  distinct() %>%
  filter(Label_Name %in% (summary_table %>%
                            filter(`Data displayed on map` == "yes") %>%
                            distinct(`County (City/Utility)`) %>%
                            pull()),
         !wwtp_name %in% shape_df$sewershed
  ) %>% 
  pull(Label_Name)

summary_table <- summary_table %>% 
  mutate(`Data displayed on map` = ifelse(`County (City/Utility)` %in% no_display_on_map,
                                          "no",
                                          `Data displayed on map`))

region_summary_table <- c2 %>%
  filter(is.na(wwtp_name)) %>%
  as.data.frame() %>% 
  select(region, wwtp_name, level, trend2, data_source, pcr_gene_target,
         "model_pc", "model_pc_lwr", "model_pc_upr") %>%
  mutate(`Percent Change [95% CI]` = paste0(model_pc, "% [", model_pc_lwr, "%, ",
                                            model_pc_upr, "%]")) %>%
  mutate(`Percent Change [95% CI]` = ifelse(model_pc > 250, "> 250%",
                                            ifelse(is.na(model_pc), " ", `Percent Change [95% CI]`))) %>%
  select(-c("model_pc", "model_pc_lwr", "model_pc_upr")) %>%
  arrange(region, wwtp_name, `Percent Change [95% CI]`) %>%
  mutate(region = regname(region,reverse = T)) %>% 
  rename(Region = region, 
         Level = level, `21 day Trend` = trend2, `Data Source` = data_source,
         `PCR Gene Target` = pcr_gene_target) %>% select(-wwtp_name) 

download_df2 = summary_table %>% as.data.frame() %>% 
  filter(`PCR Gene Target` %in% sapply(target_choice, rename_pathogen)) %>% 
  mutate(`PCR Target` = sapply(`PCR Gene Target`, rename_pathogen)) %>% 
  select("Region", "County" ,"County (City/Utility)", "PCR Gene Target", "PCR Target",
         "Level", "21 day Trend", "Percent Change [95% CI]", "Data displayed on map", "Most Recent Sample Date",
         "Data Source")

county_df = download_df1 %>% select(County, Region) %>% distinct()
county_list <- tapply(county_df$County, county_df$Region, function(x) sort(x)) %>%
  setNames(map_chr(names(.), regname, reverse = T))
county_list <- county_list[sort_region(names(county_list), 2)]

# =============================================================================
# 9. STATE-LEVEL SUMMARIES ----
# state_region_plot_df : Last 2 years of regional aggregates, used by the
#                        Overview multi-panel plot (region_plot2 in server.R).
# covid_/infA_/infB_/rsv_state_level + _state_trend:
#   Scalar statewide level & trend strings per pathogen. These feed the
#   clickable homepage info boxes (home_covid, home_fluA, ...) so the app can
#   show current statewide status without reading the full dataset in the UI
#   thread.
# =============================================================================

# state map data ----------------------------------------------------------

state_region_plot_df = c1 %>% filter(sample_date > max(sample_date)-730)

# State level and trend ----------------------------------------------------------

covid_state_level = c2 %>% filter(region == "State", pcr_gene_target == "n") %>% pull(level)
covid_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "n") %>% pull(trend)

infA_state_level = c2 %>% filter(region == "State", pcr_gene_target == "infa") %>% pull(level)
infA_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "infa") %>% pull(trend)

infB_state_level = c2 %>% filter(region == "State", pcr_gene_target == "infb") %>% pull(level)
infB_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "infb") %>% pull(trend)

rsv_state_level = c2 %>% filter(region == "State", pcr_gene_target == "rsv") %>% pull(level)
rsv_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "rsv") %>% pull(trend)


# NOTE: region_vec and region_choice are defined here AND again inside
# section 10 below. The duplicate assignments are harmless (same values) and
# were left in place during the v1->v2 refactor. Safe to consolidate in a
# future cleanup.
region_vec = c("ABAHO", "RANCHO", "SJVC", "SACRAMENTO", "LA","SOCAL")

region_choice = regname(region_vec, reverse = T)


# =============================================================================
# 10. UI GLOBALS ----
# region_vec    : Ordered vector of RPHO region codes used for UI dropdowns.
# region_choice : Human-readable region names for UI display.
# js_code_plot  : JavaScript callback injected into Plotly charts (via
#                 htmlwidgets::onRender) to make range-selector buttons and
#                 legend items keyboard-accessible (ADA Section 508 compliance).
# =============================================================================

region_vec = c("ABAHO", "RANCHO", "SJVC", "SACRAMENTO", "LA","SOCAL")

region_choice = regname(region_vec, reverse = T)

js_code_plot <- "
function(el, x) {
  // Use a short delay to ensure the plot is fully rendered before we modify it
  setTimeout(function() {
    // --- Make Range Selector Buttons Accessible ---
    const rangeButtons = el.querySelectorAll('.rangeselector .button');
    rangeButtons.forEach(button => {
      // Get the text content of the button for the aria-label
      const buttonText = button.querySelector('text').textContent;

      // Make the button group focusable and interactive
      button.setAttribute('tabindex', '0');
      button.setAttribute('role', 'button');
      button.setAttribute('aria-label', `Select time range: ${buttonText}`);

      button.addEventListener('keydown', function(event) {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          // Simulate a click event on the button
          const clickEvent = new MouseEvent('click', {
            view: window,
            bubbles: true,
            cancelable: true
          });
          button.dispatchEvent(clickEvent);
        }
      });
    });

    // --- Make Legend Items Accessible ---
    // The selector '.legend .traces' targets the clickable legend items
    const legendItems = el.querySelectorAll('.legend .traces');
    legendItems.forEach(item => {
      // Get the text of the legend item for the aria-label
      const legendText = item.querySelector('.legendtext').textContent;
      
      if (legendText) {
        // Make the legend item focusable and interactive
        item.setAttribute('tabindex', '0');
        item.setAttribute('role', 'button');
        item.setAttribute('aria-label', `Toggle visibility for ${legendText}`);

        item.addEventListener('keydown', function(event) {
          if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            // Simulate a click event on the legend item
            const clickEvent = new MouseEvent('click', {
              view: window,
              bubbles: true,
              cancelable: true
            });
            item.dispatchEvent(clickEvent);
          }
        });
      }
    });
  }, 500); // 500ms delay
}
"

