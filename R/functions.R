# =============================================================================
# FILE: R/functions.R
# PROJECT: Cal-SuWers Public Dashboard v2
# DESCRIPTION:
#   All custom helper functions used across global.R, server.R, and ui.R.
#   This file is sourced at the top of global.R so these functions are
#   available to every other component of the app.
#
# SECTIONS:
#   1. Region Mapping Functions   - recode_rpho_region, sort_region, rename_region, regname
#   2. Pathogen Utility Functions - rename_pathogen, datafilter, rename_thresholds
#   3. Color / Theme Functions    - getStateColor, get_box_class, seweshed_get_box_class,
#                                   theme_fonts
#   4. Plot Utility Functions     - metric_plot, trend_symbol
#   5. UI Helper Functions        - createOverviewTab
#   6. Data I/O Functions         - get_latest_csv
# =============================================================================

# =============================================================================
# 1. REGION MAPPING FUNCTIONS ----
# =============================================================================

#' Recode an RPHO region column using a fixed mapping.
#'
#' @param .data     A data frame with a column containing region names.
#' @param .new_col  Name (string) of the column to write into. Will be created
#'                  if it does not already exist.
#' @param .ref_col  Name (string) of the column to read from.
#' @param reverse   FALSE (default) maps full names ("Bay Area") to codes
#'                  ("ABAHO"). TRUE maps codes back to full names.
#' Used throughout global.R and server.R so region labels stay consistent
#' between the data files and the UI.
recode_rpho_region <- function(.data, .new_col, .ref_col, reverse = F) {
  
  if(reverse == F){
    region_map <- list(
      "Greater Sierra Sacramento" = "SACRAMENTO",
      "Bay Area"                  = "ABAHO",
      "Southern California"       = "SOCAL",
      "Central California"        = "SJVC",
      "Rural North"               = "RANCHO",
      "Los Angeles"               = "LA",
      "State"                     = "State"
    )
  } else if (reverse == T){
    region_map <- list(
      "SACRAMENTO" = "Greater Sierra Sacramento",
      "ABAHO"      = "Bay Area",
      "SOCAL"      = "Southern California",
      "SJVC"       = "Central California",
      "RANCHO"     = "Rural North",
      "LA"         = "Los Angeles",
      "State"      = "State"
    )
  }
  
  case_formulas <- imap(region_map, ~{
    expr(!!sym(.ref_col) == !!.y ~ !!.x)
  })
  
  if (.new_col %in% names(.data)) {
    default_formula <- expr(TRUE ~ !!sym(.new_col))
  } else {
    default_formula <- expr(TRUE ~ NA_character_)
  }
  
  all_formulas <- c(case_formulas, default_formula)
  
  .data %>%
    mutate(
      !!.new_col := case_when(!!!all_formulas)
    )
}

#' Sort a vector of region names into the dashboard's canonical display order.
#'
#' @param vec  A character vector of region names.
#' @param set  1 = use short codes ("ABAHO", "RANCHO", ...);
#'             2 = use full names ("Bay Area", "Rural North", ...).
#' The dashboard always displays regions in a fixed order (not alphabetical),
#' so use this function before plotting or rendering dropdowns.
sort_region <- function(vec, set = 1) {
  # Define the two reference orders
  order1 <- c("ABAHO", "RANCHO", "SJVC",
              "SACRAMENTO", "LA", "SOCAL", "State")
  
  order2 <- c("Bay Area", "Rural North", "Central California", 
              "Greater Sierra Sacramento", "Los Angeles", "Southern California", "State")
  
  if (set == 1) {
    ref_order <- order1
  } else if (set == 2) {
    ref_order <- order2
  } else {
    stop("Invalid input: set must be 1 or 2")
  }
  
  sort(factor(vec, levels = ref_order)) |> as.character()
}

#' Rename region codes to full display names in a data frame's `region` column.
#' Kept separate from regname() because it operates on a whole data frame and
#' returns the modified frame rather than a vector.
rename_region = function(df){
  df2 = df %>%
    mutate(region = ifelse(region == "SOCAL",
                           "Southern California",
                           ifelse(region == "SACRAMENTO",
                                  "Greater Sierra Sacramento",
                                  ifelse(region == "SJVC",
                                         "Central California",
                                         region)))
    )
  return(df2)
}

# =============================================================================
# 4. PLOT UTILITY FUNCTIONS ----
# =============================================================================

#' Build a Plotly time-series chart for wastewater concentration.
#'
#' This is the workhorse plotting function used by the state, region, and
#' sewershed plots in server.R. It handles:
#'   - optional horizontal threshold lines (hline_y1..hline_y4) for the
#'     Very Low / Low / Moderate / High / Very High bands,
#'   - a vertical "21 days ago" reference line (vline_date),
#'   - an optional rangeslider + range-selector buttons,
#'   - multi-target overlays (e.g. Flu A solid + Flu B dotted) driven by
#'     target_col + solid_line_target,
#'   - an optional scatter overlay of individual samples styled by data_type
#'     (regular / limited / below LOD), toggled with show_scatter.
#'
#' Most arguments have sensible defaults; the mandatory ones are `data`,
#' `vline_date`, `vline_label_position`, and the four `hline_y*` thresholds
#' (which may be NA to skip drawing horizontal lines).
metric_plot <- function(data,
                        x_col = "sample date",
                        y_col = "raw concentration normalized by pmmov",
                        hover_label = "WVAL Rolling Average",
                        vline_date,
                        vline_label_position,
                        vline_label_font = list(size = 12),
                        vline_col = "raw concentration normalized by pmmov",
                        hline_y1, 
                        hline_y2,
                        hline_y3,
                        hline_y4,
                        plot_title = "",
                        plot_title_y = 1.1,
                        y_label = "",
                        y_label_font = list(size = 18),
                        x_label = "Adjust the time range using the slider above",
                        x_label_show = F,
                        y_lower_limit = 0,
                        concentration_label = "Raw Concentration",
                        show_scatter = F,
                        scatter_col = "",
                        scatter_hover_text_col = "",
                        scatter_type = "",
                        point_size = 20,
                        show_h_lines = T,
                        show_h_label = F,
                        show_rangeslider = F,
                        rangeslider_thickness = 0.08,
                        range_selector = T,
                        ymax = FALSE,
                        margins = list(l = 80, r = 50, t = 50, b = 50),
                        upper_y_plot_limit = 1.2,
                        dual_target = FALSE,
                        single_plot = F,
                        select_target = "",
                        target_col = "",
                        solid_line_target = "",
                        legend_font_size = 16,
                        show_y_axis_line = T,
                        show_legend = TRUE) {
  
 
  if(select_target %in% c("n", "rsv") | single_plot == T){
    dual_target <- FALSE
  }
  
  if (dual_target == FALSE | single_plot == T) {
    show_legend <- FALSE
  }
  
  if(dual_target == F) { show_y_axis_line = F }
  if(dual_target == T & solid_line_target == "infb") {ymax = F}
  
  max_y_value <- max(data[[y_col]], na.rm = TRUE)
  y_axis_upper_limit <- if (ymax) max_y_value * upper_y_plot_limit else NULL
  if (show_scatter && scatter_col != "") {
    y_axis_lower_limit <- -0.5
  } else {
    y_axis_lower_limit <- y_lower_limit
  }
  min_x <- min(data[[x_col]], na.rm = TRUE) - 1
  max_x <- max(data[[x_col]], na.rm = TRUE) + 1
  num_day_diff <- as.numeric(as.Date(max_x) - as.Date(min_x)) + 2
  shapelist <- list(
    list(
      type = 'line', 
      x0 = as.Date(vline_date), 
      x1 = as.Date(vline_date),
      y0 = 0, 
      y1 = max(data[[vline_col]], na.rm = TRUE) * upper_y_plot_limit,
      line = list(color = 'gray', dash = 'dash')
    )
  )
  
  `Very Low` = "#BAE8DE";
  Low = "#B8E5AC";
  Moderate = "#FEA82F";
  High = "#F45B53";
  `Very High` = "#C15C9C";
 
  if (show_h_lines & !is.na(hline_y1) & !is.na(hline_y2) & !is.na(hline_y3) & !is.na(hline_y4)) {
    shapelist <- c(shapelist,
                   list(
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y1,
                       y1 = hline_y1,
                       line = list(color = `Very Low`, dash = 'dash')
                     ),
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y2,
                       y1 = hline_y2,
                       line = list(color = Low, dash = 'dash')
                     ),
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y3,
                       y1 = hline_y3,
                       line = list(color = Moderate, dash = 'dash')
                     ),
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y4,
                       y1 = hline_y4,
                       line = list(color = High, dash = 'dash')
                     )
                   ))
  }
  
  plot <- plot_ly()
  
  legend_order <- c("infa", "infb", "h5")
  
  if (!(solid_line_target %in% legend_order)) {
    legend_order <- c(solid_line_target, legend_order)
  }
  
  styles <- list(
    main = list(color = 'black',   dash = 'solid'), 
    infa = list(color = '#002900', dash = 'dot',     name = "Flu A"),
    infb = list(color = '#002900', dash = 'dot',     name = "Flu B"),
    h5   = list(color = '#E69F00', dash = 'dashdot', name = "Flu A (H5)")
  )
  
  main_target_names <- list(infa = "Flu A", infb = "Flu B", h5 = "H5", n = "SARS-CoV-2", rsv = "RSV")
  
  if(select_target %in% c("n", "rsv")){
    targets_to_plot <- select_target
  } else {
    targets_to_plot <- unique(data[[target_col]])
  }
  
  for (current_target in legend_order) {
    
    if (current_target %in% targets_to_plot) {
      
      if (solid_line_target == "infb" && current_target == "h5") {
        next
      }
      
      is_main_target <- (current_target == solid_line_target)
      
      if (is_main_target) {
        current_style <- styles$main
        legend_name <- main_target_names[[current_target]]
      } else {
        current_style <- styles[[current_target]]
        legend_name <- current_style$name
      }
      
      if (is.null(current_style)) next
      
      data_subset <- subset(data, get(target_col) == current_target)
      hover_text <- paste0("<b>Target:</b> ", legend_name, 
                           "<br><b>Sample Date:</b> ", data_subset[[x_col]], 
                           "<br><b>", hover_label, ":</b> ", round(data_subset[[y_col]], digits = 1))
      
      plot <- plot %>%
        add_lines(data = data_subset, x = ~get(x_col), y = ~get(y_col),
                  text = hover_text, hoverinfo = 'text',
                  line = list(color = current_style$color, dash = current_style$dash),
                  name = legend_name,
                  legendgroup = legend_name,
                  showlegend = show_legend)
    }
  }
  
  plot <- plot %>%
    layout(
      legend = list(font = list(size = legend_font_size)),
      xaxis = list(
        title = if (x_label_show) x_label else "", tickformat = "%m/%Y", showgrid = FALSE, range = c(min_x, max_x),
        rangeslider = list(visible = show_rangeslider, range = c(min_x, max_x), borderwidth = 2, thickness = rangeslider_thickness, bordercolor = "#5A789A", bgcolor = "rgba(211, 211, 211, 0.4)"),
        rangeselector = if (range_selector) list(buttons = list(list(count = 45, label = "45 days", step = "day", stepmode = "backward"), list(count = 6, label = "6m", step = "month", stepmode = "backward"), list(count = 1, label = "1 yr", step = "year", stepmode = "backward"), list(count = num_day_diff, label = "All", step = "day", stepmode = "backward")), y = 1.01, x = 0.1, bgcolor = "#635e5e", activecolor = "#141414", borderwidth = 1, font = list(color = "white")) else NULL
      ),
      yaxis = list(
        title = y_label,
        titlefont = y_label_font,
        range = c(y_axis_lower_limit, y_axis_upper_limit),
        showgrid = FALSE,
        showline = show_y_axis_line
      ),
      shapes = shapelist,
      margin = margins
    )
  
  annotations <- list(list(x = as.Date(vline_date), y = vline_label_position, text = "21 days ago", showarrow = FALSE, xanchor = "left", yanchor = "middle", ax = 20, ay = -40, textangle = -90, font = vline_label_font, bgcolor = "rgba(255, 255, 255, 0.5)", borderpad = 2))
  if (show_h_lines & !is.na(hline_y1) & !is.na(hline_y2) & !is.na(hline_y3) & !is.na(hline_y4) & show_h_label) {
    annotations <- c(annotations,
                     list(
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)), y = hline_y1, text = "Very Low",
                         showarrow = FALSE, xanchor = "left", yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(186, 232, 222, 0.8)",  # #BAE8DE
                         borderpad = 2
                       ),
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)), y = hline_y2, text = "Low",
                         showarrow = FALSE, xanchor = "left", yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(184, 229, 172, 0.8)",  
                         borderpad = 2
                       ),
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)), y = hline_y3, text = "Moderate",
                         showarrow = FALSE, xanchor = "left", yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(254, 168, 47, 0.8)",  
                         borderpad = 2
                       ),
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)), y = hline_y4, text = "High",
                         showarrow = FALSE, xanchor = "left", yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(244, 91, 83, 0.8)",  
                         borderpad = 2
                       )
                     ))
  }
  plot <- plot %>% layout(annotations = annotations) %>%
    add_annotations(text = plot_title, x = 0.5, y = plot_title_y, yref = "paper", xref = "paper", xanchor = "center", yanchor = "top", yshift = 0, showarrow = FALSE, font = list(size = 15, family = "Arial", color = "black"))
  
  if (show_scatter && scatter_col != "") {
    data2 = data %>% filter(pcr_gene_target == solid_line_target)
    if(dual_target == T & solid_line_target == "infb") {
      data2 = data2 %>% mutate(data_type = ifelse(data_type == "limited", "regular", data_type))
    }
    
    regular_data <- subset(data2, get(scatter_type) == "regular") 
    regular_data[[scatter_col]] <- round(regular_data[[scatter_col]], 2)
    scatter_hover_text_regular <- paste("<b>Sample Date:</b>", regular_data[[x_col]], "<br><b>", concentration_label, ":</b>", regular_data[[scatter_hover_text_col]])
    plot <- plot %>% add_markers(data = regular_data, x = ~get(x_col), y = ~get(scatter_col), text = scatter_hover_text_regular, hoverinfo = 'text', marker = list(size = point_size, color = 'rgba(153, 182, 207, 0.7)', symbol = "circle"), name = "Regular", showlegend = F)
    
    
    limited_data <- subset(data2, get(scatter_type) == "limited")
    limited_data[[scatter_col]] <- round(limited_data[[scatter_col]], 2)
    scatter_hover_text_limited <- paste("<b>High value, above y-axis limit</b><br>", "<b>Sample Date:</b>", limited_data[[x_col]], "<br><b>", concentration_label, ":</b>", limited_data[[scatter_hover_text_col]])
    plot <- plot %>% add_markers(data = limited_data, x = ~get(x_col), y = ~get(scatter_col), text = scatter_hover_text_limited, hoverinfo = 'text', marker = list(size = point_size, color = 'rgba(0, 0, 102, 0.7)', symbol = "triangle-up"), name = "High value", showlegend = F)
    
    no_detect_data <- subset(data2, get(scatter_type) == "below LOD")
    scatter_hover_text_below <- paste("<b>Not detected</b><br>", "<b>Sample Date:</b>", no_detect_data[[x_col]])
    plot <- plot %>% add_markers(data = no_detect_data, x = ~get(x_col), y = ~get(scatter_col), text = scatter_hover_text_below, hoverinfo = 'text', marker = list(size = point_size, color = 'rgba(32, 32, 32, 0.7)', symbol = "triangle-down-open"), name = "Non-detected", showlegend = F)
  }
  
  return(plot)
}

# =============================================================================
# 6. DATA I/O ----
# =============================================================================

#' Read the most recently modified CSV in a folder.
#' Used in global.R to pick up the latest weekly metrics/aggregate exports
#' without hard-coding a filename.
get_latest_csv <- function(path) {
  files <- list.files(path, full.names = TRUE, pattern = "csv")
  latest_file <- files[which.max(file.info(files)$mtime)]
  read.csv(latest_file)
}

# =============================================================================
# 1b. REGION NAME HELPER (vector version)
# =============================================================================

#' Convert region names between full names and short codes.
#' Unlike recode_rpho_region() (which works on a data frame column) this works
#' on a plain character vector and is handy for one-off label conversions.
#'
#' @param region  Character vector to convert.
#' @param reverse FALSE = full name -> code (e.g. "Bay Area" -> "ABAHO");
#'                TRUE  = code -> full name.
regname <- function(region, reverse = FALSE) {
  region_map <- c(
    "Statewide" = "State",
    "Greater Sierra Sacramento" = "SACRAMENTO",
    "Bay Area" = "ABAHO",
    "Southern California" = "SOCAL",
    "Central California" = "SJVC",
    "Rural North" = "RANCHO",
    "Los Angeles" = "LA"
  )
  
  if (reverse) {
    reverse_map <- setNames(names(region_map), region_map)
    return(ifelse(region %in% names(reverse_map), reverse_map[region], region))
  } else {
    return(ifelse(region %in% names(region_map), region_map[region], region))
  }
}

# =============================================================================
# 2. PATHOGEN UTILITY FUNCTIONS ----
# =============================================================================

#' Convert between display pathogen names and internal pcr_gene_target codes.
#'
#' Passes through input unchanged if no match is found, so it's safe to call
#' repeatedly. The function is bidirectional: it looks up the name as a key
#' first, and if that fails, looks it up as a value.
#'
#' @param input_value Either a display name ("SARS-CoV-2", "Flu A") or an
#'                    internal code ("n", "infa").
#' @param choice      1 uses short display names ("Flu A"); 2 (default) uses
#'                    formal names ("Influenza A"). Tables and titles generally
#'                    use choice=2; sidebar pickers use choice=1.
rename_pathogen <- function(input_value, choice = 2) {
  if (choice == 1){
    pathogen <- c(
      "SARS-CoV-2" = "n",
      "Flu A" = "infa",
      "Flu B" = "infb",
      "RSV" = "rsv",
      "Influenza A (H5)" = "h5"
    )
  }else if (choice ==2) {
    pathogen <- c(
      "SARS-CoV-2" = "n",
      "Influenza A" = "infa",
      "Influenza B" = "infb",
      "RSV" = "rsv",
      "Influenza A (H5)" = "h5"
    )
  }
  
    if (input_value %in% names(pathogen)) {
    return(pathogen[[input_value]])  # Return the value based on name
  }
  
  if (input_value %in% pathogen) {
    return(names(pathogen)[pathogen == input_value])  # Return the name based on value
  }
  
  return(input_value)
}

#' Filter a data frame on a single column == value.
#' Used pervasively to subset the c1..c5 frames to the current pathogen.
#' Errors loudly if the column doesn't exist.
datafilter <- function(data, column_name = "pcr_gene_target", value) {
  if (!column_name %in% colnames(data)) {
    stop(paste("Column", column_name, "does not exist in the dataset"))
  }
  filtered_data <- data[data[[column_name]] == value, ]
  
  return(filtered_data)
}

# =============================================================================
# 3. COLOR / THEME FUNCTIONS ----
# =============================================================================

#' Return the CSS class name used to color a region/state info box by level.
#' The class names map to custom CSS rules defined in www/styles.css
#' (e.g. "bg-custom-covid_high"). Returns NULL for unrecognized levels.
get_box_class <- function(target, state_value_list) {
  if (target %in% "n") {
    value <- state_value_list[["level_call"]]
    
    
    if (value == "Very High") {
      return("bg-custom-covid_very_high")
    } else if (value == "High") {
      return("bg-custom-covid_high")
    } else if (value == "Moderate") {
      return("bg-custom-covid_moderate")
    } else if (value == "Low") {
      return("bg-custom-covid_low")
    } else if (value == "Very Low") {
      return("bg-custom-covid_very_low")
    } else if (value == "Not enough data") {
      return("bg-custom-covid_no_data")
    }
    
  } else if (target %in% c("infa", "rsv", "infb")) {
    value <- state_value_list[["level_call"]]
    
    
    if (value == "Very High") {
      return("bg-custom-covid_very_high")
    } else if (value == "High") {
      return("bg-custom-covid_high")
    } else if (value == "Moderate") {
      return("bg-custom-covid_moderate")
    } else if (value == "Low") {
      return("bg-custom-covid_low")
    } else if (value == "Very Low") {
      return("bg-custom-covid_very_low")
    } else if (value == "Not enough data") {
      return("bg-custom-covid_no_data")
    }
    
  }
  
  return(NULL)  
}

#' Sewershed-tab variant of get_box_class().
#' Uses a different CSS palette (bg-custom-sewershed_*) tuned for the
#' neutral-beige sewershed map rather than the multi-color region map.
#' NOTE: name is misspelled ("seweshed") — kept as-is to avoid breaking
#' callers. Fix in a future release.
seweshed_get_box_class <- function(state_value_list) {
  
  value <- state_value_list[["level_call"]]
  
  if (value == "Very High") {
    return("bg-custom-sewershed_very_high")
  } else if (value == "High") {
    return("bg-custom-sewershed_high")
  } else if (value == "Moderate") {
    return("bg-custom-sewershed_moderate")
  } else if (value == "Low") {
    return("bg-custom-sewershed_low")
  } else if (value == "Very Low") {
    return("bg-custom-sewershed_very_low")
  } else if (value == "Not enough data") {
    return("bg-custom-sewershed_no_data")
  }
  
  return(NULL)  
}

#' Map a (pathogen, level) pair to the hex fill color used in state/region
#' summary boxes and the map legend. Both arms of the if/else currently
#' return the same colors — kept as two branches so that future pathogens
#' can diverge without a large refactor.
getStateColor <- function(target, category) {
  if (target == "n") {
    
    if (category == "Very High") {
      return("#C15C9C")
    } else if (category == "High") {
      return("#F45B53")
    } else if (category == "Moderate") {
      return("#FEA82F")
    } else if (category == "Low") {
      return("#B8E5AC")
    } else if (category == "Very Low") {
      return("#BAE8DE")
    } else if (category == "Not enough data") {
      return("#969696")
    } else {
      return(NULL)
    }
  }
  
  else if (target %in% c("infa", "infb", "rsv")) {
    
    if (category == "Very High") {
      return("#C15C9C")
    } else if (category == "High") {
      return("#F45B53")
    } else if (category == "Moderate") {
      return("#FEA82F")
    } else if (category == "Low") {
      return("#B8E5AC")
    } else if (category == "Very Low") {
      return("#BAE8DE")
    } else if (category == "Not enough data") {
      return("#969696")
    } else {
      return(NULL)
    }
  }
  
  else {
    return(NULL) 
  }
}

#' ggplot2 theme used by any static (non-Plotly) plots in the app.
#' Strips gridlines and uses bold axis/legend text tuned for the dashboard.
theme_fonts <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

#' Map a trend category string to a single-character symbol used in the
#' "Trends" technical-notes table. The symbols mirror the Font Awesome icons
#' used on the Leaflet maps so the legend and the table match visually.
trend_symbol <- function(trend_category) {
  if (trend_category %in% c(
    "Decrease", "Potential Decrease", "Strong Decrease",
    "Potential Strong Decrease", "Potential Very Strong Decrease"
  )) {
    return("↓")  # Down arrow
  } else if (trend_category %in% c(
    "Increase", "Strong Increase", "Very Strong Increase",
    "Potential Increase", "Potential Strong Increase", "Potential Very Strong Increase"
  )) {
    return("↑")  # Up arrow
  } else if (trend_category %in% c("Plateau", "Potential Plateau")) {
    return("↔")  # Bidirectional arrow for plateau
  } else if (trend_category == "Sporadic Detections") {
    return("…")  # Ellipsis for sporadic detections
  } else if (trend_category == "All Samples Below LOD") {
    return("◯")  # Open circle for all below LOD
  } else if (trend_category == "Not enough data") {
    return("×")  # Cross for insufficient data
  } else {
    return("")  # Default empty
  }
}


# =============================================================================
# 5. UI HELPER FUNCTIONS ----
# =============================================================================

#' Build a standard tabPanel used on the Respiratory Virus Data page.
#' Every per-pathogen tab (Statewide, Region, Sewershed) shares the same
#' header band + optional Overview/Each Region toggle + plot container +
#' optional collapsible summary table, so this helper keeps ui.R DRY.
#'
#' @param title_id       uiOutput id that the server renders the page title into.
#' @param tab_name       Tab label shown in the tabsetPanel.
#' @param include_switch TRUE to show the Overview / Each Region radio group.
#' @param main_plot      uiOutput id whose server-side code renders the plot.
#' @param include_table  TRUE to show the collapsible summary table panel.
createOverviewTab <- function(title_id, tab_name, include_switch = TRUE, main_plot,
                              include_table = F) {
  tabPanel(tab_name,
       
           tags$head(tags$style(HTML("
              #region_toggle_container .btn-group {
                height: 51px !important; /* <-- Manually set the height in pixels */
              }
              
              /* This part vertically centers the text inside the taller buttons */
             #region_toggle_container .btn {
                display: flex;
                align-items: center;
                justify-content: center;
                height: 100%;
                font-size: 2.1rem !important;  /* <-- Increase font size */
               
              }
           "))),
           
           br(),
           fluidRow(
       
             column(
               width = if (include_switch) 6 else 12,
               div(style = "background-color: #99b6cf; padding: 12px; border-radius: 5px; height: 100%;",
                   h3(uiOutput(title_id),
                      style = "text-align: left; margin: 0; font-weight: bold; color: #333;"
                   )
               )
             ),
             
             if (include_switch) {
               column(width = 6, id = "region_toggle_container",
                      radioGroupButtons(
                        inputId = "region_toggle",
                        label = NULL,
                        choices = c("Overview", "Each Region"),
                        selected = "Overview",
                        status = "primary",
                        justified = TRUE
                      )
               )
             }
           ),
           br(),
           uiOutput(main_plot),
           if(include_table){
             list(
               titlePanel(
                 uiOutput("table_header"),
               ),
               bsCollapse(
                 id = "collapseTable",  
                 open = "panel1",         
                 bsCollapsePanel(
                   tags$span("Show/Hide Table", style = "font-size: 24px; font-weight: bold;"),
                   DT::dataTableOutput("covid_table") %>% withSpinner(color = "#5A789A"),
                   value = "panel1"       
                 )
               )
             )
           }
  )
}

#' Rename the four WVAL threshold columns from the upstream CSV schema
#' (Very_Low_Threshold, Low_Threshold, Medium_Threshold, High_Threshold) to
#' the short q1/q2/q3/q4 names used throughout the dashboard code.
rename_thresholds <- function(df) {
  df %>%
    rename(
      q1 = Very_Low_Threshold,
      q2 = Low_Threshold,
      q3 = Medium_Threshold,
      q4 = High_Threshold
    )
}
