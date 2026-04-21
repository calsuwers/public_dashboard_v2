# =============================================================================
# FILE: server.R
# PROJECT: Cal-SuWers Public Dashboard v2
# DESCRIPTION:
#   Defines the Shiny server function. All reactive expressions, observers,
#   and output renderers live here. The server reads global objects produced
#   in global.R and responds to user inputs defined in ui.R.
#
# READING ORDER TIP:
#   Sections are grouped by purpose, not strict dependency order. A handful of
#   reactives (notably `target()` in section 12 and rv/re in section 16) are
#   referenced by earlier sections. If you're tracing a single feature end to
#   end, search for the relevant `output$...` name rather than reading top to
#   bottom.
#
# SECTIONS (section number matches the in-body header):
#   1.  Reactive Data Filters         - wdf(), w1()
#   2.  Statewide Trend Summary       - statewide_trend_summmary renderUI
#   3.  UI Visibility Toggles         - shinyjs toggles; flu_toggle_message()
#   4.  Plot Signal Helper            - single_plot_signal()
#   5.  Technical Notes Tables        - levelsTable, trendsTable
#   6.  Navigation Observers          - tab link clicks, description toggles
#   7.  ShinyAlert Announcement Modal - announce_button popup
#   8.  Dashboard Update Feed         - dash_update renderUI
#   9.  Homepage Info Boxes           - home_covid, home_fluA, home_fluB, home_rsv
#  10.  State Summary UI              - state_summary_ui, h5_toggle_message()
#  11.  Region Summary UI             - region_content renderUI
#  12.  Target Selector & Filters     - target(), c11()-c55(), page titles
#  13.  Dropdown Reactives            - statedropdown(), dynamic_*_picker
#  14.  Map Data Reactives            - mapdf(), smapdf(), icon lists, labels
#  15.  Leaflet Map Outputs           - heatmap_region, heatmap_region_2,
#                                       heatmap_sewershed
#  16.  Map-Click Selection Reactives - rv/re reactiveVals, plot_site_data()
#  17.  Plot Helpers                  - create_bar_plot(), plot_target(),
#                                       dualplot(), annotation lists
#  18.  State Plot                    - state_plot renderPlotly
#  19.  Region Plots                  - each_region_plot (single region)
#  20.  Overview / Sewershed Plots    - region_plot2 (multi-panel overview),
#                                       dynamic_sewershed_plot, long_plot
#  21.  Info Boxes                    - level_box, level_box_sewershed,
#                                       state_info_box, trend_box, rv2/rv3
#  22.  Summary Tables                - region_summary_table,
#                                       sewershed_summary_table
#  23.  Data Download Tables          - download_table1, download_table2,
#                                       downloadData1, downloadData2
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # 1. REACTIVE DATA FILTERS ----
  # wdf()  : Filters td2 to relevant pathogens, renames raw_concentration to
  #          norm_pmmov for consistent downstream column naming.
  # w1()   : Builds short (60-day) and long (all-time) windows from wdf(),
  #          joins threshold quartiles (q1-q4), clips outliers above y-axis limit.
  # ===========================================================================
  wdf <- reactive({
    
    df <- td2 %>%
      select(-norm_pmmov, -norm_pmmov_ten_rollapply) %>% 
      rename(
        norm_pmmov = raw_concentration,
        norm_pmmov_ten_rollapply = raw_concentration_ten_rollapply
      )
    
    df <- df %>%
      filter(pcr_gene_target %in% c("infa", "h5", "rsv", "n", "infb")) %>% 
      filter(!is.na(region)) %>%
      left_join(
        c2 %>%
          filter(report_include == T) %>%
          select(wwtp_name, data_source, data_source_short, report_include, pcr_gene_target, most_recent_sample)
      ) %>% filter(report_include == T)
    
    return(df)
  })
  
  w1 <- reactive({
    req(wdf())
    wdf <- wdf()
    
    df = wdf %>%
      filter(sample_date > Sys.Date()-60) %>%
      mutate(term = "short") %>%
      bind_rows(
        wdf %>%
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
    
    return(df)
  })
  
  # ===========================================================================
  # 2. STATEWIDE TREND SUMMARY ----
  # Renders a bullet-point UI summarizing current statewide level + trend
  # for each active pathogen. Displayed on the Statewide tab header.
  # ===========================================================================
  output$statewide_trend_summmary <- renderUI({
    date_text <- published_date_state$n
    
    trend_data <- state_df %>%
      as.data.frame() %>%
      filter(region == "State", pcr_gene_target != "h5") %>%
      select(pcr_gene_target, level, trend) %>%
      mutate(
        pathogen_name = sapply(pcr_gene_target, rename_pathogen, choice = 2),
        trend_verb = case_when(
          trend == "Very Strong Increase"         ~ "very strongly increasing",
          trend == "Strong Increase"              ~ "strongly increasing",
          trend == "Potential Very Strong Increase" ~ "potentially very strongly increasing",
          trend == "Potential Strong Increase"    ~ "potentially strongly increasing",
          trend == "Potential Increase"           ~ "potentially increasing",
          trend == "Increase"                     ~ "increasing",
          trend == "Potential Decrease"           ~ "potentially decreasing",
          trend == "Strong Decrease"              ~ "strongly decreasing",
          trend == "Potential Strong Decrease"    ~ "potentially strongly decreasing",
          trend == "Potential Very Strong Decrease" ~ "potentially very strongly decreasing",
          trend == "Decrease"                     ~ "decreasing",
          trend == "Plateau"                      ~ "plateauing",
          trend == "Potential Plateau"            ~ "potentially plateauing",
          trend == "Not enough data"              ~ "not enough data to determine a trend",
          trend == "Sporadic Detections"          ~ "sporadic detections",
          trend == "All Samples Below LOD"        ~ "below the limit of detection",
          TRUE                                    ~ tolower(trend)
        )
      )
    
    bullet_points <- lapply(1:nrow(trend_data), function(i) {
      sentence <- paste0(
        trend_data$pathogen_name[i], " concentrations are ",
        tolower(trend_data$level[i]), " and ",
        trend_data$trend_verb[i], "."
      )
      tags$li(style = "font-size: 23px;", sentence)
    })
    
    tagList(
      p(style = "font-size: 23px;", "As of ", date_text,
        " wastewater concentrations show the following statewide patterns:"),
      tags$ul(bullet_points)
    )
  })
  

  # ===========================================================================
  # 3. UI VISIBILITY TOGGLES  (shinyjs) ----
  # These observers show/hide sidebar control panels depending on which
  # tab and pathogen the user has selected. Ensures only relevant controls
  # are visible (e.g., flu B toggle only shows when Flu A is selected).
  # ===========================================================================
  observeEvent(input$sidebar, {
    req(input$sidebar) # Ensure input$sidebar is not NULL
    toggle(id = "overview_controls", condition = input$sidebar == 'overview')
  })
  
  observeEvent(input$pathogen, {
    req(input$pathogen)
    toggle(id = "flu_switch_A_panel", condition = input$pathogen == 'Flu A')
    toggle(id = "flu_switch_B_panel", condition = input$pathogen == 'Flu B')
  })
  
  observe({
    toggle(id = "region_panel", 
           condition = !is.null(input$tab) && !is.null(input$region_toggle) && 
             input$tab == 'Region' && input$region_toggle == 'Each Region')
    toggle(id = "sewershed_panel", 
           condition = !is.null(input$tab) && input$tab == 'Sewershed')
  })
  
  flu_toggle_message <- reactive({
    current_target <- target() 
    
    if (current_target == "infa") {
      compare_to_pathogen_code <- "infb"
      h5_message = "To provide a perspective of
      current Influenza A(H5) (avian influenza) levels with overall Influenza A,
      toggle the ‘Include Flu A (H5)’ switch on the left sidebar."
    } else if (current_target == "infb") {
      compare_to_pathogen_code <- "infa"
      h5_message = ""
    } else {
      return("")
    }
    
    replace_flu <- function(text) {
      text <- gsub("infa", "Flu A", text, ignore.case = TRUE)
      text <- gsub("infb", "Flu B", text, ignore.case = TRUE)
      return(text)
    }
    
    current_pathogen_name <- replace_flu(current_target)
    compare_to_pathogen_name <- replace_flu(compare_to_pathogen_code)
    
    tagList(
      tags$p(style = "font-size: 22px;",
             paste0(
               "To provide a perspective of the current level of ",
               current_pathogen_name,
               " compared with ",
               compare_to_pathogen_name,
               ", turn on the 'Include ",
               compare_to_pathogen_name,
               "' switch button on the left sidebar."
             )
      ),
      tags$p(style = "font-size: 22px;", h5_message)
    )
  })

  # ===========================================================================
  # 4. PLOT SIGNAL HELPERS ----
  # single_plot_signal(): Returns TRUE when only one line should be drawn
  #   (i.e., no dual-target overlay is active). Used to suppress the legend
  #   and simplify subplot layout when viewing a single pathogen.
  # ===========================================================================
  
  single_plot_signal <- reactive({
    if (target() %in% c("n", "rsv")) {
      TRUE
    } else if (!input$flu_switch_B && target() %in% c("infb")) {
      TRUE
    } else if (!input$flu_switch_A && !input$h5_switch && target() %in% c("infa")) {
      TRUE
    } else {
      FALSE
    }
  })

  # ===========================================================================
  # 5. TECHNICAL NOTES TABLES ----
  # levelsTable : DT table showing WVAL thresholds per pathogen/level category.
  #               Column headers are color-coded to match map legend.
  # trendsTable : DT table mapping 21-day % change ranges to trend categories.
  # ===========================================================================

  output$levelsTable <- renderDT({
    datatable(
      levels_data,
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(targets = "_all", render = JS(
            "function(data, type, row, meta) {",
            "  return '<div style=\"white-space: nowrap;\">' + data + '</div>';",
            "}"
          ))
        ),
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  var colors = {",
          "    'Very Low': '#BAE8DE',",
          "    'Low': '#B8E5AC',",
          "    'Moderate': '#FEA82F',",
          "    'High': '#F45B53',",
          "    'Very High': '#C15C9C'",
          "  };",
          "  $(thead).find('th').each(function(i) {",
          "    var colName = $(this).text().trim();",
          "    if (colors[colName]) {",
          "      $(this).css({'background-color': colors[colName], 'color': 'black'});",
          "    }",
          "  });",
          "}"
        )
      ),
      rownames = FALSE,
      elementId = "note-level-table"
    )
  })

  output$trendsTable <- renderDT({
    datatable(trends_data, options = list(dom = 't', ordering = FALSE, paging = FALSE, searching = FALSE, 
                                          columnDefs = list(list(width = '50px', targets = "_all",
                                                                 className = 'dt-center')
                                          )
    ), 
    rownames = FALSE, colnames = c('21-day Percent Change Estimate', 'Trend Category', 'Trend Symbol on the Map')) %>%
      formatStyle(
        columns = names(trends_data),  
        fontSize = '20px'  
      )
  })
  
  # ===========================================================================
  # 6. NAVIGATION OBSERVERS ----
  # Catch clicks on hyperlinks in the About / Instructions pages and route
  # the user to the correct sidebar tab. Also handles homepage info-box clicks:
  # clicking a pathogen box updates the pathogen selector and navigates to
  # the Respiratory Virus Data > Statewide view.
  # ===========================================================================

  observeEvent(input$about_dashboard_link, {
    updateTabItems(session, "sidebar", selected = "technical_notes")
  })
  
  observeEvent(input$instructions_link, {
    updateTabItems(session, "sidebar", selected = "instructions")
  })
  
  observeEvent(input$virus_link, {
    updateTabItems(session, "sidebar", selected = "overview")
  })
  
  rv_text <- reactiveValues(
    # state_visible = TRUE,
    region_visible = TRUE,
    sewershed_visible = TRUE
  )
  
  observeEvent(input$toggle_sewershed_description, {
    rv_text$sewershed_visible <- !rv_text$sewershed_visible
  })

  
  observe({
    shinyjs::toggle(id = "collapsible_text_sewershed", condition = rv_text$sewershed_visible, anim = TRUE)
  })
  
  observeEvent(input$toggle_state_description, {
    shinyjs::toggle(id = "collapsible_text_state", anim = TRUE) # anim = TRUE adds a nice fade effect
  })
  
  observeEvent(input$toggle_region_description, {
    shinyjs::toggle(id = "collapsible_text_region", anim = TRUE)
  })
  
  # ===========================================================================
  # 7. SHINYALERT — ANNOUNCEMENT MODAL ----
  # A dismissible info modal shown when the user clicks "Announcement".
  # Currently used to note that the v2 dashboard is under review.
  # To auto-show on load: uncomment the do.call(shinyalert, alert_params) line.
  # ===========================================================================

  alert_params = list(
    title = HTML('<div style="font-size: 26px; color: #000;">Announcement</div>'),  # Dark black title
    text = HTML('<div style="font-size: 20px; color: #000;">The updated dashboard is currently under review and may undergo additional changes over the coming weeks</div>'),  # Dark black text
    type = "info",
    html = TRUE,  
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "Close",  # Set button text without HTML
    className = 'alert'
  )
  
  observeEvent(input$announce_button, {
    do.call(shinyalert, alert_params)
  })
  
  # ===========================================================================
  # 8. DASHBOARD UPDATE FEED ----
  # Reads the dashboard_update_table CSV (loaded in global.R) and renders
  # a chronological list of update entries (date + title + HTML message).
  # ===========================================================================

  output$dash_update <- renderUI({
    formatted_dates <- dash_update_data$date
    
    
    # Use the external dataset
    entries <- lapply(1:nrow(dash_update_data), function(i) {
      div(class = "entry",
          tags$b(formatted_dates[i]), tags$br(),  # Display formatted date
          tags$b(dash_update_data$title[i]), tags$br(),
          HTML(dash_update_data$message[i])
      )
    })
    
    # Return the list of entries to be rendered
    do.call(tagList, entries)
  })

  # ===========================================================================
  # 9. HOMEPAGE INFO BOXES ----
  # create_clickable_box(): Builds a colored div styled as a shinydashboard
  #   "small-box" that shows pathogen name, current level, and trend.
  #   Clicking navigates to the Respiratory Virus Data tab for that pathogen.
  #   Keyboard accessible via onkeydown (Enter/Space) for ADA compliance.
  # home_covid / home_fluA / home_fluB / home_rsv: Rendered using global
  #   scalar values (covid_state_level, etc.) computed in global.R.
  # ===========================================================================

  create_clickable_box <- function(pathogen_name, display_name, level = NULL, trend, color, box_id) {
    onclick_js <- sprintf("Shiny.setInputValue('box_clicked', '%s', {priority: 'event'});", pathogen_name)
    onkeydown_js <- sprintf("if(event.keyCode === 13 || event.keyCode === 32) { %s }", onclick_js)
    
    div(
      id = box_id,
      role = "button",
      tabindex = "0",
      onkeydown = onkeydown_js,
      style = "cursor: pointer;",
      onclick = onclick_js,
      
      div(
        class = "small-box",
        style = paste("background-color:", color, "; color: black; border: 5px solid #3c3d45; border-radius: 8px; min-height: 160px;"),
        div(
          class = "inner",
          h3(display_name),
          tags$div(
            class = "home-box-text",
            style = "word-wrap: break-word; overflow-wrap: break-word; white-space: normal; max-width: 100%;",
            if (!is.null(level)) {
              list(tags$b(paste0("Level: ", level), style = "font-size: 24px;"), br())
            },
            tags$b(paste0("Trend: ", trend), style = "font-size: 24px;")
          )
        )
      )
    )
  }

  output$home_covid <- renderUI({
    create_clickable_box("SARS-CoV-2", rename_pathogen("n"), covid_state_level, covid_state_trend, getStateColor("n", covid_state_level), box_id = "home_covid")
  })
  
  output$home_fluA <- renderUI({
    create_clickable_box("Flu A", rename_pathogen("infa"), infA_state_level, infA_state_trend, getStateColor("infa", infA_state_level), box_id = "home_fluA")
  })
  
  output$home_fluB <- renderUI({
    create_clickable_box("Flu B", rename_pathogen("infb"), infB_state_level, infB_state_trend, getStateColor("infb", infB_state_level), box_id = "home_fluB")
  })
  
  output$home_rsv <- renderUI({
    create_clickable_box("RSV", rename_pathogen("rsv"), rsv_state_level, rsv_state_trend, getStateColor("rsv", rsv_state_level), box_id = "home_rsv")
  })
  
  observeEvent(input$box_clicked, {
    req(input$box_clicked)
    
    updateSelectInput(session, 
                      inputId = "pathogen", 
                      selected = input$box_clicked)
    updateTabItems(session, 
                   inputId = "sidebar", 
                   selected = "overview")
    updateTabsetPanel(session,
                      inputId = "tab",
                      selected = "Statewide")
  })
  
  # ===========================================================================
  # 10. STATE SUMMARY UI ----
  # state_summary_ui: Full renderUI for the Statewide tab — includes the
  #   collapsible description, flu toggle message, state-level info box,
  #   trend box, and the state_plot plotly output.
  # h5_toggle_message(): Contextual banner shown when Flu A is selected,
  #   explaining the H5 overlay toggle in the sidebar.
  # ===========================================================================
  
  h5_toggle_message <- reactive({
    if (target() == "infa") {
      h5(
        "Influenza A levels reflect a range of subtypes that may enter wastewater from both human and 
        animal sources. The Flu A (H5) toggle overlay provides additional context for interpreting 
        increases in Influenza A, particularly when they may be driven by subtypes like H5 circulating 
        among animals.",
        style = "
        font-size: 18px;                      /* Increases font size */
        border: 2px solid black;            /* Adds a light gray border */
        border-radius: 10px;                  /* Rounds the corners */
        padding: 15px;                        /* Adds space inside the border */
        text-align: center;                   /* Centers the text */
        background-color: #fac66b; /* Adds a light blue background */
      "
      )
    } else {
      NULL
    }
  })
  
  output$h5_message_region_1 <- renderUI({  h5_toggle_message() })
  output$h5_message_region_2 <- renderUI({  h5_toggle_message() })
  output$h5_message_sewershed <- renderUI({  h5_toggle_message() })
  
  output$state_summary_ui <- renderUI({
    tagList(
      h1(
        strong(
          paste0(
            "State Summary of California for ", rename_pathogen(target()),
            " (Last Update: ", published_date_state[[target()]], ")"
          )
        ),
        style = "color: black;"
      ),
      actionLink(inputId = "toggle_state_description", "Click here to see or hide the description", 
                 style = "font-size: 20px; color: #2A4058; font-weight: bold;"),
      div(
        id = "collapsible_text_state",
        style = "background-color: #e9e9e9; padding: 5px 15px; border-radius: 10px;",
        
        tags$p(
          style = "font-size: 22px; margin-top: 10px;",
          "The statewide", rename_pathogen(target()), "wastewater concentrations aggregates data from all participating surveillance sites across California.
         The current wastewater level and its trend are determined by comparing this aggregated data to the levels
         21 days prior.",
          "Wastewater surveillance is one metric to consider alongside other metrics and does not represent severity of disease. For more information on clinical indicators for respiratory viruses, see the ",
          tags$a(
            href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx",
            "Respiratory Virus Dashboard",
            target = "_blank",
            style = "color: #00008B;"
          )
        ),
        flu_toggle_message()
      ),
      br(),
      fluidRow(
        column(
          width = 4,
          class = "state-info-container",
          fluidRow(
            div(style = "margin-top: 10px;"),
            uiOutput("state_info_box", width = 10),
            br(),
            uiOutput("trend_box", width = 10)
          )
        ),
        column(
          width = 8,
          h5_toggle_message(),
          plotlyOutput("state_plot", height = "700px") %>%
            withSpinner(color = "#5A789A"),
          style = "padding-right: 15px; padding-left: 45px;"
        )
      )
    )
  })
  
  # ===========================================================================
  # 11. REGION SUMMARY UI ----
  # region_content: Renders the header and collapsible description for the
  #   Region tab. The flu_toggle_message() is embedded inside.
  # ===========================================================================

  output$region_content <- renderUI({
    
    # Title
    fluidRow(
      div(
        style = "background-color: white; padding: 15px;",
        tagList(
          h1(
            strong(
              paste0(
                "Regional Trends and Levels of ",
                rename_pathogen(target()),
                " in California",
                " (Last Update: ", published_date[[input$tab]][[target()]], ")"
              )
            ),
            style = "color: black;"
          ),
          actionLink(
            inputId = "toggle_region_description", 
            "Click to show or hide the description", 
            style = "font-size: 20px; color: #2A4058; font-weight: bold;"
          ),
          div(
            id = "collapsible_text_region",
            style = "background-color: #e9e9e9; padding: 5px 15px; border-radius: 10px;",
            tags$p(
              style = "font-size: 22px; margin-top: 10px;",
              "Select ",
              strong("Overview"),
              " to view concentrations for all regions. To explore a specific region, choose ",
              strong("Each Region"),
              " and then click a region from the map or select a region from the dropdown menu to see its trend and level data. 
                                                                    The current wastewater level and its trend are determined by comparing this aggregated data to the levels recorded 21 days ago.",
              
              " Wastewater surveillance is one metric to consider alongside other metrics and does not represent severity of disease. For more information on clinical indicators forespiratory viruses, see the ",
              tags$a(
                href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx", 
                "Respiratory Virus Dashboard",
                target = "_blank",
                style = "color: #00008B;"
              ),
              
              flu_toggle_message()
            )
          )
        )
      )
    )
  })

  # ===========================================================================
  # 12. TARGET SELECTOR & PATHOGEN-FILTERED DATA ----
  #
  # target(): Converts the human-readable pathogen label from input$pathogen
  #   (e.g. "SARS-CoV-2") into the internal pcr_gene_target code
  #   ("n", "infa", "infb", "rsv"). This is THE central reactive — almost every
  #   downstream reactive calls target() to know what to filter on.
  #
  # c11()-c55(): Convenience reactives that filter the global c1..c5 data
  #   frames (defined in global.R, section 7) to the current target(). Names
  #   intentionally mirror the globals: c11() == c1 filtered, c22() == c2
  #   filtered, and so on. In short:
  #     c1  = regional aggregate time series (WVAL)      -> c11()
  #     c2  = site-level metrics (level + trend)         -> c22()
  #     c3  = c2 joined to sewershed/region shapefiles   -> c33()
  #     c4  = c3 flattened for sewershed summary tables  -> c44()
  #     c5  = c4 with a handful of excluded sewersheds   -> c55()
  #   See global.R lines ~283-476 for how the base frames are built.
  # ===========================================================================

  # Map UI pathogen label -> internal pcr_gene_target code.
  targetlist = setNames(c("SARS-CoV-2", "Flu A", "Flu B", "RSV"), c("n", "infa", "infb", "rsv"))
  target = reactive({ names(targetlist[targetlist == input$pathogen]) })
  
  output$overview_title_sewershed <- renderUI({
    tagList(
      h1(
        strong(
          paste0(
            "Trends of ",
            rename_pathogen(target()),
            " for Sewersheds in California",
            " (Last Update: ", published_date[[input$tab]][[target()]], ")"
          )
        ),
        style = "color: black;"
      ),
      actionLink(inputId = "toggle_sewershed_description", 
                 "Click to show or hide the description", 
                 style = "font-size: 20px; color: #2A4058; font-weight: bold;"),
      
      div(
        id = "collapsible_text_sewershed",
        style = "background-color: #e9e9e9; padding: 5px 15px; border-radius: 10px;",
        
        tags$p(
          style = "font-size: 22px; margin-top: 10px;",
          
          "To view the trend of each sewershed and wastewater concentration, use the dropdown on the left sidebar or click the sewershed on the map. The current wastewater trends are determined by comparing this data to the levels 21 days prior.",
          
          "Wastewater surveillance is one metric to consider alongside other metrics and does not represent severity of disease. For more information on clinical indicators for respiratory viruses, see the ",
          tags$a(
            href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx", 
            "Respiratory Virus Dashboard",
            target = "_blank",
            style = "color: #00008B;"
          ),
          "."
        ),
        flu_toggle_message(),
        tags$div(
          style = "margin-top: 20px;", 
          tags$span(
            style = "background-color: #fff3cd; color: #856404; padding: 10px; display: block; border-left: 6px solid #ffeeba; font-weight: bold; font-size: 24px;",
            "Note: Sewershed-level metrics are currently under development and will be available on the dashboard soon."
          )
        ),
        br(),
      )
    )
  })
  
  # c1..c5 filtered to the currently-selected pathogen.
  # See comment block above for what each base frame contains.
  c11 <- reactive({ datafilter(c1, value = target()) })  # regional aggregate WVAL series
  c22 <- reactive({ datafilter(c2, value = target()) })  # site-level metrics (level/trend)
  c33 <- reactive({ datafilter(c3, value = target()) })  # c2 + geometry (used by maps)
  c44 <- reactive({ datafilter(c4, value = target()) })  # c3 flattened to a plain data.frame
  c55 <- reactive({ datafilter(c5, value = target()) })  # c4 with known-bad sewersheds removed
  
  output$table_header <- renderUI({
    h2(strong(paste0(rename_pathogen(target()), " : ", "Trend Summary Table for Each Sewershed (Last Update: ", published_date[["Sewershed"]][[target()]], ")")), align = "center")
  })
  output$region_table_header <- renderUI({
    h2(strong(paste0(rename_pathogen(target()), " : ", "Trend and Level Summary Table for Each Region and Statewide (Last Update: ", published_date[["Region"]][[target()]], ")")), align = "center")
  })
  output$publish_note <- renderUI({
    p(
      style = "font-size: 20px;",
      strong(rename_pathogen("n")), ": ", published_date_state$n, " for statewide; ", 
      published_date_region$n, " for regions; ", published_date_wwtp$n, " for sewersheds.",
      tags$br(),  # Add a line break
      tags$i("Please note that the state metrics may be updated at a different time than the regional metrics.")
    )
  })

  # ===========================================================================
  # 13. DROPDOWN REACTIVES ----
  # statedropdown()      : Builds a grouped list of sewershed Label_Names
  #                        ordered by region for the sewershed picker.
  # filtered_labels()    : Filters statedropdown() to the selected region
  #                        (input$SHO). Returns all regions when "Statewide".
  # dynamic_HO_picker    : Region dropdown for the Region tab (input$HO).
  #                        Excludes RANCHO for Flu B (no data).
  # dynamic_SHO_picker   : Region dropdown for the Sewershed tab (input$SHO).
  # dynamic_wwtp_picker  : Sewershed picker, grouped by region when Statewide.
  # ===========================================================================
  wwtp_factor = reactive({
    as.data.frame(c33()) %>%
      select(wwtp_name, Shape_Area) %>%
      distinct() %>%
      arrange(-Shape_Area) %>% pull(wwtp_name)
  })
  
  output$dynamic_title <- renderUI({
    req(target(), published_date_region, published_date_state, published_date_wwtp)
    titlePanel(strong(paste0(" Statewide Wastewater Surveillance "#, 
    )))
  })
  
  statedropdown <- reactive({
    x2 <- c22() %>%
      filter(region != "State", !is.na(Label_Name), !is.na(level)) %>%
      distinct(region, Label_Name) %>%
      mutate(region = factor(region, levels = region_vec)) %>%
      { split(.$Label_Name, .$region) } %>%
      lapply(sort)
    
    names(x2) <- regname(names(x2), reverse = TRUE)
    return(x2)
  })
  
  filtered_labels <- reactive({
    req(input$SHO)  
    if(input$SHO == "Statewide"){
      return(statedropdown())
    } else if (input$SHO != "Statewide") {
      return(sort(na.omit(unique(c22() %>% filter(region == regname(input$SHO), !is.na(level)) %>% .$Label_Name))))
    }
  })
  
  output$dynamic_HO_picker <- renderUI({
    if(!target() %in% "infb") {
      selectInput(
        inputId = "HO",
        label = "Select a Region",
        choices = region_choice, 
        selected = region_choice[1]
      )
    } else {
      selectInput(
        inputId = "HO",
        label = "Select a Region",
        choices = c(region_choice[c(1,3,4,5,6)]), 
        selected = region_choice[1],
      )
    }
  })
  
  output$dynamic_SHO_picker <- renderUI({
    if(!target() %in% c("infb")) {
      selectInput(
        inputId = "SHO",
        label = "Select a Region",
        choices = c("Statewide", region_choice), 
        selected = "Statewide"
      )
    } else {
      selectInput(
        inputId = "SHO",
        label = "Select a Region",
        choices = c("Statewide", region_choice[c(1, 3, 4,5, 6)]), 
        selected = "Statewide"
      )
    }
  })
  
  output$dynamic_wwtp_picker <- renderUI({
    req(filtered_labels())
    selectInput(
      inputId = "select_wwtp",
      label = span(
        "Choose a Wastewater Sewershed by County (Utility Name) ",
        div(id = "sewershed_dropdown",
            style = "display:inline-block;",
            title = "Please note that the sites on the map may be fewer than those in the
              dropdown due to the lack of shapefiles for all sewersheds",
            icon("info-circle"))
      ),
      choices = filtered_labels(),
      multiple = FALSE
    )
  })

  # ===========================================================================
  # 14. MAP DATA REACTIVES ----
  # mapdf()          : Filters state_df to selected pathogen for region map.
  # smapdf()         : Filters c33() to selected region/pathogen for sewershed map.
  # iconList()       : awesomeIcons for region map — icon shape encodes trend,
  #                    marker color encodes level.
  # siconList()      : awesomeIcons for sewershed map — all markers are lightgray
  #                    (level shown via polygon fill, not marker color).
  # sewershed_label(): HTML tooltip showing name, level, trend for region map.
  # s_sewershed_label(): Simplified HTML tooltip for sewershed map.
  # pal_value()      : Reactive colorFactor palette formula for region polygons.
  # spal_value()     : Reactive colorFactor palette formula for sewershed polygons.
  # map_legend / s_map_legend: Static HTML legend controls added to each map.
  # ===========================================================================

  mapdf = reactive({
    state_df %>% filter(!is.na(wwtp_name), pcr_gene_target == target(),
                        report_include == T)
  })
  
  smapdf <- reactive({
    req(input$SHO, target(), wwtp_factor())
    if (input$SHO == "Statewide")
    {
      c33() %>% filter(
        !is.na(wwtp_name), pcr_gene_target == target(),
        report_include == T, !is.na(level)) %>%
        mutate(wwtp_name = factor(wwtp_name, levels = wwtp_factor())) %>%
        arrange(wwtp_name)
    } else if (input$SHO != "Statewide") {
      c33() %>% filter(region == regname(input$SHO),
                       !is.na(wwtp_name), pcr_gene_target == target(),
                       report_include == T, !is.na(level)) %>%
        mutate(wwtp_name = factor(wwtp_name, levels = wwtp_factor())) %>%
        arrange(wwtp_name)
    }
  })
  
  # ---------------------------------------------------------------------------
  # Trend tallies (used by the statewide "N sites increasing / decreasing / …"
  # bullet list). trend_vec() counts sewersheds in c55() by trend string, then
  # trend_increase/decrease/plateau/low bucket them into the four display
  # categories. `demon` is the denominator (total sites with a trend call in
  # the past 21 days) used for the percentages.
  # ---------------------------------------------------------------------------
  trend_vec    <- reactive({ table(c55()$trend) })
  names_trend  <- reactive({ names(trend_vec()) })

  trend_decrease <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Decrease", ignore_case = T))])
  })
  trend_increase <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Increase", ignore_case = T))])
  })
  trend_plateau <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Plateau", ignore_case = T))])
  })
  trend_low <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("(Sporadic Detections|All samples below LOD)", ignore_case = T))])
  })
  not_enough <- reactive({
    sum(trend_vec()[str_detect(names_trend(), "Not enough data")])
  })

  demon <- reactive({
    sum(trend_increase(), trend_decrease(), trend_plateau(), trend_low())
  })

  total_sites <- reactive({
    paste0("Number of sites reporting data in past 21 days: ", demon())
  })
  increasing <- reactive({
    paste0("Increasing at ", trend_increase(), "/", demon(), " sites (",
           round(trend_increase()/demon()*100, 0), "%)")
  })
  decreasing <- reactive({
    paste0("Decreasing at ", trend_decrease(), "/", demon(), " sites (",
           round(trend_decrease()/demon()*100, 0), "%)")
  })
  plateauing <- reactive({
    paste0("Plateauing at ", trend_plateau(), "/", demon(), " sites (",
           round(trend_plateau()/demon()*100, 0), "%)")
  })
  not_enough_2 <- reactive({
    paste0("Concentrations too low to define trends ", trend_low(), "/", demon(), " sites (",
           round(trend_low()/demon()*100, 0), "%)")
  })

  # ---------------------------------------------------------------------------
  # Map view centers. z() is the default statewide view; sz() recenters the
  # sewershed map when the user picks a region from the SHO dropdown
  # (coordinates come from `zoomlist` in global.R).
  # ---------------------------------------------------------------------------
  z = reactive({
    list("lng" = -120.3384, "lat" = 37.06523, "zoom" = 6)
  })

  sz = reactive({
    list("lng" = zoomlist[[regname(input$SHO)]][1],
         "lat" = zoomlist[[regname(input$SHO)]][2],
         "zoom" = zoomlist[[regname(input$SHO)]][3])
  })
  
  iconList <- reactive({
    
    trends <- mapdf()$trend2
    levels <- mapdf()$level
    
    icons <- case_when(
      str_detect(trends, "Very Strong Increase") ~ "arrow-up",
      str_detect(trends, "Strong Increase") ~ "arrow-up",
      str_detect(trends, "Increase") ~ "arrow-up",
      str_detect(trends, "Plateau") ~ "arrows-h",
      str_detect(trends, "Decrease") ~ "arrow-down",
      str_detect(trends, "Sporadic Detections") ~ "ellipsis-h",
      str_detect(trends, "All Samples Below LOD") ~ "circle-o",
      str_detect(trends, "Not enough data") ~ "x",
      TRUE ~ "minus"
    )
    
    colors <- case_when(
      levels == "Very High" ~ "purple",
      levels == "High" ~ "darkred",
      levels == "Moderate" ~ "orange",
      levels == "Low" ~ "green",
      levels == "Very Low" ~ "blue",
      TRUE ~ "gray"
    )
    
    awesomeIcons(
      icon = icons,
      iconColor = "black",
      markerColor = colors,
      library = "fa"
    )
    
  })

  siconList <- reactive({
    req(smapdf())
    
    trends <- smapdf()$trend2
    levels <- smapdf()$level
    
    icons <- case_when(
      str_detect(trends, "Very Strong Increase") ~ "arrow-up",
      str_detect(trends, "Strong Increase") ~ "arrow-up",
      str_detect(trends, "Increase") ~ "arrow-up",
      str_detect(trends, "Plateau") ~ "arrows-h",
      str_detect(trends, "Decrease") ~ "arrow-down",
      str_detect(trends, "Sporadic Detections") ~ "ellipsis-h",
      str_detect(trends, "All Samples Below LOD") ~ "circle-o",
      str_detect(trends, "Not enough data") ~ "x",
      TRUE ~ "minus"
    )
    
    colors <- rep("lightgray", length(levels))
    
    awesomeIcons(
      icon = icons,
      iconColor = "black",
      markerColor = colors,
      library = "fa"
    )
    
  })
  
  sewershed_label <-
    reactive({
      df <- mapdf()
      sprintf(
        "<strong>%s</strong><br/><span style='font-weight:normal;'>Level: </span><strong>%s</strong><br/><span style='font-weight:normal;'>Trend: </span><strong>%s</strong>",
        regname(df$wwtp_name, reverse = T),
        df$level,
        df$trend
      ) %>%
        lapply(htmltools::HTML)
    })
  
  s_sewershed_label <- reactive({
    df <- smapdf()
    sprintf(
      "<strong>%s</strong><br/><span style='font-weight:normal;'>Trend: </span><strong>%s</strong>",
      df$wwtp_name,
      df$trend
    ) %>%
      lapply(htmltools::HTML)
  })
  
  pal_value <- reactive({
    ~pal[[target()]](mapdf()$level)
  })
  
  spal_value <- reactive({
    ~spal[[target()]](smapdf()$level)
  })
  
  map_legend <- HTML('
            <div style="background: rgba(255, 255, 255, 0.4); padding: 9px; border-radius: 5px;">
              <strong>Trend</strong><br>
              <i class="fa fa-arrow-up" style="color:black;"></i> Increase<br>
              <i class="fa fa-arrows-h" style="color:black;"></i> Plateau<br>
              <i class="fa fa-arrow-down" style="color:black;"></i> Decrease<br>
              <i class="fa fa-ellipsis-h" style="color:black;"></i> Sporadic Detections<br>
              <i class="fa fa-circle-o" style="color:black;"></i> All Samples Below LOD<br>
              <i class="fa fa-times" style="color:black;"></i> Not enough data<br>
              
              <strong>Level</strong><br>
              <div style="display: flex; align-items: center;">
                <div style="background-color: #C15C9C; width: 20px; height: 20px; margin-right: 8px;"></div> Very High<br>
              </div>
              <div style="display: flex; align-items: center;">
                <div style="background-color: #F45B53; width: 20px; height: 20px; margin-right: 8px;"></div> High<br>
              </div>
              <div style="display: flex; align-items: center;">
                <div style="background-color: #FEA82F; width: 20px; height: 20px; margin-right: 8px;"></div> Moderate<br>
              </div>
              <div style="display: flex; align-items: center;">
                <div style="background-color: #B8E5AC; width: 20px; height: 20px; margin-right: 8px;"></div> Low<br>
              </div>
              <div style="display: flex; align-items: center;">
                <div style="background-color: #BAE8DE; width: 20px; height: 20px; margin-right: 8px;"></div> Very Low<br>
              </div>
              <div style="display: flex; align-items: center;">
                <div style="background-color: gray; width: 20px; height: 20px; margin-right: 8px;"></div> Insufficient Data
              </div>
            </div>
            ')
  
  s_map_legend <- HTML('
      <div style="background: rgba(255, 255, 255, 0.4); padding: 9px; border-radius: 5px;">
        <strong>Trend</strong><br>
        <i class="fa fa-arrow-up" style="color:black;"></i> Increase<br>
        <i class="fa fa-arrows-h" style="color:black;"></i> Plateau<br>
        <i class="fa fa-arrow-down" style="color:black;"></i> Decrease<br>
        <i class="fa fa-ellipsis-h" style="color:black;"></i> Sporadic Detections<br>
        <i class="fa fa-circle-o" style="color:black;"></i> All Samples Below LOD<br>
        <i class="fa fa-times" style="color:black;"></i> Not enough data<br>
      </div>
    ')
 
  # ===========================================================================
  # 15. LEAFLET MAP OUTPUTS ----
  # heatmap_region   : Region-level choropleth + awesome marker map.
  #                    Polygon fill = level; marker icon = trend direction.
  # heatmap_region_2 : Identical map used in the "Overview" sub-panel
  #                    (needed because Shiny cannot reuse an output ID).
  # heatmap_sewershed: Sewershed-level map. Polygon fill uses spal (neutral
  #                    beige); marker icon encodes trend. User clicks update rv2().
  # Dynamic height   : observe() block resizes map divs via runjs() based on
  #                    screen width (responsive layout for mobile).
  # ===========================================================================
  output$heatmap_region <- renderLeaflet({
    
    req(mapdf(), z(), pal_value(), sewershed_label(), iconList())
    
    df <- mapdf() 
    z <- z()
    
    marker_label <- sprintf(
      "<strong>%s</strong>",
      df$wwtp_name
    ) %>%
      lapply(htmltools::HTML)
    
    leaflet(df) %>%
      setView(lng = z$lng, lat = z$lat, zoom = z$zoom)  %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = pal_value(),
        weight = 1,
        opacity = 1,
        color = "#666",
        dashArray = "1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = sewershed_label(),
        layerId = df$wwtp_name,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-refresh",
        title = "Reset View",
        onClick = JS(paste0("
          function(btn, map) {
            map.setView([", z$lat, ", ", z$lng, "], ", z$zoom, ");
          }"))
      )) %>%
      addAwesomeMarkers(
        lat = ~lat,
        lng = ~lng,
        icon = iconList(),
        layerId = ~df$wwtp_name,
        label = sewershed_label(),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addControl(
        html = map_legend,
        position = "topright"
      )
  })

  # NOTE: heatmap_region_2 is intentionally a duplicate of heatmap_region.
  # Shiny requires a distinct outputId for each location the map appears, and
  # this map is rendered in two tabs (Region > Each Region AND Region > Overview).
  # If you edit one, edit both. A future cleanup could factor the leaflet call
  # into a helper that both outputs delegate to.
  output$heatmap_region_2 <- renderLeaflet({

    req(mapdf(), z(), pal_value(), sewershed_label(), iconList())
    
    df <- mapdf() 
    z <- z()
    
    marker_label <- sprintf(
      "<strong>%s</strong>",
      df$wwtp_name
    ) %>%
      lapply(htmltools::HTML)
    
    leaflet(df) %>%
      setView(lng = z$lng, lat = z$lat, zoom = z$zoom)  %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = pal_value(),
        weight = 1,
        opacity = 1,
        color = "#666",
        dashArray = "1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = sewershed_label(),
        layerId = df$wwtp_name,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-refresh",
        title = "Reset View",
        onClick = JS(paste0("
          function(btn, map) {
            map.setView([", z$lat, ", ", z$lng, "], ", z$zoom, ");
          }"))
      )) %>%
      addAwesomeMarkers(
        lat = ~lat,
        lng = ~lng,
        icon = iconList(),
        layerId = ~df$wwtp_name,
        label = sewershed_label(),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addControl(
        html = map_legend,
        position = "topright"
      )
  })

  output$heatmap_sewershed <-
    
    renderLeaflet({
      req(smapdf(), sz(), spal_value(),  s_sewershed_label(), siconList())
    
      df <- smapdf() 
      sz <- sz()
      
      marker_label <- sprintf(
        "<strong>%s</strong>",
        df$wwtp_name
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet(df) %>%
        setView(lng = sz$lng, lat = sz$lat, zoom = sz$zoom) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = spal_value(),
          weight = 1,
          opacity = 1,
          color = "#666",
          dashArray = "1",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = s_sewershed_label(),
          layerId = df$wwtp_name,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addEasyButton(easyButton(
          icon = "fa-refresh",
          title = "Reset View",
          onClick = JS(paste0(
            "function(btn, map) {
             map.setView([", sz$lat, ", ", sz$lng, "], ", sz$zoom, ");
           }"))
        )) %>%
        addAwesomeMarkers(
          lat = ~lat,
          lng = ~lng,
          icon = siconList(),
          layerId = ~wwtp_name,
          label = s_sewershed_label(),
          labelOptions = labelOptions(
            style = list("font-weight" = "bold", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addControl(html = s_map_legend, position = "topright")
      
    })
  
  # ===========================================================================
  # 16. MAP-CLICK SELECTION REACTIVES ----
  #
  # The dashboard lets the user pick a region or a sewershed two ways:
  #   1. Choose from a dropdown in the sidebar (input$HO for region,
  #      input$SHO for sewershed region filter, input$select_wwtp for sewershed).
  #   2. Click directly on a polygon or marker in one of the Leaflet maps
  #      (input$heatmap_region_*_click, input$heatmap_sewershed_*_click).
  #
  # Four reactiveVals remember the "current selection" and are kept in sync by
  # observeEvent handlers that fire on either trigger:
  #   rv   -> selected sewershed name, drives the sewershed time-series plot
  #   rv2  -> selected sewershed name, drives the sewershed info box
  #           (see section 21; defined there because the info box needs it)
  #   rv3  -> selected region name, drives the region info box (section 21)
  #   re   -> selected region name, drives the region time-series plot
  # rv and rv2 track the same thing but are kept separate so the plot and box
  # can live in different sections without a reactive-dependency cycle.
  #
  # plot_site_data(): w1() filtered to the selected sewershed (rv()) and the
  #   pathogen(s) currently on screen.
  # metric_plotdf() : c1 filtered to the selected region (re()) for the
  #   "Each Region" plot. Falls back to ABAHO for RANCHO+Flu B (no data).
  #
  # This block also contains a dynamic-height observer that resizes the three
  # leaflet divs via runjs() based on the active pathogen.
  # ===========================================================================
  observe({
    if (target() == "infa") {
      heights <- list(region_overview = 980, region_each = 880, sewershed = 900)
    } else {
      heights <- list(region_overview = 900, region_each = 780, sewershed = 780)
    }
    runjs(paste0("document.getElementById('heatmap_region_2').style.height = '", heights$region_overview, "px';"))
    runjs(paste0("document.getElementById('heatmap_region').style.height = '", heights$region_each, "px';"))
    runjs(paste0("document.getElementById('heatmap_sewershed').style.height = '", heights$sewershed, "px';"))
  })

  observeEvent(input$heatmap_sewershed_marker_click, {
    if(input$tab == "Sewershed"){
      click <- input$heatmap_sewershed_marker_click
      loc <- smapdf() %>% filter(wwtp_name == click$id)
      leafletProxy("heatmap_sewershed") %>%
        setView(lng = loc$lng, lat = loc$lat, zoom = 11)
    }
  })
  
  observeEvent(input$heatmap_sewershed_shape_click, {
    if(input$tab == "Sewershed"){
      click <- input$heatmap_sewershed_shape_click
      loc <- smapdf() %>% filter(wwtp_name == click$id)
      leafletProxy("heatmap_sewershed") %>%
        setView(lng = loc$lng, lat = loc$lat, zoom = 11)
    }
  })
  
  # Main plot ---------------------------------------------------------------
  
  rv <- reactiveVal(NULL)

  observeEvent(input$select_wwtp, {
    
    req(input$select_wwtp)
    rv(input$select_wwtp)
    
  })
  
  observeEvent(input$heatmap_sewershed_shape_click, {
    rv(gsub(" $", "", input$heatmap_sewershed_shape_click$id))
  })
  
  observeEvent(input$heatmap_sewershed_marker_click, {
    rv(gsub(" $", "", input$heatmap_sewershed_marker_click$id))
  })
  
  plot_site_data = reactive({
    req(rv(), plot_target(), w1())
    
    w1() %>% filter(wwtp_name == rv(),
                    pcr_gene_target %in% plot_target() ) %>%
      mutate(term = factor(term, c("long", "short")))
  })

  re <- reactiveVal(NULL)

  observeEvent(input$HO, {
    re(gsub(" $", "", regname(input$HO)))
  })
  
  observeEvent(input$heatmap_region_shape_click, {
    re(gsub(" $", "", input$heatmap_region_shape_click$id))
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    re(gsub(" $", "", input$heatmap_region_marker_click$id))
  })
  
  metric_plotdf = reactive({
    req(re(), plot_target())
    
    filter_region <- if (re() == "RANCHO" && target() == "infb") {
      "ABAHO"
    } else {
      re()
    }
    
    if(single_plot_signal()) {
      c1 %>% filter(region == filter_region, sample_date > max(sample_date)-730,
                    pcr_gene_target == target()
      )
    } else {
      c1 %>% filter(region == filter_region, sample_date > max(sample_date)-730,
                    pcr_gene_target %in% plot_target()
      )
    }
  })
  
  conversion_factor <- reactive({
    if(target() %in% "n") {
      1
    } else {
      1
    }
  }) 
  
  combine_annotation_list <-
    list(
      list(
        x = 0.5,  # Center the text horizontally
        y = -0.1,  # Position just below the plot (not dependent on the rangeslider)
        yshift = -40,  # Dynamically shift the text downward to ensure it's below the rangeslider
        text = "Adjust the time range using the slider above",  # The text to appear below the rangeslider
        showarrow = FALSE,
        xref = "paper",  # Use paper coordinates for responsiveness
        yref = "paper",  # Use paper coordinates for responsiveness
        xanchor = 'center',  # Anchor text to center
        yanchor = 'top',  # Anchor to the top to prevent overlap with rangeslider
        font = list(size = 16, color = "black")  # Adjust font size and color
      )
    )
  
  single_annotation_list <-
    list(
      list(
        x = 0.5,  # Center the text horizontally
        y = -0.1,  # Position just below the plot (not dependent on the rangeslider)
        yshift = -40,  # Dynamically shift the text downward to ensure it's below the rangeslider
        text = "Adjust the time range using the slider above",  # The text to appear below the rangeslider
        showarrow = FALSE,
        xref = "paper",  # Use paper coordinates for responsiveness
        yref = "paper",  # Use paper coordinates for responsiveness
        xanchor = 'center',  # Anchor text to center
        yanchor = 'top',  # Anchor to the top to prevent overlap with rangeslider
        font = list(size = 16, color = "black")  # Adjust font size and color
      )
    )

  # ===========================================================================
  # 17. PLOT HELPERS ----
  # create_bar_plot(): Builds a stacked Plotly bar chart used as a color-coded
  #   level indicator alongside each time-series line chart. Each stack segment
  #   represents one level band (Very Low → Very High).
  # plot_target()   : Returns a vector of gene target codes to plot, respecting
  #   the flu A/B and H5 toggle switches from the sidebar.
  # dualplot()      : TRUE when two pathogens are overlaid on one chart.
  # conversion_factor(): Reserved for unit scaling (currently 1 for all targets).
  # combine/single_annotation_list: Plotly annotation configs for the
  #   range-slider instruction text shown below each plot.
  # ===========================================================================
  create_bar_plot <- function(stack1, stack2, stack3, stack4, stack5, show_legend = FALSE,
                              color_very_low = "#BAE8DE",
                              color_low = "#B8E5AC",
                              color_moderate = "#FEA82F",
                              color_high = "#F45B53",
                              color_very_high = "#C15C9C",
                              low_base = 0) {
    
    bar_plot <- plot_ly() %>%
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack1 + low_base,
        type = 'bar',
        name = 'Very Low',
        marker = list(color = color_very_low, line = list(color = 'white', width = 1.5)),
        base = -low_base,
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack2,
        type = 'bar',
        name = 'Low',
        marker = list(color = color_low, line = list(color = 'white', width = 1.5)),
        base = stack1,
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack3,
        type = 'bar',
        name = 'Moderate',
        marker = list(color = color_moderate, line = list(color = 'white', width = 1.5)),
        base = stack1 + stack2,
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack4,
        type = 'bar',
        name = 'High',
        marker = list(color = color_high, line = list(color = 'white', width = 1.5)),
        base = stack1 + stack2 + stack3,
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack5,
        type = 'bar',
        name = 'Very High',
        marker = list(color = color_very_high, line = list(color = 'white', width = 1.5)),
        base = stack1 + stack2 + stack3 + stack4,
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      layout(
        xaxis = list(showticklabels = FALSE, title = NULL, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(showticklabels = FALSE, title = NULL, showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 50),
        barmode = 'stack'
      )
    
    return(bar_plot)
  }
  
  plot_target <- reactive({
    targets_to_plot <- c(target())
    
    if (target() == "infa" && isTRUE(input$flu_switch_A)) {
      targets_to_plot <- c(targets_to_plot, "infb")
    } else if (target() == "infb" && isTRUE(input$flu_switch_B)) {
      targets_to_plot <- c(targets_to_plot, "infa")
    }
    
    if (isTRUE(input$h5_switch) && !target() %in% c("n", "rsv")) {
      targets_to_plot <- c(targets_to_plot, "h5")
    }
    unique(targets_to_plot)
  })
  
  dualplot <- reactive({
    if(target() %in% c("n", "rsv")) {
      F
    } else {
      length(plot_target()) > 1 
    }
  })
  
  # ===========================================================================
  # 18. STATE PLOT ----
  # state_plot: Plotly time-series for the statewide WVAL aggregate.
  #   - Reads last 730 days from c1 filtered to selected pathogen(s).
  #   - Draws horizontal dashed lines at q1-q4 threshold values.
  #   - Draws a vertical dashed line at "21 days ago".
  #   - When single pathogen: combines bar_plot + line_plot via subplot().
  #   - When dual pathogen:  line plot only (no bar, thresholds suppressed).
  #   - Injects js_code_plot via htmlwidgets::onRender() for ADA keyboard access.
  # ===========================================================================
  
  state_region_line_label <- reactive({
    if (target() %in% "n"){
      "WVAL Rolling Average"
    } else {
      "WVAL Rolling Average"
    }
  })
  
  output$state_plot = renderPlotly({
    
    plotdf =  c1 %>% filter(region == "State", sample_date > max(sample_date)-730,
                            pcr_gene_target %in% plot_target()) %>%
      mutate(ww_aggregate = ww_aggregate* conversion_factor()) %>%
      rename(`sample date` = sample_date,
             `raw concentration normalized by pmmov` = ww_aggregate) %>%
      arrange(`sample date`)
    
    if(target() %in% c("infa", "infb") & dualplot() == T & single_plot_signal() == F ) {
      h1 = NA
      h2 = NA
      h3 = NA
      h4 = NA
    } else {
      h1 = na.omit(unique(plotdf$q1))[1] * conversion_factor()
      h2 = na.omit(unique(plotdf$q2))[1] * conversion_factor()
      h3 = na.omit(unique(plotdf$q3))[1] * conversion_factor()
      h4 = na.omit(unique(plotdf$q4))[1] * conversion_factor()
    }
    
    region_plot = metric_plot(
      data = plotdf,
      x_col = "sample date",
      y_col = "raw concentration normalized by pmmov",
      hover_label = state_region_line_label(),
      vline_date = max(plotdf$`sample date`) - 21,
      vline_label_position = max(plotdf$`raw concentration normalized by pmmov`, na.rm = T)*0.8,
      vline_label_font = list(size = 14),
      hline_y1 = h1,
      hline_y2 = h2,
      hline_y3 = h3,
      hline_y4 = h4,
      plot_title = "",
      y_label = "WVAL",
      x_label = "",
      show_h_lines = T,
      margins = list(l = 80, r = 50, t = 10, b = 0),
      show_rangeslider = T,
      ymax = T,
      dual_target = dualplot(),
      single_plot = single_plot_signal(),
      select_target = target(),
      target_col = "pcr_gene_target",
      solid_line_target = target(),
      show_y_axis_line = T
    )
    
    max_conc =  max(plotdf[["raw concentration normalized by pmmov"]],
                    na.rm = TRUE)
    if(!is.na(h4)) {
      if(h4>max_conc){
        h5 = 2
      } else {
        h5 = (max_conc - h4)
      }
      stack5 <- h5 * upper_y_plot_limit
    }
    
    stack4 <- h4 - h3
    stack3 <- h3 - h2
    stack2 <- h2 - h1
    stack1 <- h1
    
    title_margin = list(l = 20, r = 20, t = 90, b = 35)
    
    if ((!is.na(stack1) & !is.na(stack2) & !is.na(stack3) & !is.na(stack4) & dualplot() == F) || 
        (target() %in% c("n", "rsv")) || 
        (input$flu_switch_B == F & target() %in% c("infb")) ) {
      
      bar_plot = create_bar_plot(stack1 = stack1, 
                                 stack2 = stack2, 
                                 stack3 = stack3, 
                                 stack4 = stack4,
                                 stack5 = stack5,
                                 show_legend = T)
      
      final_plot <- combined_plot <- subplot(
        bar_plot, region_plot,
        widths = c(0.04, 0.96),
        shareY = TRUE,
        margin = 0
      ) %>%
        layout(
          autosize = TRUE,
          yaxis = list(autorange = TRUE),
          xaxis = list(autorange = TRUE),
          legend = list(
            title = list(text = "   <b>Level</b>"), 
            x = 1.00,
            y = 1
          ),
          title = list(
            text = paste("Aggregated Plot for", rename_pathogen(target())),
            font = list(size = 24, weight = "bold")
          ),
          margin = title_margin,
          annotations = combine_annotation_list
        )
      
    } else {
      
      final_plot <- region_plot  %>%
        layout(
          title = list(text = paste("Aggregated Plot for", rename_pathogen(target())),
                       font = list(size = 24, family = "Arial", weight = "bold")
          ),
          margin = title_margin,
          annotations = single_annotation_list
        )
    }
    
    if (!is.null(final_plot)) {
      htmlwidgets::onRender(final_plot, js_code_plot)
    }
  })

  # ===========================================================================
  # 19. REGION PLOTS ----
  # region_plot2  : Multi-panel "Overview" plot — one subplot per RPHO region
  #   laid out in a grid. Number of rows adapts to screen width via
  #   debounced_width() + current_nrows(). Each sub-plot is a metric_plot().
  # each_region_plot: Single-region time-series plot shown in "Each Region" view.
  #   Same structure as state_plot but filtered to the selected region.
  # ===========================================================================

  region_plot_height <- reactive({
    if(target() == "n"){
      "670px"
    } else {
      "690px"
    }
  })
  
  output$each_region_plot_ui <- renderUI({
    plotlyOutput(outputId = "each_region_plot", height = region_plot_height()) %>% withSpinner(color = "#5A789A")
  })
  
  output$each_region_plot = renderPlotly({
    req(metric_plotdf())
    
    plotdf = metric_plotdf() %>%
      mutate(ww_aggregate = ww_aggregate* conversion_factor()) %>%
      rename(`sample date` = sample_date,
             `raw concentration normalized by pmmov` = ww_aggregate) %>%
      arrange(`sample date`)
    
    if(target() %in% c("infa", "infb") & dualplot() == T & single_plot_signal() == F ) {
      h1 = NA
      h2 = NA
      h3 = NA
      h4 = NA
    } else {
      h1 = na.omit(unique(plotdf$q1))[1] * conversion_factor()
      h2 = na.omit(unique(plotdf$q2))[1] * conversion_factor()
      h3 = na.omit(unique(plotdf$q3))[1] * conversion_factor()
      h4 = na.omit(unique(plotdf$q4))[1] * conversion_factor()
    }
    
    region_plot = metric_plot(
      data = plotdf,
      x_col = "sample date",
      y_col = "raw concentration normalized by pmmov",
      hover_label = state_region_line_label(),
      vline_date = max(plotdf$`sample date`) - 21,
      vline_label_position = max(plotdf$`raw concentration normalized by pmmov`, na.rm = T)*0.8,
      vline_label_font = list(size = 14),
      hline_y1 = h1,
      hline_y2 = h2,
      hline_y3 = h3,
      hline_y4 = h4,
      plot_title = "",
      y_label = "WVAL",
      x_label = "Adjust the time range using the slider above",
      x_label_show = F,
      show_h_lines = T,
      margins = list(l = 80, r = 50, t = 10, b = 40),
      show_rangeslider = T,
      single_plot =  single_plot_signal(),
      ymax = T,
      range_selector = T,
      dual_target = dualplot(),
      target_col = "pcr_gene_target",
      solid_line_target = target()
    )
    
    max_conc =  max(plotdf[["raw concentration normalized by pmmov"]],
                    na.rm = TRUE)
    if(!is.na(h4)) {
      if(h4>max_conc){
        h5 = 2
      } else {
        h5 = (max_conc - h4)
      }
      
      stack5 <- h5 * upper_y_plot_limit
    }
    
    stack4 <- h4 - h3
    stack3 <- h3 - h2
    stack2 <- h2 - h1
    stack1 <- h1
    
    if ((!is.na(stack2) & !is.na(stack3) & dualplot() == F) || single_plot_signal() ) {
      
      bar_plot = create_bar_plot(stack1 = stack1, 
                                 stack2 = stack2, 
                                 stack3 = stack3, 
                                 stack4 = stack4,
                                 stack5 = stack5,
                                 show_legend = T)
      
      final_plot <- combined_plot <- subplot(bar_plot, region_plot, widths = c(0.04, 0.96), shareY = TRUE, margin = 0) %>%
        layout(
          autosize = TRUE,
          yaxis = list(autorange = TRUE),
          xaxis = list(autorange = TRUE),
          legend = list(
            title = list(text = "    <b>Level</b>"),  # Add a legend title
            x = 1.00,  # Adjust x position of the legend to the right
            y = 1      # Keep the legend at the top
          ),
          margin = list(l = 0, r = 20, t = 30, b = 40),  # Adjust margins to reduce left space
          yaxis = list(
            automargin = TRUE  # Automatically adjust margins to fit labels
          ),
          annotations = combine_annotation_list
        )
      
    } else {
      
      final_plot <- region_plot %>%
        layout(
          margin = list(l = 0, r = 20, t = 30, b = 40),
          annotations = single_annotation_list
        )
    }
    
    if (!is.null(final_plot)) {
      htmlwidgets::onRender(final_plot, js_code_plot)
    }
  })

  # ===========================================================================
  # 20. OVERVIEW MULTI-REGION PLOT + SEWERSHED PLOT ----
  #
  # This section contains two distinct plots:
  #
  # (a) region_plot2 — "Overview" multi-panel grid showing one sub-plot per
  #     RPHO region. Number of rows adapts to screen width via debounced_width()
  #     + current_nrows(). generated_plots() builds the list of sub-plots,
  #     then region_plot2 combines them with subplot().
  #
  # (b) dynamic_sewershed_plot — the single-sewershed time series on the
  #     "Sewershed" tab. Uses long-window data from w1() filtered by rv().
  #     When input$include_data is TRUE, individual samples are overlaid as
  #     scatter markers styled by data_type:
  #       regular  -> filled circle
  #       limited  -> triangle-up  (value clipped above y-axis plot limit)
  #       below LOD -> open triangle-down
  # ===========================================================================
  ### Multi Region plot  ------------------------------------------------------
  
  observeEvent(input$region_toggle, {
    req(input$region_toggle) 
    
    toggle(id = "overview_view_panel", condition = input$region_toggle == "Overview")
    toggle(id = "each_region_view_panel", condition = input$region_toggle == "Each Region")
  })
  
    debounced_width <- reactive({
    session$clientData$output_region_plot2_width
  }) %>% debounce(500)
  
  current_nrows <- reactiveVal(3)

  observe({
    width <- debounced_width()
    req(width) 
    
    new_rows <- ifelse(target() == "infb" && width > 768, 
                       3,
                       ifelse(target() == "infb" && width <= 768, 
                              5,
                              ifelse(width <= 768 && target() != "infb", 
                                     6, 
                                     3)))
    
    if (new_rows != current_nrows()) {
      current_nrows(new_rows)
    }
  })
  
  generated_plots <- reactive({
    req(target(), plot_target())
    
    # --- Data Preparation ---
    if(single_plot_signal() == T) {
      plotdf <- state_region_plot_df %>%
        filter(pcr_gene_target %in% target()) %>%
        mutate(ww_aggregate = ww_aggregate * conversion_factor()) %>%
        rename(
          `sample date` = sample_date,
          `raw concentration normalized by pmmov` = ww_aggregate
        )
    } else {
      plotdf <- state_region_plot_df %>%
        filter(pcr_gene_target %in% plot_target()) %>%
        mutate(ww_aggregate = ww_aggregate * conversion_factor()) %>%
        rename(
          `sample date` = sample_date,
          `raw concentration normalized by pmmov` = ww_aggregate
        )
    }
    
    if(target() %in% c("infa", "infb") & dualplot() == T & single_plot_signal() == F) {
      d1 <- plotdf %>%
        group_by(region) %>%
        summarise(q1 = mean(q1, na.rm = T) * conversion_factor()) %>% 
        mutate(q1 = NA)
      d2 <- plotdf %>%
        group_by(region) %>%
        summarise(q2 = mean(q2, na.rm = T) * conversion_factor()) %>% 
        mutate(q2 = NA)
      d3 <- plotdf %>%
        group_by(region) %>%
        summarise(q3 = mean(q3, na.rm = T) * conversion_factor()) %>% 
        mutate(q3 = NA)
      d4 <- plotdf %>%
        group_by(region) %>%
        summarise(q4 = mean(q4, na.rm = T) * conversion_factor()) %>% 
        mutate(q4 = NA)
    } else {
      d1 <- plotdf %>%
        group_by(region) %>%
        summarise(q1 = mean(q1, na.rm = T) * conversion_factor())
      d2 <- plotdf %>%
        group_by(region) %>%
        summarise(q2 = mean(q2, na.rm = T) * conversion_factor())
      d3 <- plotdf %>%
        group_by(region) %>%
        summarise(q3 = mean(q3, na.rm = T) * conversion_factor())
      d4 <- plotdf %>%
        group_by(region) %>%
        summarise(q4 = mean(q4, na.rm = T) * conversion_factor())
    }
    
    plotdf$vline <- max(plotdf$`sample date`) - 21
    
    plotdf <- plotdf %>%
      group_by(region) %>%
      summarise(label_pos = max(`raw concentration normalized by pmmov`, na.rm = T) * 0.8) %>%
      right_join(plotdf)
    
    df <- as.data.frame(state_df %>% filter(pcr_gene_target == target())) %>%
      select(wwtp_name, level, trend) %>%
      filter(!is.na(wwtp_name))
    
    region_name <- sort_region(d1$region, 1)
    
    update_list <- c("", "", "", "", "", paste0(" (<span style='color:dark gray;'>Updated: ", published_date_state[[target()]], "*</span>)"))
    
    if (target() %in% c("infa", "rsv", "n")) {
      num_plot = c(1:6) 
    } else if (target() %in% c("infb") && single_plot_signal() == T) {
      num_plot = c(1:5)
    } else if (target() %in% c("infb") && single_plot_signal() == F) {
      num_plot = c(1,3,4,5,6)
    }
    
    if (target() %in% c("infa", "infb") && dualplot() == F ||  single_plot_signal() == T  ) {
      
      if (target() == "infb") {
        legend_vector <- c(T, F, F, F, F, F)
      } else {
        legend_vector <- c(T, F, F, F, F, F, F)
      }
      
      bar_level <- plotdf %>%
        group_by(region) %>%
        summarise(max_conc = max(`raw concentration normalized by pmmov`, na.rm = T) * 1.1) %>%
        left_join(d1) %>% left_join(d2) %>% left_join(d3) %>% left_join(d4) %>% 
        mutate(stack2 = q2 - q1,
               stack3 = q3 - q2,
               stack4 = q4 - q3) %>%
        mutate(
          max_value = max(max_conc, na.rm = TRUE),
          q4_at_max = q4[which.max(max_conc)],
          stack5 = ifelse(
            max_value > q4_at_max,
            max_value - q4_at_max,
            2)) %>% 
        rename(stack1 = q1) %>%
        mutate(show_legend = legend_vector,
               region = factor(region, levels = c(region_vec, "State"))) %>%
        arrange(region) %>% 
        select(stack1, stack2, stack3, stack4, stack5, show_legend)
      
      line_plotlist <- lapply(num_plot, function(v) {
        metric_plot(
          data = plotdf %>% filter(region == region_name[v]) %>% arrange(`sample date`),
          x_col = "sample date",
          y_col = "raw concentration normalized by pmmov",
          hover_label = state_region_line_label(),
          vline_date = unique(plotdf$vline),
          vline_label_position = plotdf %>% filter(region == region_name[v]) %>% pull(label_pos) %>% unique(),
          hline_y1 = filter(d1, region == region_name[v])$q1,
          hline_y2 = filter(d2, region == region_name[v])$q2,
          hline_y3 = filter(d3, region == region_name[v])$q3,
          hline_y4 = filter(d4, region == region_name[v])$q4,
          plot_title = paste0(regname(region_name[v], reverse = T), ":\nTrend: ", filter(df, wwtp_name == region_name[v])$trend, " ", " | Level: ", filter(df, wwtp_name == region_name[v])$level),
          y_label = "WVAL",
          x_label = "",
          show_h_lines = T,
          show_h_label = F,
          range_selector = F,
          single_plot = single_plot_signal(),
          dual_target = dualplot(),
          target_col = "pcr_gene_target",
          solid_line_target = target()
        )
      })
      
      bar_plotlist <- pmap(bar_level, create_bar_plot)
      ymax_vec <- bar_level$stack5
      
      plotlist <- lapply(seq_along(line_plotlist), function(i) {
        subplot(bar_plotlist[[i]], line_plotlist[[i]], widths = c(0.04, 0.96), shareY = TRUE, margin = 0) %>%
          layout(
            autosize = TRUE,
            yaxis = list(autorange = TRUE),
            xaxis = list(autorange = TRUE),
            yaxis = list(range = c(0, ymax_vec[i]))
          )
        
      })
      
      return(plotlist)
      
    } else {
      
      plotlist <- lapply(num_plot, function(v) {
        metric_plot(
          data = plotdf %>% filter(region == region_name[v]) %>% arrange(`sample date`),
          x_col = "sample date",
          y_col = "raw concentration normalized by pmmov",
          hover_label = state_region_line_label(),
          vline_date = unique(plotdf$vline),
          vline_label_position = plotdf %>% filter(region == region_name[v]) %>% pull(label_pos) %>% unique(),
          hline_y1 = filter(d1, region == region_name[v])$q1,
          hline_y2 = filter(d2, region == region_name[v])$q2,
          hline_y3 = filter(d3, region == region_name[v])$q3,
          hline_y4 = filter(d4, region == region_name[v])$q4,
          plot_title = paste0(regname(region_name[v], reverse = T), ":\nTrend: ", filter(df, wwtp_name == region_name[v])$trend),
          plot_title_y = 1,
          y_label = paste0(rename_pathogen(target(), choice = 2), "/PMMOV (x1 million)"),
          x_label = "",
          show_h_lines = T,
          range_selector = F,
          dual_target = dualplot(),
          target_col = "pcr_gene_target",
          solid_line_target = target(),
          show_legend = (v == 1)
        )
      })
      
      return(plotlist)
      
    }
  })
  
  ### Single Region plot  ------------------------------------------------------
  
  output$region_plot2 <- renderPlotly({
    req(generated_plots(), current_nrows())
    
    plotlist_to_render <- generated_plots()
    rows <- current_nrows() 
    
    final_plot <- subplot(plotlist_to_render,
                          nrows = rows,
                          shareX = FALSE, 
                          shareY = FALSE, 
                          margin = 0.04)
    
    if (( target() %in% c("infa", "infb") & dualplot() == F) || single_plot_signal() ) {
      final_plot %>% layout(
        legend = list(title = list(text = "  <b>Level</b>"), x = 1.0, y = 1),
        margin = list(t = 70)
      )
    } else {
      final_plot %>% layout(
        annotations = list(
          list(x = 0, y = 0.5, xref = "paper", yref = "paper",
               text = paste0(input$pathogen, "/PMMOV (x1 million)"),
               showarrow = FALSE, textangle = -90,
               font = list(size = 18),
               xshift = -70)
        )
      )
    }
    
    if (!is.null(final_plot)) {
      htmlwidgets::onRender(final_plot, js_code_plot)
    }
  })
  
  ## Sewershed plot ---------------------------------------------------------------
  
  sewershed_plot_height <- reactive({
    if(target() == "n"){
      "570px"
      # "528px"
    } else {
      "570px"
    }
  })
  
  output$dynamic_sewershed_plot <- renderUI({
    
    div(
      id = "sewershed-plot-tabset",
      tabsetPanel(
        id = "tabset",
        tabPanel("Sewershed Plot",
                 plotlyOutput("wwplot_metric_scale_sewershed", height = sewershed_plot_height()) %>% withSpinner(color = "#5A789A")
        )
      ),
      div(
        class = "switch-container",
        span(paste("Include Data Points for ", rename_pathogen(target()), ":"), class = "switch-label"),
        tags$input(id = "include_data", type = "checkbox", class = "switch-input")
      )
    )
    
  })
  
  output$wwplot_metric_scale_sewershed <- renderPlotly({
    
    req(plot_site_data())
    conversion_factor = 1
    
    if(target() %in% c("infa", "infb") & dualplot() == T & single_plot_signal() == F ) {
      df1 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df2 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df3 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df4 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df21day =  data.frame(term = c("long", "short"), Z = c(as.numeric(max((plot_site_data()$sample_date))-21), NA))
    } else {
      df1 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df2 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df3 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df4 <- data.frame(term = c("long", "short"), Z = c(NA, NA))
      df21day =  data.frame(term = c("long", "short"), Z = c(as.numeric(max((plot_site_data()$sample_date))-21), NA))
    }
    
    plotdf = plot_site_data() %>% rename(`sample date` = sample_date,
                                         `raw concentration normalized by pmmov` = norm_pmmov,
                                         `10 days rolling average of normalized concentration` = norm_pmmov_ten_rollapply)
    plotdf$vline = max(plotdf$`sample date`) - 21
    
    plotdf = plotdf %>% filter(term == "long") %>% group_by(term) %>%
      summarise(label_pos =
                  max(`10 days rolling average of normalized concentration`, na.rm = T)*0.9) %>%
      bind_rows(
        plotdf %>% filter(term == "short") %>% group_by(term) %>%
          summarise(label_pos =
                      max(`raw concentration normalized by pmmov`, na.rm = T)*0.9)
      ) %>% right_join(plotdf)
    
    
    long_plotdf =  plotdf %>% filter(term == "long") %>% 
      arrange(`sample date`) 
    
    if(input$include_data &
       max(long_plotdf[["10 days rolling average of normalized concentration"]], na.rm = T) > 100)
    {y_limit = 0} else {y_limit = 0}
    
    label <- list("y_label" = paste0(input$pathogen, " (Raw Concentration)"),
                  "scatter_hover_text_col" = "Raw Concentration",
                  "hover_label" = "Raw Concentration Rolling Average")
    
    long_plot =
      metric_plot(
        data = long_plotdf,
        x_col = "sample date",
        y_col = "10 days rolling average of normalized concentration",
        vline_col = "10 days rolling average of normalized concentration",
        vline_date = max(long_plotdf$`sample date`) - 21,
        vline_label_position = unique(long_plotdf$label_pos),
        vline_label_font = list(size = 14),
        hline_y1 = filter(df1, term == "long")$Z,
        hline_y2 = filter(df2, term == "long")$Z,
        hline_y3 = filter(df3, term == "long")$Z,
        hline_y4 = filter(df4, term == "long")$Z,
        plot_title = "",
        y_label =  label$y_label,
        x_label = "Adjust the time range using the slider above",
        x_label_show = F,
        y_lower_limit = y_limit,
        concentration_label = label$scatter_hover_text_col,
        show_scatter = input$include_data,
        scatter_col = "norm_pmmov_limit",
        point_size = 8,
        margins = list(l = 80, r = 50, t = 10, b = 0),
        show_rangeslider = T,
        ymax = T,
        single_plot = single_plot_signal(),
        scatter_hover_text_col = "raw concentration normalized by pmmov",
        scatter_type = "data_type",
        dual_target = dualplot(),
        target_col = "pcr_gene_target",
        solid_line_target = target(),
        hover_label = label$hover_label
      )
    
    stack5 <-  max(long_plotdf[["10 days rolling average of normalized concentration"]],
                   na.rm = TRUE) * upper_y_plot_limit
    stack4 <- filter(df4, term == "long")$Z - filter(df3, term == "long")$Z
    stack3 <- filter(df3, term == "long")$Z - filter(df2, term == "long")$Z
    stack2 <- filter(df2, term == "long")$Z - filter(df1, term == "long")$Z
    stack1 <- filter(df1, term == "long")$Z
    
    if(any(long_plotdf$wwtp_name %in% below_LOD_list[[target()]]) & input$include_data)
    {low_base_value2 = 0.5} else {low_base_value2 = 0.5}  
    
    final_plot <- long_plot %>%
      layout(
        margin = list(l = 15, r = 15, t = 30, b = 40),  # Adjust margins to reduce left space
        annotations = single_annotation_list
      )
    
    # }
    
    if (!is.null(final_plot)) {
      htmlwidgets::onRender(final_plot, js_code_plot)
    }
  })
  

  # ===========================================================================
  # 21. INFO BOXES  (Region & Sewershed) ----
  # rv2 / rv3: Store last-clicked sewershed / region name.
  # site_value()   : Filters c3 to selected sewershed for info box values.
  # value_df()     : Filters c22() to selected region for info box values.
  # value_list()   : Assembles title, level, trend, source into a named list
  #                  used by level_box and level_box_sewershed outputs.
  # subtitle_value(): Formats trend call string with CI for display.
  # level_box      : Renders the colored summary box for Region tab.
  # level_box_sewershed: Renders the colored summary box for Sewershed tab.
  # state_info_box : Renders the statewide level box on the State tab.
  # trend_box      : Renders the statewide trend box on the State tab.
  # ===========================================================================
  # Info Box ---------------------------------------------------------------
  
  ## Regional and sewershed info box ---------------------------------------------------------------
  
  
  rv2 <- reactiveVal(NULL)
  
  observeEvent(input$select_wwtp, {
    
    req(input$select_wwtp)
    rv2(input$select_wwtp)
    
  })
  
  observeEvent(input$heatmap_sewershed_shape_click, {
    rv2(gsub(" $", "", input$heatmap_sewershed_shape_click$id))
  })
  
  observeEvent(input$heatmap_sewershed_marker_click, {
    rv2(gsub(" $", "", input$heatmap_sewershed_marker_click$id))
  })
  
  site_value = reactive({
    req(target(), rv2())
    c3 %>% filter(
      !is.na(wwtp_name), pcr_gene_target == target(),
      report_include == T) %>%
      mutate(wwtp_name = factor(wwtp_name, levels = wwtp_factor())) %>%
      arrange(wwtp_name) %>%
      filter(wwtp_name == rv2())
  })
  
  rv3 <- reactiveVal(NULL)
  
  observeEvent(input$HO, {
    req(input$HO)
    rv3(gsub(" $", "", regname(input$HO)))
  })
  
  observeEvent(input$heatmap_region_shape_click, {
    req(input$heatmap_region_shape_click)
    rv3(gsub(" $", "", input$heatmap_region_shape_click$id))
    # }
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    req(input$heatmap_region_shape_click)
    rv3(gsub(" $", "", input$heatmap_region_marker_click$id))
  })
  
  value_df <- reactive({
    # Make sure all inputs are available, including target()
    req(rv3(), c22(), target())
    
    filter_region <- if (rv3() == "RANCHO" && target() == "infb") {
      "ABAHO"
    } else {
      rv3()
    }
    
    c22() %>% 
      filter(region == filter_region, is.na(wwtp_name))
  })
  
  value_list <- reactive({
    if(input$tab == "Region"){
      req(value_df())
      
      region = gsub("\\s*\\(.*\\)", "", regname(value_df()$region, reverse = T))
      
      list(
        title = tags$p(paste0(region, " (Aggregated)"),
                       style = "font-size: 75%; white-space: normal; word-wrap: break-word;"),
        source = value_df()$data_source,
        trend_value = value_df()$trend,
        trend_call = paste0(value_df()$trend, " ", value_df()$model_pc, "% ", "[",
                            value_df()$model_pc_lwr, "%, ", value_df()$model_pc_upr, "%]"),
        level_call = value_df()$level
      )
    } else if(input$tab == "Sewershed"){
      req(site_value(),
          
          site_value()$Label_Name == rv2()
      )
      
      list(
        title = tags$p(paste0(site_value()$Label_Name),
                       style = "font-size: 75%; white-space: normal; word-wrap: break-word;"),
        source = site_value()$data_source,
        trend_value = site_value()$trend2,
        trend_call = paste0(site_value()$trend2, " ", site_value()$model_pc, "% ", "[",
                            site_value()$model_pc_lwr, "%, ", site_value()$model_pc_upr, "%]"),
        level_call = site_value()$level,
        recent_sample = format(as.Date(site_value()$most_recent_sample), "%m/%d/%Y")
      )
    }
  })
  
  subtitle_value <- reactive({
    req(target(), value_list(), input$tab)
    
    if(input$tab == "Region"){
      tags$div(
        "Level: ", strong(value_list()$level_call), br(),
        "Trend: ", strong(value_list()$trend_call), br(),
        "Data Source: ", strong(value_list()$source),
        style = "font-size: 170%; white-space: normal; word-wrap: break-word;")
      
    } else if (input$tab == "Sewershed") {
      tags$div(
        "Trend: ", strong(value_list()$trend_call), br(),
        "Most Recent Sample Date: ", strong(value_list()$recent_sample), br(),
        "Data Source: ", strong(value_list()$source),
        style = "font-size: 170%; white-space: normal; word-wrap: break-word;")
    }
  })
  
  output$level_box <- renderUI({
    req(target(), value_list(), subtitle_value())
    
    target_value <- target()
    value <- value_list()
    
    box_class <- get_box_class(target_value, value)
    
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(value_list()$title),
          p(subtitle_value())
      )
    )
  })
  
  output$level_box_sewershed <- renderUI({
    
    req(target(), value_list(), subtitle_value(), input$tab == "Sewershed")
    
    target_value <- target()
    value <- value_list()
    
    box_class <- seweshed_get_box_class(value)
    
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(value_list()$title),
          p(subtitle_value())
      )
    )
  })
  
  state_value_df = reactive({
    req(c22())
    c22() %>% filter(region == "State", is.na(wwtp_name))
  })
  
  state_value_list <- reactive({
    
    req(state_value_df())
    
    list(
      title = tags$p("State: Level & Trend",
                     style = "font-size: 70%; white-space: normal; word-wrap: break-word;"),
      source = state_value_df()$data_source,
      trend_value = state_value_df()$trend,
      trend_call = paste0(state_value_df()$trend, " ", state_value_df()$model_pc, "% ", "[",
                          state_value_df()$model_pc_lwr, "%, ", state_value_df()$model_pc_upr, "%]"),
      level_call = state_value_df()$level
    )
  })
  
  state_subtitle_value <- reactive({
    req(target(), state_value_list())
    
    common_style <- "white-space: normal; word-wrap: break-word;"
    
    if (target() %in% c("n", "infa", "rsv", "infb")) {
      req(target(), state_value_list())
      tags$div(
        tags$div(
          "Level: ", strong(state_value_list()$level_call), br(),
          "Trend: ", strong(state_value_list()$trend_call),
          style = "font-size: 180%;"
        ),
        tags$div(
          "Data Source: ", strong(state_value_list()$source),
          style = "font-size: 150%;" # Or any size you prefer
        ),
        style = common_style
      )
      
    }
  })
  
  output$state_info_box <- renderUI({
    req(target(), state_value_list(), state_subtitle_value())
    
    target_value <- target()
    state_values <- state_value_list()
    
    box_class <- get_box_class(target_value, state_values)
    
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(state_value_list()$title),
          state_subtitle_value()
          
      )
    )
  })
  
  ## State info box ----------------------------------------------------
  
  output$trend_box <- renderUI({
    
    tags$div(
      id = "state_summaryBox",
      box(
        title = "Summary Tables",
        width = 14,
        solidHeader = TRUE,
        status = "primary",
        tabsetPanel(
          id = "summary_tabs",
          tabPanel("Trend Summary", htmlOutput("trend_summary_table"))
        )
      )
    )
  })
  
  output$trend_summary_table <- renderUI({
    req(c55())
  
    trend_order <- c("Increase", "Plateau", "Decrease", "Concentration too low for trend call", "Not enough data")
 
    summary_trend <- c55() %>%
      mutate(
        trend_category = case_when(
          str_detect(tolower(trend), "increase")  ~ "Increase",
          str_detect(tolower(trend), "decrease")  ~ "Decrease",
          str_detect(tolower(trend), "plateau")   ~ "Plateau",
          trend %in% c("Sporadic Detections", "All Samples Below LOD", "Concentrations too low to call trend") ~ "Concentration too low for trend call",
          str_detect(tolower(trend), "not enough data")   ~ "Not enough data",
          TRUE ~ "Other"
          
        )
      ) %>%
      mutate(trend_category = factor(trend_category, levels = trend_order)) %>%
      count(trend_category, name = "count") %>%
      mutate(
        total = sum(count),
        fraction = paste0(count, "/", total),
        percentage = scales::percent(count / total, accuracy = 0.1)
      ) %>%
      select(
        `Trend Category` = trend_category,
        `Site Proportion` = fraction,
        `Site Percentage` = percentage
      )
    
    html_table_trend <- summary_trend %>%
      kable(
        "html",
        caption = "<span style='font-size: 17px; color:black;'>
                 Summary of Trend Data: Number and percentage of wastewater sites in each trend category.
               </span>",
        
        align = c('l', 'c', 'c')
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = TRUE,
        font_size = 20
      )
    
    accessible_html_table <- gsub("<th", "<th tabindex='0'", html_table_trend)
    accessible_html_table <- gsub("<td", "<td tabindex='0'", accessible_html_table)
    
    HTML(accessible_html_table)
    
  })

  output$level_summary_table <- renderUI({
    req(target(), c55())
   
    level_order <- c("High", "Medium", "Low", "Not enough data", "Not Available")
    
    summary_level <- c55() %>%
      mutate(
        level_category = case_when(
          str_detect(tolower(level), "high") ~ "High",
          str_detect(tolower(level), "medium") ~ "Medium",
          str_detect(tolower(level), "low") ~ "Low",
          str_detect(tolower(level), "not enough data") ~ "Not enough data",
          is.na(level) ~ "Not Available",
          TRUE ~ "Other"
        )
      ) %>%
      mutate(level_category = factor(level_category, levels = level_order)) %>%
      count(level_category, name = "count") %>%
      mutate(
        total = sum(count),
        fraction = paste0(count, "/", total),
        percentage = scales::percent(count / total, accuracy = 1)
      ) %>%
      select(
        Level = level_category,
        `Site Proportion` = fraction,
        `Site Percentage` = percentage
      )
  
    html_table_level <- summary_level %>%
      kable(
        "html",

        caption = "<span style='font-size:17px; color: black;'>
                 Summary of Level Data: proportion and percentage of wastewater sites in each level category.
               </span>",
        align = c('l', 'c', 'c')
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = TRUE,
        font_size = 20
      )
    
    accessible_html_table <- gsub("<th", "<th tabindex='0'", html_table_level)
    accessible_html_table <- gsub("<td", "<td tabindex='0'", accessible_html_table)
    
    HTML(accessible_html_table)
    
  })
  
  # ===========================================================================
  # 22. SUMMARY TABLES ----
  # sewershed_summary_table : DT table of all sewersheds filtered to target().
  #   Shows trend, level, data source, and % change with CI.
  # region_summary_table    : DT table of all regions + statewide.
  # covid_table             : DT table used in the collapsible panel on the
  #   Statewide tab (same data as sewershed_summary_table).
  # All tables use KeyTable extension and a headerCallback for ADA compliance.
  # ===========================================================================

  report_table = reactive({
    req(target())
    
    target_value <- target()
    
    data = summary_table %>%
      filter(`PCR Gene Target` == target_value) %>%
      select(-`PCR Gene Target`)%>%
      as.data.frame() %>%
      recode_rpho_region(.new_col = "Region", .ref_col = "Region", reverse = T) %>% 
      select("Region", "County" ,"County (City/Utility)", "Level",
             "21 day Trend", "Percent Change [95% CI]", "Data displayed on map", "Most Recent Sample Date",
             "Data Source") %>% 
      mutate(`21 day Trend` = as.character(`21 day Trend`)) %>% 
      mutate(across(.cols = -`Percent Change [95% CI]`, .fns = as.factor)) %>%
      mutate(Region = factor(Region, levels = c(region_choice, "Statewide"))) %>% 
      arrange(Region, County, `County (City/Utility)`)
    
    return(data)
  })
  
  region_report_table = reactive({
    req(target())
    
    target_value <- target()
    
    data <- region_summary_table %>%
      filter(`PCR Gene Target` == target_value) %>%
      select(-`PCR Gene Target`) %>%
      select("Region", "Level", "21 day Trend", "Percent Change [95% CI]", "Data Source") %>%
      mutate(`21 day Trend` = as.character(`21 day Trend`)) %>% 
      mutate(across(.cols = -`Percent Change [95% CI]`, .fns = as.factor)) %>%
      mutate(Region = factor(Region, levels = c(region_choice, "Statewide"))) %>% 
      arrange(Region)
    
    return(data)
  })
  
  output$region_summary_table <- renderDT({
    req(target())
    
    if (target() %in% "n") {
      column_name = "Level"
    } else if (target() %in% c("infa", "rsv", "infb")) {
      column_name = "Level"
    }
    
    DT::datatable(
      region_report_table() %>% as.data.frame(),
      extensions = 'KeyTable',
      filter = "top",
      caption = htmltools::tags$caption(
        "Enter your search criteria in the boxes below to filter the table",
        style = "color:#AD302C"
      ),
      callback = JS(
        "$('div.has-feedback input[type=\"search\"]').attr('placeholder', 'Follow instructions to select...');",
        "document.querySelectorAll(\"div.form-group input[type='search']\").forEach(function(el) {",
        "  el.addEventListener('keypress', function(event) {",
        "    if (event.keyCode === 13 || event.keyCode === 32) {",
        "      el.click();",
        "    }",
        "  });",
        "});"
      ),
      options = list(pageLength = 50,
                     scrollX = TRUE,
                     keys = TRUE, 
                     headerCallback = htmlwidgets::JS(
                       "function(thead, data, start, end, display) {",
                       "  $(thead).find('th').attr('tabindex', 0);",
                       "}"
                     )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        column_name,
        backgroundColor = styleEqual(
          names(state_threshold_colors_transparent[[target()]]),
          state_threshold_colors_transparent[[target()]]
        ),
        color = "black"  
      )
  })
  
  output$sewershed_summary_table <- renderDT({
    req(target())
    
    if (target() %in% "n") {
      column_name = "Level"
    } else if (target() %in% c("infa", "rsv", "infb")) {
      column_name = "Level"
    }
   
    DT::datatable(
      report_table() %>% as.data.frame() %>% select(-Level), 
      extensions = 'KeyTable',
      filter = "top",
      caption = htmltools::tags$caption(
        "Enter your search criteria in the boxes below to filter the table",
        style = "color:#AD302C"
      ),
      callback = JS(
        "$('div.has-feedback input[type=\"search\"]').attr('placeholder', 'Follow instructions to select...');",
        "document.querySelectorAll(\"div.form-group input[type='search']\").forEach(function(el) {",
        "  el.addEventListener('keypress', function(event) {",
        "    if (event.keyCode === 13 || event.keyCode === 32) {",
        "      el.click();",
        "    }",
        "  });",
        "});"
      ),
      options = list(pageLength = 50,
                     scrollX = TRUE,
                     keys = TRUE, 
                     headerCallback = htmlwidgets::JS(
                       "function(thead, data, start, end, display) {",
                       "  $(thead).find('th').attr('tabindex', 0);",
                       "}"
                     )
      ),
      rownames = FALSE
    ) 
  })
  
  

  # ===========================================================================
  # 23. DATA DOWNLOAD ----
  # download_table1: DT table of raw wastewater data with column selector
  #   (input$show_vars_wastewater). Factor columns get dropdown filters.
  # download_table2: DT table of metrics summary data (input$show_vars_metrics).
  # confirmDownload: Shows a modal dialog to confirm before downloading.
  # downloadData1 / downloadData2: downloadHandlers that export the currently
  #   filtered rows (respecting DT column filters) as CSV files.
  # reactive_vals_download1/2: reactiveValues that cache filtered data so the
  #   download handler can access exactly what the user sees in the table.
  # ===========================================================================
  # Data download -----------------------------------------------------------
  
  reactive_vals_download1 <- reactiveValues(filtered_data = NULL)
  
  output$download_table1 <- DT::renderDT({
        
    download_df1 <- download_df1 %>%
      recode_rpho_region(.new_col = "Region", .ref_col = "Region", reverse = T) %>% 
      mutate(across(c("Region", "County", "County (City/Utility)", "Abbreviated Name",
                      "PCR Gene Target", "PCR Target",
                      "Sample Type", "Data Source"), as.factor))
    
    reactive_vals_download1$filtered_data <- download_df1 %>%
      select(input$show_vars_wastewater)
    
    DT::datatable(
      reactive_vals_download1$filtered_data,
      
      extensions = 'KeyTable',
      
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 2.1rem; color: black; line-height: 1.5;',
        "Use in the boxes below each column header to filter the data. Specific instructions are below:",
        htmltools::tags$ul(
          style = "margin-top: 5px; padding-left: 25px; font-size: 2.1rem;", 
          htmltools::tags$li(
            "For category columns (e.g., Region): Click the box to select an option from the list. For keyboard use: ",
            htmltools::tags$strong("Tab"), " to the filter, press ",
            htmltools::tags$strong("Enter"), " to open the list, use the ",
            htmltools::tags$strong("arrow keys"), " to choose, and finally press ",
            htmltools::tags$strong("Enter"), " to confirm."
          ),
          htmltools::tags$li(
            "For numeric columns (e.g., Raw Concentration): Use the slider or type a range using '...' to filter between two values (e.g., ",
            htmltools::tags$strong("100...500"), ")."
          ),
          htmltools::tags$li(
            "For the 'Sample Date' column: Use the slider or type a date range using '...' with the format ",
            htmltools::tags$strong("YYYY-MM-DD...YYYY-MM-DD"), " (e.g., 2024-01-01...2024-01-31)."
          )
        )
      ),
      
      filter = "top",
      rownames = FALSE,
      
      callback = JS(
        "$('div.has-feedback input[type=\"search\"]').attr('placeholder', 'Follow instructions to select...');",
        "document.querySelectorAll(\"div.form-group input[type='search']\").forEach(function(el) {",
        "  el.addEventListener('keypress', function(event) {",
        "    if (event.keyCode === 13 || event.keyCode === 32) {",
        "      el.click();",
        "    }",
        "  });",
        "});"
      ),
      
      options = list(
        keys = TRUE,
        lengthMenu = c(20, 50, 100),
        pageLength = 10,
        scrollX = TRUE
      ),
      
      class = 'download-datatable-1'
      
    ) %>%
      formatSignif(columns = intersect(download1_num_col, input$show_vars_wastewater), digits = 3)
  })
  
  
  observeEvent(input$confirmDownload, {
    
    showModal(
      modalDialog(
        title = "Confirm Download",
        tags$p("Are you sure you want to download the wastewater data? Depending on your selection, the file size may be quite large."),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("downloadData1", "Yes, Download", class = "btn-success")
        ),
        easyClose = TRUE
      )
    )
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("wastewater-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_rows <- input$download_table1_rows_all
      
      filtered_data <- reactive_vals_download1$filtered_data[filtered_rows, ]
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  
  reactive_vals_download2 <- reactiveValues(filtered_data = NULL)
  
  output$download_table2 <- DT::renderDT({
    
    reactive_vals_download2$filtered_data <- download_df2 %>%
      recode_rpho_region(.new_col = "Region", .ref_col = "Region", reverse = T) %>% 
      select(input$show_vars_metrics) %>% 
      mutate(across(c("Region", "County", "County (City/Utility)", 
                      "PCR Gene Target", "PCR Target", "Level", "Data displayed on map",
                      "21 day Trend", "Most Recent Sample Date","Data Source"), as.factor)) %>% select(-Level)
    
    # Render the datatable
    DT::datatable(
      reactive_vals_download2$filtered_data,
      
      extensions = 'KeyTable',
     
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 2.1rem; color: black; line-height: 1.5;',
        "Use in the boxes below each column header to filter the data. Specific instructions are below:",
        htmltools::tags$ul(
          style = "margin-top: 5px; padding-left: 25px; font-size: 2.1rem;", # Indent the list
          htmltools::tags$li(
            "For category columns (e.g., Region): Click the box to select an option from the list. For keyboard use: ",
            htmltools::tags$strong("Tab"), " to the filter, press ",
            htmltools::tags$strong("Enter"), " to open the list, use the ",
            htmltools::tags$strong("arrow keys"), " to choose, and finally press ",
            htmltools::tags$strong("Enter"), " to confirm."
          )
        )
      ),

      filter = "top",  
      rownames = FALSE,
      
      callback = JS(
        "$('div.has-feedback input[type=\"search\"]').attr('placeholder', 'Follow instructions to select...');",
        "document.querySelectorAll(\"div.form-group input[type='search']\").forEach(function(el) {",
        "  el.addEventListener('keypress', function(event) {",
        "    if (event.keyCode === 13 || event.keyCode === 32) {",
        "      el.click();",
        "    }",
        "  });",
        "});"
      ),
      
      options = list(
        lengthMenu = c(20, 50, 100),
        pageLength = 10,
        scrollX = TRUE
      ),
      class = 'download-datatable-2' 
    )
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("wastewater-metrics-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_rows <- input$download_table2_rows_all
      filtered_data <- reactive_vals_download2$filtered_data[filtered_rows, ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

