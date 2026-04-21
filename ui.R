# =============================================================================
# FILE: ui.R
# PROJECT: Cal-SuWers Public Dashboard v2
# DESCRIPTION:
#   Defines the complete Shiny user interface using shinydashboard.
#   All UI components are wrapped in a dashboardPage() object assigned to `ui`.
#   The global CSS (responsive breakpoints, custom box classes, font sizes)
#   is embedded inside dashboardSidebar() via tags$style().
#
# STRUCTURE:
#   ui
#   └── tags$html (lang="en")          ← Accessibility: sets page language
#       └── dashboardPage
#           ├── dashboardHeader         ← Logo, title, Announcement button
#           ├── dashboardSidebar        ← Navigation menu, pathogen selector,
#           │                              region/sewershed dropdowns, toggles
#           └── dashboardBody
#               └── tabItems
#                   ├── tabItem: "home"            ← Homepage with clickable info boxes
#                   ├── tabItem: "overview"        ← Respiratory Virus Data
#                   │   └── tabsetPanel
#                   │       ├── "Statewide"        ← State-level plot + summary
#                   │       ├── "Region"           ← Regional map + plot
#                   │       └── "Sewershed"        ← Sewershed map + plot
#                   ├── tabItem: "technical_notes" ← About / Methods page
#                   ├── tabItem: "instructions"    ← Dashboard how-to guide
#                   └── tabItem: "download"        ← Data download tables
#
# ADA NOTES:
#   - tags$html(lang="en") sets accessible language attribute
#   - role="img" + aria-label on Leaflet and Plotly containers
#   - Keyboard-accessible buttons via onkeydown handlers
#   - tabindex="0" on clickable non-button elements
# =============================================================================

# UI ----------------------------------------------------------------------

ui <- tags$html(lang = "en",
                tags$head(
                  # Load the main Google Analytics library
                  tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-C755LFBN2T"),
                  tags$script(src = "analytics.js"),
                  tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                  tags$title("Cal-SuWers Dashboard")  # Change this to your desired title
                ),
                dashboardPage(
                  dashboardHeader(
                    title = tags$span(
                      tags$img(
                        src = "favicon.png", # Path to your image file
                        height = "30px"#, # Adjust the height of the image
                      ),
                      "CDPH California Surveillance of Wastewaters (Cal-SuWers) Network 2",
                      class = "dashboard-title"
                    ),
                    tags$li(
                      class = "dropdown",
                      div(id = "my_special_button",
                          style = "margin-right: 10px",
                          actionButton("announce_button", "Announcement", class = "btn-primary")
                      )
                    )
                  ),
                  
  # ---------------------------------------------------------------------------
  # SIDEBAR
  # Contains: CSS overrides, navigation menu, pathogen selector (input$pathogen),
  # flu A/B & H5 toggles, region/sewershed dropdowns, data points toggle,
  # and sidebar navigation buttons (State Summary, Regional Summary, etc.)
  # ---------------------------------------------------------------------------
                  ## Sidebar -----------------------------------------------------------------
                  
                  dashboardSidebar(
                    useShinyjs(),
                    tags$style(HTML("
                    
                    @media (min-width: 1031px) {
                    
                        .main-header .logo {
                          width: 650px;
                        }
                        
                        .main-header .navbar {
                          margin-left: 650px;
                        }
                        
                        .text16 {
                          font-size: 12px !important; /* override to 12px */
                        }
                        .text18 {
                            font-size: 15px !important; /* override to 15px */
                        }
                        
                        #regional_summary {
                          color: black;
                          background-color: #99b6cf;
                          border-color: #a7cdf2;
                          width: 90%;
                          font-weight: bold;
                          font-size: 14px;
                          border-radius: 10px;
                          padding: 10px;
                          margin-top: 10px; /* replicate any margin from your wrapper */
                        }
                         
                        #state_summary {
                           color: black; 
                           background-color: #99b6cf; 
                           border-color: #2e6da4;
                           width: 90%; 
                           font-weight: bold; 
                           font-size: 14px; 
                           border-radius: 10px; 
                           padding: 10px;
                        }
                        
                        #level_heatmap {
                          color: black; 
                          background-color: #99b6cf; 
                          border-color: #a7cdf2;
                          width: 90%; 
                          font-weight: bold; 
                          font-size: 18px; 
                          border-radius: 10px; 
                          padding: 10px;
                        }
                        
                        .state-info-container {
                            padding-left: 45px;
                        }
                        
                        #modal_region .modal-dialog{
                        width: 90vw;
                        height: 120vh;
                        }
                        
                        .region-plot2-container .plotly{
                          height: 1000px !important;
                        }
                        
                        .dashboard-title {
                          font-size: 18px !important;
                          font-weight: bold;
                        }
                        
                        .home-box-text {
                             font-size: 21px !important;
                            word-wrap: break-word;
                            overflow-wrap: break-word;
                            white-space: normal;
                            max-width: 100%;
                        }
                      }
                      
                    /* Reset width at exactly px */
                      @media (max-width: 1030px) {
                      
                        .dashboard-title {
                          font-size: 16px !important;
                        }
                      
                         #regional_summary {
                          color: black;
                          background-color: #99b6cf;
                          border-color: #a7cdf2;
                          width: 90%;
                          font-weight: bold;
                          font-size: 14px; 
                          border-radius: 10px;
                          padding: 10px;
                          margin-top: 10px; /* replicate any margin from your wrapper */
                         }
                         #state_summary {
                           color: black; 
                           background-color: #99b6cf; 
                           border-color: #2e6da4;
                           width: 90%; 
                           font-weight: bold; 
                           font-size: 14px; 
                           border-radius: 10px; 
                           padding: 10px;
                         }
                        #level_heatmap {
                          color: black; 
                          background-color: #99b6cf; 
                          border-color: #a7cdf2;
                          width: 90%; 
                          font-weight: bold; 
                          font-size: 14px; 
                          border-radius: 10px; 
                          padding: 10px;
                        }
                        
                        .text16 {
                          font-size: 12px !important; /* override to 12px */
                        }
                        .text18 {
                            font-size: 15px !important; /* override to 15px */
                        }
                         .state-info-container {
                            padding-left: 45px;
                            padding-right: 45px;
                         }
                        
                        .level-box-container {
                          padding-top: 20px;
                        }
                        
                        #modal_region .modal-dialog{
                          width: 90vw;
                          height: 190vh;
                        }
                        
                       .region-plot2-container .plotly{
                          height: 1900px !important;
                       }
                       
                         
                       .home-box-text {
                            font-size: 21px !important;
                            word-wrap: break-word;
                            overflow-wrap: break-word;
                            white-space: normal;
                            max-width: 100%;

                        }
                      
                      }
                    
                    .main-header .sidebar-toggle:before {
                      content: '\\f362'; 
                    }
                    
                    .skin-blue .main-header .logo {
                      background-color: #5A789A;
                    }
                    .skin-blue .main-header .logo:hover {
                      background-color: #5A789A;
                    }
                    .skin-blue .main-header .navbar {
                      background-color: #5A789A;
                    }
                    .sidebar-menu > li > a {
                      font-size: 18px;
                      padding: 12px 5px;
                    }
                    .nav-tabs {
                      background-color: white;
                    }
                    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
                        background-color: #2C3E50 !important;
                          color: white !important;
                      }
                    
                    .nav-tabs > li > a {
                      color: black;
                    }
                                     
                    #my_special_button {
                        position: relative;
                        width: 100%;
                        margin-top: 2px;
                        text-align: center;
                    }
                   
                    #my_special_button .btn {
                      background-color: #D8BFD8;
                      color: black;
                      border: 5px solid gray;
                      border-radius: 10px;
                      font-weight: bold; /* Make text bold */
                      font-size: 16px;
                    }
                    #my_special_button .btn:hover {
                      background-color: #89CFF0;
                      color: black;
                    }
                    @media (min-width: 768px) {
                    .sidebar-collapse .main-sidebar {
                      width: 0px;
                    }
                    .sidebar-collapse .content-wrapper,
                    .sidebar-collapse .main-footer {
                      margin-left: 0px;
                    }
                    }
                    #collapse_state .panel-heading {
                      background-color: #D8BFD8 !important;
                      color: black !important;
                      font-size: 24px !important;
                      font-weight: bold !important;
                    }
                    #collapseTable .panel-heading, #region_#collapseTable .panel-heading{
                      background-color: #e3dcdc !important;
                      color: black !important;
                      font-size: 24px !important;
                      font-weight: bold !important;
                      cursor: pointer;
                    }
                    #collapseTable .panel-body, #region_collapseTable .panel-body {
                      font-size: 18px;
                    }
                    #state_summary:hover {
                    background-color: #D8BFD8 !important;  /* Change button background color on hover */
                    color: black !important;  /* Change text color on hover */
                    border-color: #ff4500!important;  /* Change border color on hover */
                    }
                  
                    #regional_summary:hover {
                      background-color: #66CDAA !important;  /* Change button background color on hover */
                      color: black !important;  /* Change text color on hover */
                      border-color: #228b22 !important;  /* Change border color on hover */
                    }
                    
                    #level_heatmap:hover {
                    background-color: #f7f7a8 !important;  /* Change button background color on hover */
                      color: black !important;  /* Change text color on hover */
                      border-color: #f5c97d !important;  /* Change border color on hover */
                      }
                    
                    /* Tooltip container with specific ID */
                    #downloadButtonWithTooltip1, #downloadButtonWithTooltip2, #sewershed_tooltip_icon {
                      position: relative;
                      display: inline-block;
                      width: 100%;  /* Ensure the container spans the full width */
                    }
                    
                    /* Tooltip text for the specific download button */
                    #downloadButtonWithTooltip1 .tooltip-text, #downloadButtonWithTooltip2 .tooltip-text{
                      visibility: hidden;
                      max-width: 400px;  /* Make the tooltip box wider */
                      background-color: #FFFACD;  /* Light yellow background */
                      color: #333333;  /* Dark text */
                      font-size: 16px;  /* Smaller font size */
                      text-align: center;
                      border-radius: 5px;
                      padding: 10px;  /* Slightly reduce padding */
                      position: absolute;
                      z-index: 1;
                      bottom: 150%;  /* Position the tooltip above the button */
                      left: 50%;  /* Center the tooltip relative to the button */
                      transform: translateX(-50%);  /* Centering fix */
                      opacity: 0;
                      transition: opacity 0.3s;
                    
                      /* Added thick dark border */
                      border: 3px solid #333333;
                      white-space: normal;  /* Ensure text wraps inside the box */
                    }
                  
                  /* Show the tooltip text when hovering over the specific button */
                  #downloadButtonWithTooltip1:hover .tooltip-text, #downloadButtonWithTooltip2:hover .tooltip-text, #sewershed_tooltip_icon .tooltip-text{
                    visibility: visible;
                    opacity: 1;
                  }
                  
                 h3 {
                      font-size: 24px;
                      font-weight: bold;
                    }
                    p, ul {
                      font-size: 18px;
                    }
                    .dataTables_wrapper .dataTables_paginate .paginate_button {
                      padding: 0;
                      margin-left: 0;
                      display: inline;
                      border: 0;
                      background: transparent;
                    }
                    /* Add padding to the dashboard body */
                    .content-wrapper {
                      padding-left: 20px;
                      padding-right: 20px;
                    }
                    .dynamic-title {
                    padding-left: 20px;
                    }
                    .selectize-dropdown, .selectize-control.single .selectize-input {
                      background-color: #e3dcdc !important;
                      color: black !important;
                      border: 4px solid #5A789A !important;
                    }
                    /* Style for selectInput labels */
                    label.control-label {
                      color: #e3dcdc !important;
                      font-weight: bold;
                      padding: 5px;
                      border-radius: 5px;
                    }
                    
                    #modal_state .modal-dialog{
                      width: 90vw;
                      height: 90vh;
                    }
                    #modal_state .modal-header, #modal_region .modal-header, #modal_level_heatmap .modal-header {
                      background-color: #337ab7;
                    }
                    #modal_state .modal-footer, #modal_region .modal-footer, #modal_level_heatmap .modal-footer {
                      background-color: #337ab7;
                      color: white;
                      text-align: right;
                      padding: 15px;
                    }
                    
                    #modal_level_heatmap .modal-dialog {
                      width: 90vw !important;
                    }
                     
                    #modal_level_heatmap label {
                                 color: black !important;
                                 font-weight: bold
                    }
                    
                    .level-controls {
                     display: flex;
                     align-items: center;
                     flex-wrap: wrap; /* Allows elements to move to the next line */
                     gap: 15px; /* Spacing between elements */
                     margin-top: 35px; /* Adjust this value to lower the elements */
                     margin-right:35px;
                   }
                   .level-box {
                     display: inline-block;
                     width: 25px;
                     height: 25px;
                     border-radius: 4px;
                   }
                  
                   .pretty.p-switch {
                     transform: scale(1.6);  /* Adjust overall size */
                     margin-top: 14px;        /* Adjust spacing */
                     margin-left: 40px;
                   }
                   
                   #level_site_dropdown {
                     font-size: 18px !important;  /* Increases the text size of the selected item */
                   }
                   .selectize-dropdown-content {
                     font-size: 18px !important;  /* Increases the text size of the dropdown choices */
                   }
                   
                     /* Increase size of close button */
                    #closeBtn, #closeBtn2, #closeBtn3 {
                      font-size: 18px; /* Close button font size */
                      background-color: #f44336; /* Close button background color */
                      color: white; /* Close button text color */
                      border: none;
                      padding: 10px 20px;
                      border-radius: 10px;
                    }
                    #closeBtn:hover, #closeBtn2:hover, #closeBtn3:hover {
                      background-color: #d32f2f; /* Darker hover effect for close button */
                    }
              
                    #show_vars .checkbox label {
                      font-size: 16px;  /* Increases text size only for this checkboxGroupInput */
                      color: #000000;   /* Ensures checkbox labels are black */
                    }
                    .checkbox label {
                      white-space: normal;
                      word-wrap: break-word;
                    }
                    .checkbox-group-input {
                      display: flex;
                      flex-wrap: wrap;
                    }
                    .checkbox input {
                      margin-right: 10px;
                    }
                    
                     /* Add white background and padding to the conditionalPanel */
                    .download-panel {
                      background-color: white;
                      padding: 15px;
                      border: 1px solid #ddd; /* Optional: Add a light border */
                      border-radius: 5px; /* Optional: Add some rounded corners */
                      box-shadow: 0px 2px 10px rgba(0, 0, 0, 0.1); /* Optional: Add a subtle shadow */
                    }
              
                    /* Style the download buttons */
                    #downloadData1, #downloadData2 {
                      font-size: 18px;           /* Increase text size */
                      padding: 10px 20px;        /* Increase padding (makes the button larger) */
                      background-color: #337ab7; /* Add background color */
                      color: white;              /* Change text color to white */
                      border: none;              /* Remove border */
                      border-radius: 5px;        /* Rounded corners */
                    }
              
                    /* Hover effect for the download buttons */
                    #downloadData1:hover, #downloadData2:hover {
                      background-color: #285e8e; /* Darker blue on hover */
                    }
                    
                    /* Shared styling for both download-datatable-1 and download-datatable-2 */
                      .download-datatable-1 td, .download-datatable-1 th,
                      .download-datatable-2 td, .download-datatable-2 th {
                        font-size: 18px;  /* Increase the text size for cells and headers */
                      }
              
                    /* Style for the Data Download action buttons */
                    #message_1, #message_2 {
                      font-size: 18px;           /* Increase text size */
                      padding: 10px 20px;        /* Increase padding */
                      background-color: #026769; /* Green background color */
                      color: white;              /* White text */
                      border: none;              /* Remove border */
                      border-radius: 5px;        /* Rounded corners */
                    }
              
                    /* Hover effect for Data Download action buttons */
                    #message_1:hover, #message_2:hover {
                      background-color: #0a9a9c; /* Darker green on hover */
                    }
                    
                     /* Scoped styles within #dash_update_panel_container */
                    #dash_update_panel_container {
                      width: 100%;
                      max-width: 600px;
                      margin: 0 auto;
                      border: 2px solid #022852; /* Add border color, width, and style */
                      border-radius: 5px; /* Optional: Add rounded corners */
                      box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); /* Optional: Add shadow for better visualization */
                    }
              
                    /* Style for the top banner (panel heading) inside the scoped container */
                    #dash_update_panel_container .panel-header {
                      background-color: #022852;
                      color: white;
                      padding: 13px;
                      text-align: left;
                      font-weight: bold;
                      cursor: pointer;
                      font-size: 22px; /* Increase the font size for larger text */
                    }
              
                    /* Remove default button styles within the scoped container */
                    #dash_update_panel_container #toggle_btn {
                      display: none; /* Hide the default actionButton */
                    }
              
                    /* Style for the collapsible content within the scoped container */
                    #dash_update_panel_container #scrollable_content {
                      max-height: 650px;
                      overflow-y: auto;
                      border: 1px solid #ccc;
                      padding: 10px;
                      box-sizing: border-box;
                    }
              
                    /* Styling for each row entry with a faint line within the scoped container */
                    #dash_update_panel_container .entry {
                      margin-bottom: 15px;
                      border-bottom: 1px solid #ddd;
                      padding-bottom: 10px;
                      font-size: 17px; /* Change the text size for entries */
                    }
              
                    /* Remove the bottom border for the last entry within the scoped container */
                    #dash_update_panel_container .entry:last-child {
                      border-bottom: none;
                    }
                    
                    .instructions-section p, .technical-notes-section p {
                      font-size: 21px;  /* Adjust font size for p elements in these sections */
                    }
                    .instructions-section li, .technical-notes-section li {
                      font-size: 21px;  /* Adjust font size for li elements in these sections */
                    }
                    .instructions-section h4, .technical-notes-section h4 {
                      font-size: 21px;  /* Adjust font size for h4 elements in these sections */
                    }
                    
                    #show_vars_metrics .checkbox label, #show_vars_wastewater .checkbox label{
                      font-size: 18px;
                    }
                    
                    #sewershed-plot-tabset .nav-tabs {
                      position: relative;
                      padding-right: 10px; /* Ensure there is enough space on the right for the switches */
                    }
                    #sewershed-plot-tabset .switch-container {
                      position: absolute;
                      right: 0px;
                      top: 10px; /* Adjust to align vertically */
                      z-index: 10;
                      display: flex;
                      align-items: center;
                    }
                    #sewershed-plot-tabset .switch-label {
                      margin-right: 5px;
                      display: inline-block;
                      position: relative;
                      top: 0px; /* Adjust text position slightly lower */
                      font-size: 19px; /* Increase the font size for the label */
                    }
                    #sewershed-plot-tabset .switch-input {
                      position: relative;
                      width: 40px;
                      height: 20px;
                      -webkit-appearance: none;
                      background-color: #ccc;
                      outline: none;
                      cursor: pointer;
                      border-radius: 20px;
                      transition: .4s;
                      top: -2px; /* Adjust switch position slightly higher */
                    }
                    #sewershed-plot-tabset .switch-input:first-of-type {
                      margin-right: 25px; /* Reduced spacing specifically between the first switch and the second label */
                    }
                    #sewershed-plot-tabset .switch-input:checked {
                      background-color: #2C3E50; /* Updated ON color */
                    }
                    #sewershed-plot-tabset .switch-input:before{
                      content: '';
                      position: absolute;
                      width: 18px;
                      height: 18px;
                      border-radius: 50%;
                      background-color: white;
                      top: 1px;
                      left: 1px;
                      transition: .4s;
                    }
                    #sewershed-plot-tabset .switch-input:checked:before {
                      transform: translateX(20px);
                    }
                    
                    .alert .confirm {background-color: #2874A6 !important;}
                    
                    .bg-custom-trendinfo { background-color: #e3dcdc !important; color: #000000 !important; border: 5px solid #3c3d45; border-radius: 8px; }
                    .bg-custom-covid_very_low {
                      background-color: #BAE8DE !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-covid_low {
                      background-color: #B8E5AC !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-covid_moderate {
                      background-color: #FEA82F !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-covid_high {
                      background-color: #F45B53 !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-covid_very_high {
                      background-color: #C15C9C !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-covid_no_data {
                      background-color: #969696 !important;
                      color: #000000 !important;
                      border: 5px solid #3c3d45;
                      border-radius: 8px;
                    }
                    
                    .bg-custom-sewershed_very_low {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }
                      
                      .bg-custom-sewershed_low {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }
                      
                      .bg-custom-sewershed_moderate {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }
                      
                      .bg-custom-sewershed_high {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }
                      
                      .bg-custom-sewershed_very_high {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }
                      
                      .bg-custom-sewershed_no_data {
                        background-color: #ffffff !important;
                        color: #000000 !important;
                        border: 5px solid #3c3d45;
                        border-radius: 8px;
                      }


                    .bg-custom-flu_decrease { background-color: #67a867 !important; color: #000000 !important; border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_strong_increase { background-color: #FF7F7F !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_increase { background-color: #FFA07A !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_very_strong_increase { background-color: #CD5C5C !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_plateau { background-color: #d4ac77 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_no_data { background-color: #969696 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_sporadic_detections { background-color: #D3C7A0 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    .bg-custom-flu_all_below_lod { background-color: #D3D3D3 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                    
                    #state_summaryBox .box-title { font-size: 24px; }
                    #state_summaryBox .box-header { 
                                              background-color: #5A789A;
                                              border-color: #5A789A;
                    }
                    #state_summaryBox .nav-tabs > li > a { font-size: 21px; } 
                    
                    .bootstrap-switch .bootstrap-switch-handle-on {
                        background-color: #5A789A !important;
                    }
                      
                     /* Style for the SELECTED (primary) button - a darker shade */
                      #region_toggle_container .btn-primary.active {
                        background-color: #2A4058; /* Darker color */
                        border-color: #2A4058;
                      }
                
                      /* Style for the UNSELECTED (non-primary) button(s) */
                      #region_toggle_container .btn-primary:not(.active) {
                        background-color: #54789A; /* Your specified color */
                        border-color: #54789A;
                        color: #FFFFFF;            /* Text color changed to white for readability */
                      }
                     
                     
                      /* General rule for all links, basic buttons, and checkboxes */
                        body a:focus-visible,
                      body .btn:focus-visible,
                      body input[type=checkbox]:focus-visible {
                        outline: 3px solid #ff3c00 !important; /* Your orange color */
                        outline-offset: 2px;
                        border-radius: 4px;
                      }
                  
                      /* --- SPECIFIC OVERRIDES FOR COMPLEX WIDGETS --- */
                  
                      /* 1. For radioGroupButtons */
                      body .radio-group-buttons .btn:focus-visible {
                        outline: 3px solid #ff3c00 !important;
                        outline-offset: -2px; /* Use a negative offset for buttons */
                      }
                  
                      /* 2. For Leaflet Map Icons (Markers) */
                      body .leaflet-marker-icon:focus-visible {
                        outline: 3px solid #ff3c00 !important;
                        outline-offset: 2px;
                        border-radius: 50%; /* Make the outline circular to match the icon */
                      }
                  
                      /* 3. For DT DataTable Headers and Cells */
                      body .dataTable th[tabindex='0']:focus-visible,
                      body .dataTable td.focus {
                        outline: 3px solid #ff3c00 !important;
                        outline-offset: -2px; /* Negative offset looks better on table cells */
                      }
                      
                       /* --- STYLE THE LEGEND CONTAINER --- */
                      .legend-container {
                        display: flex;         /* Aligns items in a row */
                        align-items: center;   /* Vertically centers the items */
                        gap: 20px;             /* Adds space between each legend item */
                        padding: 10px;         /* Adds some padding around the container */
                      }
                
                      /* --- STYLE EACH LEGEND ITEM (BOX + TEXT) --- */
                      .legend-item {
                        display: flex;
                        align-items: center;
                        gap: 8px;              /* Adds space between the color box and the text */
                      }
                      
                      /* --- CUSTOMIZE THE COLOR BOX SIZE HERE --- */
                      .color-box {
                        width: 28px;            /* <-- Change the width of the square box */
                        height: 28px;           /* <-- Change the height of the square box */
                        border: 1px solid #ddd; /* Adds a subtle border to the box */
                      }
                
                      /* --- CUSTOMIZE THE TEXT SIZE HERE --- */
                      .legend-text {
                        font-size: 23px;        /* <-- Change the font size of the text */
                      }
                
                      /* --- COLOR DEFINITIONS BASED ON YOUR LOGIC --- */
                      .color-very-low { background-color: #BAE8DE; }      /* Light Teal */
                      .color-low { background-color: #B8E5AC; }           /* Light Green */
                      .color-moderate { background-color: #FEA82F; }      /* Orange */
                      .color-high { background-color: #F45B53; }          /* Coral Red */
                      .color-very-high { background-color: #C15C9C; }     /* Pinkish Purple */
                      .color-nodata { background-color: #969696; }        /* Grey */
                      .selectize-dropdown .optgroup-header {
                        font-size: 19px;
                        font-weight: bold;
                        color: #2c3e50;
                        background-color: #e6f2ff;
                      }
                      .selectize-dropdown .option {
                        font-size: 17px;
                      }
                                                  
                ")),
                    sidebarMenu(
                      id = "sidebar",
                      menuItem("Home", tabName = "dashboard", icon = icon("home")),
                      tags$hr(),
                      menuItem("About the Dashboard", tabName = "technical_notes", icon = icon("file-alt")),
                      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
                      tags$hr(),
                      menuItem("Respiratory Virus Data", tabName = "overview", icon = icon("viruses")),
                      hidden(
                        div(
                          id = "overview_controls",
                          style = "padding-left: 10px; padding-right: 10px;", 
                          selectInput(
                            inputId = "pathogen",
                            label = "Select pathogen",
                            choices = c("SARS-CoV-2", "Flu A", "Flu B", "RSV"),
                            multiple = FALSE,
                            selected =  "SARS-CoV-2" 
                          ),
                          hidden(
                            div(
                              id = "flu_switch_A_panel",
                              tags$label(`for` = "flu_switch_A", style = "padding-left: 15px;",
                                         tags$span("Include Flu B", style = "font-size: 18px; font-weight: bold;")),
                              
                              switchInput(inputId = "flu_switch_A", value = F, onLabel = "Yes", offLabel = "No"),
                              tags$label(`for` = "h5_switch", style = "padding-left: 15px;",
                                         tags$span("Include Flu A (H5)", style = "font-size: 18px; font-weight: bold;")),
                              
                              switchInput(inputId = "h5_switch", value = F, onLabel = "Yes", offLabel = "No"),
                            )
                          ),
                          hidden(
                            div(
                              id = "flu_switch_B_panel",
                              tags$label(`for` = "flu_switch_B", style = "padding-left: 15px;",
                                         tags$span("Include Flu A", style = "font-size: 18px; font-weight: bold;")),
                              
                              switchInput(inputId = "flu_switch_B", value = T, onLabel = "Yes", offLabel = "No")
                            )
                          ),
                          hidden(
                            div(
                              id = "region_panel",
                              uiOutput("dynamic_HO_picker"),
                            )
                          ),
                          hidden(
                            div(
                              id = "sewershed_panel",
                              uiOutput("dynamic_SHO_picker"),
                              uiOutput("dynamic_wwtp_picker")
                            )
                          )
                        )
                      ),

                      ### Data Download ---------------------------------------------------------------
                      tags$hr(),
                      menuItem("Data Download", tabName = "download", icon = icon("download")),
                      conditionalPanel(
                        condition = "input.sidebar == 'download'", 
                        conditionalPanel(
                          condition = "input.dataset == 'Wastewater Data'",
                          div(
                            style = "margin-top: -10px;",  
                            checkboxGroupInput("show_vars_wastewater", 
                                               h3("Select columns to view and download", style = "color: #337ab7; margin-top: 0px; margin-bottom: 5px;"),  
                                               choices = names(download_df1), selected = names(download_df1))
                          ),
                          div(
                            id = "downloadButtonWithTooltip1",  
                            style = "margin-left: 11px;", 
                            actionButton('confirmDownload', 'Download Data', 
                                         style = "font-size: 18px; padding: 10px 20px; background-color: 
                                  #337ab7; color: white; border: none; border-radius: 5px;"),
                            span(class = "tooltip-text", "Click to download the selected data. 
                        Please note, the download progress may take a few moments to appear due to the large file size.")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.dataset == 'Metrics Summary Data'",
                          div(
                            style = "margin-top: -10px;",  
                            checkboxGroupInput("show_vars_metrics", 
                                               h3("Select columns to view and download", style = "color: #337ab7; margin-top: 0px; margin-bottom: 5px;"),
                                               choices = names(download_df2), selected = names(download_df2))
                          ),
                          
                          div(
                            id = "downloadButtonWithTooltip2",  
                            style = "margin-left: 11px;",  
                            downloadButton('downloadData2', 'Download Data', 
                                           style = "font-size: 18px; padding: 10px 20px; 
                                  background-color: #337ab7; color: white; border: none; 
                                  border-radius: 5px;"),
                            span(class = "tooltip-text", "Click to download the selected data")
                          )
                        )
                      ),
                      
                      tags$hr(),
                      menuItem("Contact Us", href = "mailto:Wastewatersurveillance@cdph.ca.gov", icon = icon("envelope")),
                      div(style = "margin-top: 0px;"),
                      
                      div(
                        style = "text-align: left; margin-top: 20px; margin-left: 5px;",  # Add left margin
                        tags$img(
                          src = "cdph_logo_2024e.png",
                          alt = "California Department of Public Health Logo",
                          style = "max-width: 90%; height: auto; width: 280px;"  # Adjust the width as needed
                        )
                      ),
                      div(
                        style = "text-align: left; margin-top: 20px; margin-left: 10px;",
                        
                        tags$p(
                          tags$a(
                            href   = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", 
                            "CDPH Wastewater Surveillance Webpage", 
                            target = "_blank",
                            class  = "text16"
                          )
                        ),
                        
                        tags$p(
                          tags$a(
                            href   = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx",
                            "CDPH Respiratory Virus Report", 
                            target = "_blank",
                            class  = "text16"
                          )
                        ),
                        
                        tags$br(),
                        
                        tags$p(
                          "Developed in ",
                          tags$a(href = "https://shiny.posit.co", "R-Shiny", target = "_blank"),
                          class = "text18"
                        ),
                        
                        tags$p(
                          "Version released on Oct 3, 2025",
                          class = "text18"
                        )
                      )
                    )
                  ),
                  
                  ## DashboardBody -----------------------------------------------------------
                  
                  dashboardBody(
                   
                    ### Home page ---------------------------------------------------------------
                    tabItems(
                      tabItem(tabName = "dashboard",
                              h1(strong("California Wastewater Surveillance Dashboard")),
                              p(
                                style = "font-size: 23px;", 
                                "The last update for state and regional summaries was ", 
                                strong(published_date_state$n), 
                                ", with sewershed data and metrics being updated daily, Monday through Friday."
                              ),
                              h2(strong("Statewide Overview"), " (click the info box to be directed to the pathogen page)"),
                              br(),
                              fluidRow(
                                div(class = "col-md-3 col-sm-11", uiOutput("home_covid")),
                                div(class = "col-md-3 col-sm-11", uiOutput("home_fluA")),
                                div(class = "col-md-3 col-sm-11", uiOutput("home_fluB")),
                                div(class = "col-md-3 col-sm-11", uiOutput("home_rsv"))
                              ),
                              h4(
                                style = "font-size: 23px; margin-bottom: 2px;", 
                                "Each info box is colored to show the statewide virus level, as defined by the legend below."
                              ),
                              div(class = "legend-container",
                                  div(class = "legend-item",
                                      span(class = "color-box color-very-high"),
                                      span(class = "legend-text", "Very High")
                                  ),
                                  div(class = "legend-item",
                                      span(class = "color-box color-high"),
                                      span(class = "legend-text", "High")
                                  ),
                                  div(class = "legend-item",
                                      span(class = "color-box color-moderate"),
                                      span(class = "legend-text", "Moderate")
                                  ),
                                  div(class = "legend-item",
                                      span(class = "color-box color-low"),
                                      span(class = "legend-text", "Low")
                                  ),
                                  div(class = "legend-item",
                                      span(class = "color-box color-very-low"),
                                      span(class = "legend-text", "Very Low")
                                  ),
                                  div(class = "legend-item",
                                      span(class = "color-box color-nodata"),
                                      span(class = "legend-text", "Not enough data")
                                  )
                                  
                              ),
                              h2(strong("Statewide Wastewater Surveillance Summary:")),
                              
                              uiOutput("statewide_trend_summmary"),
                              
                              div(
                                # Style is now applied to each 'p' tag directly
                                h2(strong("What does this data tell us?")),
                                p("Wastewater concentrations provide a snapshot of how much virus is in the community.", style = "font-size: 23px; line-height: 1.5;"),
                                
                                h2(strong("What doesn't this data tell us?")),
                                p("It can't show how many people are actually feeling sick or need medical care. 
                                  To get the complete picture of community health, we also look at other data and ",
                                  a(
                                    "you can find that information here.", 
                                    href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx",
                                    target = "_blank",
                                    style = "color: #00008B;"
                                  ),
                                  style = "font-size: 23px; line-height: 1.5"
                                )
                              ),
                              h2(strong("Cal-SuWers Wastewater Dashboard Summary")),
                              p(style = "font-size: 23px;",
                                "This dashboard provides an overview of wastewater data for SARS-CoV-2 (the virus that causes COVID-19), 
                                influenza, and respiratory syncytial virus (RSV) in California. This data is produced by multiple programs 
                                participating in the California Department of Public Health (CDPH) California Surveillance of Wastewaters 
                                (Cal-SuWers) network. The CDPH Cal-SuWers network also participates in the CDC National Wastewater 
                                Surveillance System (NWSS).", 
                                style = "font-size: 23px;", 
                                tags$a(href = "#", "Click here to view the contributors to the data on this site",
                                       style = "color: #00008B; font-size: 23px;",  # Darker blue color
                                       onclick = "$('#contributors_list').collapse('toggle'); return false;")
                              ),  
                              div(
                                id = "contributors_list",
                                class = "collapse",
                                style = "border: 1px solid #eee; padding: 15px; margin-top: 10px; border-radius: 5px;",
                                tags$ul(
                                  tags$li("The CDPH Cal-SuWers Program", style = "font-size: 23px;"),
                                  tags$li("The CDPH Drinking Water and Radiation Lab", style = "font-size: 23px;"),
                                  tags$li("WastewaterSCAN", style = "font-size: 23px;"),
                                  tags$li("The Centers for Disease Control and Prevention (CDC) National Wastewater Surveillance System (NWSS)", style = "font-size: 23px;"),
                                  tags$li("Historical programs that are not currently sampling", style = "font-size: 23px;"),
                                  tags$li("Wastewater utilities", style = "font-size: 23px;")
                                )
                              ),
                              p(
                                style = "font-size: 22px;",
                                "For more information, please visit our ",
                                actionLink(inputId = "about_dashboard_link", "About the Dashboard", style = "color: #00008B;"),
                                " and ",
                                actionLink(inputId = "instructions_link", "Instructions", style = "color: #00008B;"),
                                " sections. For more information on how wastewater surveillance works please visit the ",
                                a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", 
                                  "Cal-SuWers homepage", 
                                  style = "color: #00008B;", target = "_blank"),
                                ".",
                                "If you have any questions, comments, or suggestions, please contact us at ",
                                a(href = "mailto:Wastewatersurveillance@cdph.ca.gov", 
                                  "Wastewatersurveillance@cdph.ca.gov", 
                                  style = "color: #00008B;"),
                                "."
                              )
                      ),

                      ### Respiratory Virus Data ---------------------------------------------------------
                      
                      tabItem(tabName = "overview",
                              tabsetPanel(id = "tab",
                                          tabPanel("Statewide",
                                                   div(
                                                     style = "background-color: white; padding: 15px;",
                                                     uiOutput("state_summary_ui")
                                                   )
                                          ),
                                          tabPanel("Region",
                                                   tagList(
                                                     div(
                                                       style = "background-color: white; padding: 15px;",
                                                       
                                                       # CSS styles remain the same
                                                       tags$head(tags$style(HTML("
                                                        #region_toggle_container .btn-group {
                                                          height: 51px !important;
                                                        }
                                                        #region_toggle_container .btn {
                                                          display: flex;
                                                          align-items: center;
                                                          justify-content: center;
                                                          height: 80%;
                                                          font-size: 1.8rem !important;
                                                        }
                                                      "))),
                                                       
                                                       uiOutput("region_content"),
                                                       
                                                       fluidRow(
                                                         column(width = 5,
                                                                div(id = "region_toggle_container",
                                                                    radioGroupButtons(
                                                                      inputId = "region_toggle",
                                                                      label = NULL,
                                                                      choices = c("Overview", "Each Region"),
                                                                      selected = "Overview",
                                                                      status = "primary",
                                                                      justified = TRUE
                                                                    )
                                                                ),
                                                                br(),
                                                                
                                                                conditionalPanel(
                                                                  condition = "input.region_toggle == 'Overview'",
                                                                  div(class = "loading", `data-loading` = "true",
                                                                      tags$div(role = "img", `aria-label` = "Heatmap showing wastewater trend and level",
                                                                               leafletOutput("heatmap_region_2", height = "900px") %>% withSpinner(color = "#5A789A")
                                                                      )
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.region_toggle == 'Each Region'",
                                                                  div(class = "loading", `data-loading` = "true",
                                                                      tags$div(role = "img", `aria-label` = "Heatmap showing wastewater trend and level",
                                                                               leafletOutput("heatmap_region", height = "780px") %>% withSpinner(color = "#5A789A")
                                                                      )
                                                                  )
                                                                )
                                                         ),
                                                         
                                                         column(width = 7,
                                                                conditionalPanel(
                                                                  condition = "input.region_toggle == 'Overview'",
                                                                  div(class = "region-plot2-container",
                                                                      uiOutput("h5_message_region_1"),
                                                                      plotlyOutput("region_plot2") %>% withSpinner(color = "#5A789A")
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.region_toggle == 'Each Region'",
                                                                  tagList(
                                                                    div(class = "level-box-container", uiOutput("level_box")),
                                                                    uiOutput("h5_message_region_2"),
                                                                    div(class = "loading", `data-loading` = "true",
                                                                        tags$div(role = "img", `aria-label` = "Plot showing normalized virus concentration over time in wastewater",
                                                                                 uiOutput("each_region_plot_ui") %>% withSpinner(color = "#5A789A")
                                                                        )
                                                                    )
                                                                  )
                                                                )
                                                         )
                                                       ), 
                                                       
                                                       fluidRow(
                                                         column(width = 12,
                                                                
                                                                div(
                                                                  style = "margin-top: 40px;", # <-- THIS IS THE FIX
                                                                  titlePanel(
                                                                    uiOutput("region_table_header")
                                                                  )
                                                                ),
                                                                
                                                                bsCollapse(
                                                                  id = "region_collapseTable",
                                                                  open = "panel1",
                                                                  bsCollapsePanel(
                                                                    tags$span("Show/Hide Table", style = "font-size: 24px; font-weight: bold;"),
                                                                    DT::dataTableOutput("region_summary_table") %>% withSpinner(color = "#5A789A"),
                                                                    value = "panel1"
                                                                  )
                                                                )
                                                         )
                                                       )
                                                     )
                                                   )
                                          ),
                                          
                                          tabPanel("Sewershed",
                                                   div(
                                                     style = "background-color: white;padding: 15px;",
                                                     # br(),
                                                     fluidRow(
                                                       div(
                                                         style = "background-color: white; padding: 15px;",
                                                         uiOutput("overview_title_sewershed")
                                                       )
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(
                                                         6,
                                                         div(class = "loading", `data-loading` = "true",
                                                             tags$div(role = "img", `aria-label` = "Heatmap showing wastewater trend and level",
                                                                      leafletOutput("heatmap_sewershed", height = "780px") %>% withSpinner(color = "#5A789A")
                                                             )
                                                         )
                                                       ),
                                                       column(
                                                         6,
                                                         div(class = "level-box-container", uiOutput("level_box_sewershed")),
                                                         uiOutput("h5_message_sewershed"),
                                                         div(class = "loading", `data-loading` = "true",
                                                             tags$div(role = "img", `aria-label` = "Plot showing normalized virus concentration over time in wastewater",
                                                                      uiOutput("dynamic_sewershed_plot") %>% withSpinner(color = "#5A789A")
                                                             )
                                                         )
                                                       )
                                                     ),
                                                     br(),
                                                     list(
                                                       titlePanel(
                                                         uiOutput("table_header"),
                                                       ),
                                                       bsCollapse(
                                                         id = "collapseTable",  # Assign an ID to the collapse
                                                         open = "panel1",         # Specify the panel to be open by default
                                                         bsCollapsePanel(
                                                           tags$span("Show/Hide Table", style = "font-size: 24px; font-weight: bold;"),
                                                           DT::dataTableOutput("sewershed_summary_table") %>% withSpinner(color = "#5A789A"),
                                                           value = "panel1"       # Give a value that matches the open argument
                                                         )
                                                       )
                                                     )
                                                   )
                                          )
                              )    
                              
                      ),
                      
                      ### About this dashboard ----------------------------------------------------
                      
                      tabItem(tabName = "technical_notes",
                              
                              div(class = "technical-notes-section", 
                                  
                                  h2("About the Dashboard Page", style = "font-size: 35px;"),
                                  p("The California Surveillance of Wastewaters (Cal-SuWers) Wastewater dashboard displays an overview of wastewater 
                                    surveillance data for SARS-CoV-2 (the virus that causes COVID-19), influenza, and respiratory syncytial virus (RSV) 
                                    in California."),
                                  p("The Cal-SuWers Network collects and analyzes wastewater samples. Partners to the CDPH Cal-SuWers program include 
                                    the CDPH Drinking Water and Radiation Lab (DWRL), the Centers for Disease Control and Prevention (CDC) National 
                                    Wastewater Surveillance System (NWSS), WastewaterSCAN, wastewater utilities, academic researchers, local county 
                                    health department laboratories, private laboratories, and other partners across the state. "),
                                  p("For more information on how wastewater surveillance works please visit the ",
                                    tags$a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", "Cal-SuWers homepage", target = "_blank", style = "color: #00008B;"),  # Change to your URL
                                    "."),
                                  
                                  h3("Wastewater Concentrations"),
                                  p('The measure how much of a microbe (for example, the SARS-CoV-2 virus or the influenza virus) is present in a
                                    wastewater sample is known as the  “concentration”.'),
                                  
                                  h3("Sewersheds"),
                                  p('This dashboard includes maps showing outlines of each wastewater treatment plant’s service area, which is called a 
                                    “sewershed.” Sewershed boundaries represent the service area for each treatment plant, and show the community what the wastewater data represent.'),
                                  
                                  h3("Regions"),
                                  p(
                                    style = "font-size: 22px;",
                                    "Wastewater data from counties are grouped into ",
                                    a(href = "https://www.cdph.ca.gov/Programs/RPHO", 
                                      "six regional public health office (RPHO)", 
                                      style = "color: #00008B;", target = "_blank"),
                                    " regions based on geography. These groupings are used to produce regional aggregates and trends."
                                  ),
                                  p(tags$a(href = "#", "Click here to see a list of California counties by region",
                                           style = "color: #00008B;",  # Darker blue color
                                           onclick = "$('#regionDetails').collapse('toggle'); return false;")),  # Prevent default action
                                  div(
                                    id = "regionDetails",
                                    class = "collapse",  
                                    tags$ul(
                                      lapply(seq_along(county_list), function(i) {
                                        tags$li(
                                          tags$b(paste(names(county_list)[i], ": ")), 
                                          paste(county_list[[i]], collapse = ", ")
                                        )
                                      })
                                    )
                                  ),
                                  
                                  h3("State and Regional Wastewater Concentrations"),
                                  p(
                                    "To report wastewater trends and levels for the state and for each of California's six regional public health
                                    office (RPHO) regions, we combine data from multiple sewersheds. We do this by:"
                                  ),
                                  tagList(
                                    p(
                                      tags$strong("1. Standardizing the data from each sewershed"), ": We first convert wastewater concentrations from each sewershed into a standardized value 
                                      called the Wastewater Viral Activity Level (WVAL). This value is determined based on a method developed by the CDC’s National Wastewater 
                                      Surveillance System (NWSS), with some modifications by the Cal-SuWers team to better reflect California’s data."
                                    ),
                                    p(
                                      tags$strong("2. Creating regional and statewide summaries"), ": Once each sewershed has a WVAL, we calculate a population-weighted average of all the WVALs 
                                      in a region (or the state). This gives us a single summary value that reflects overall COVID-19 activity in wastewater for that area."
                                    ),
                                    p(
                                      "For more technical details on how the WVAL value is calculated, please see the", 
                                      a(href = "https://www.cdc.gov/nwss/data-methods.html", 
                                        "CDC NWSS’s teams data notes page on the WVAL", 
                                        style = "color: #00008B;", target = "_blank"),
                                      "or reach out to our",
                                      a(href = "mailto:Wastewatersurveillance@cdph.ca.gov", 
                                        "Cal-SuWers Team", 
                                        style = "color: #00008B;", target = "_blank"),
                                      "."
                                    )
                                  ),
                                  
                                  h3("State and Regional Wastewater Levels"),
                                  p('To report how high or low wastewater concentrations are, we categorize the state and regional WVALs 
                                    using five different levels:', 
                                    tags$b(style = "background-color: #BAE8DE; color: black; padding: 2px 4px;", "Very Low"), ", ",
                                    tags$b(style = "background-color: #B8E5AC; color: black; padding: 2px 4px;", "Low"), ", ",
                                    tags$b(style = "background-color: #FEA82F; color: black; padding: 2px 4px;", "Moderate"), ", ",
                                    tags$b(style = "background-color: #F45B53; color: white; padding: 2px 4px;", "High"), " and ",
                                    tags$b(style = "background-color: #C15C9C; color: white; padding: 2px 4px;", "Very High"), "."),
                                  
                                  tagList(
                                    p(
                                      "We use the California state WVAL to set thresholds for each respiratory virus. To do this, we first look at wastewater WVAL values from times when fewer than 5% (Flu/COVID-19 < 5% or  RSV <3%) of people tested positive for the virus across the state. ",
                                      "We use this data from the past two seasons to find the average (mean) and how much the data varies (standard deviation). ",
                                      "Next, we calculate a value called D. This tells us how much higher the highest WVAL from recent seasons is compared to the average WVAL during times of low test positivity. ",
                                      "We look at the past 5 seasons for COVID-19 (SARS-CoV-2) and the past 3 seasons for flu and RSV. ",
                                      "We take the difference between the highest value and the average during low test postivity, divide it by 4, and then multiply by the standard deviation. ",
                                      "We then use average and D to set four thresholds:"
                                    ),
                                    tags$ul(
                                      tags$li("Very Low to Low: just the average."),
                                      tags$li("Low to Moderate: average + D."),
                                      tags$li("Moderate to High: average + 2D."),
                                      tags$li("High to Very High: average + 3D.")
                                    ),
                                    p(
                                      "These thresholds help us understand how much virus is in the wastewater and how it compares to past seasons."
                                    )
                                  ),
                                  
                                  tagList(
                                    p(
                                      style = "font-size: 22px;",
                                      "For more technical details on this method, please see the ",
                                      a(href = "https://www.cdc.gov/respiratory-viruses/data/activity-levels.html", 
                                        "CDC’s ARI data notes page", 
                                        style = "color: #00008B;", target = "_blank"),
                                      " or reach out to us. Please note that we are using a different wastewater levels categorization method than the ",
                                      a(href = "https://www.cdc.gov/nwss/index.html", 
                                        "CDC NWSS program’s wastewater level categorization", 
                                        style = "color: #00008B;", target = "_blank"),
                                      ", and the wastewater levels reported here may differ from those reported on the CDC NWSS dashboard."
                                    ),
                                    p(
                                      style = "font-size: 22px;",
                                      "Because the range of wastewater concentrations over time are different for SARS-CoV-2, Influenza, and RSV, the wastewater 
                                      cut-offs are different for each virus. This table defines the cut-offs for the 5 different categories for each disease 
                                      (in terms of WVALs)."
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(
                                      dataTableOutput("levelsTable"), width = 8, style = "font-size:18px")
                                  ),
                                  tagList(
                                    br(),
                                    p(
                                      tags$b("**Fall of 2025: A note on new COVID-19, Influenza, and RSV wastewater levels**")
                                    ),
                                    p(
                                      tags$strong("1. To improve accuracy"), ": our previous method compared current COVID-19 wastewater concentrations to the last one year of data. 
                                      This meant that if COVID-19 activity over the past year was especially low or high, current wastewater concentrations might be 
                                      misleadingly categorized in the opposite way.  Now, we compare current COVID-19 levels to five years of data, which gives a more 
                                      accurate picture of how current wastewater levels compare to general COVID-19 activity over time."
                                    ),
                                    p(
                                      tags$strong("2. To better align with other COVID-19 indicators"), ": We tested several methods, and this new approach aligns more closely with levels 
                                      being used to categorize clinical surveillance measures including test positivity and hospital admissions. This helps improve interpretability 
                                      of all of the COVID-19 surveillance systems."
                                    ),
                                    p(
                                      tags$strong("This new method also works well for influenza and RSV, and has been adapted for these two viruses on our dashboard.")
                                    )
                                  ),
                                  h3("State, Regional, and Sewershed Trends"),
                                  p("Trend models help us see if wastewater concentrations have changed over the last three weeks. They show if 
                                    concentrations are going up (increasing), staying the same (plateauing), or going down (decreasing). The 
                                    models also estimate how much concentrations have changed, shown as a percent change. Sometimes the wastewater 
                                    measurements can vary a lot from one sample to another, which makes it difficult to get an accurate trend. The 
                                    trend models give an estimate of how certain we are about the estimated trend by showing a range of possible 
                                    percent change values in brackets. If the range of values is small, then we are very confident in the trend. 
                                    Keep in mind that these trends show what is happening in the wastewater right now, and they do not predict 
                                    what will happen in the future. Wastewater trends are calculated using a method that looks at the estimated 
                                    percent change in wastewater levels over the recent 3-week period. The percent change values are grouped into 
                                    the following categories: "),
                                  
                                  fluidRow(
                                    column(
                                      dataTableOutput("trendsTable"), width = 6, style = "font-size:18px")
                                  ),
                                  br(),
                                  p(
                                    "* If there has been no sample in the past 10 days or if there are fewer than 3 samples in the past 21 days, ",
                                    tags$b("Not enough data"),
                                    " will be assigned. Additionally, ",
                                    tags$b("Sporadic Detections"),
                                    " applies when more than 1 of the past 5 samples are below the limit of detection (or LOD), and ",
                                    tags$b("All Samples Below LOD"),
                                    " applies when all 5 of the past 5 samples are below the limit of detection (LOD). In these cases, concentrations are very low and often undetectable, making trend models noisy and unreliable. Thus, ",
                                    tags$b("Sporadic Detections"),
                                    " and ",
                                    tags$b("All Samples Below LOD"),
                                    " indicate very low concentrations with no notable trends."
                                  ),
                                  h3("Data Sources"),
                                  p("In California, wastewater monitoring is carried out by several groups. Groups who contribute data to the Cal-SuWers Network, which is 
                  managed by CDPH and submitted to the Centers for Disease Control and Prevention (CDC) National Wastewater Surveillance System (NWSS), 
                  are listed below. "),
                                  tags$ul(
                                    tags$li("CDPH Drinking Water and Radiation Lab (DWRL)"),
                                    tags$li(
                                      tags$a(href = "https://wastewaterscan.org/", "Wastewater SCAN", target = "_blank", style = "color: #00008B;")  # Hyperlinked text
                                    ),
                                    tags$li(
                                      tags$a(href = "https://www.cdc.gov/nwss/wastewater-surveillance.html", "CDC NWSS Contract", target = "_blank", style = "color: #00008B;")  # Hyperlinked text
                                    )
                                  ),
                                  h3("Dashboard Updates"),
                                  p("The state and regional wastewater summaries are updated weekly on Fridays. This weekly update schedule allows us to
                                     include as much recent wastewater data as possible, providing a more complete picture of statewide and regional 
                                     wastewater activity. Because we receive wastewater data for individual sewersheds on an ongoing basis, 
                                     the sewershed-level data is updated daily, Monday through Friday, by 5:30 PM. However, please note that these daily 
                                     updates may not immediately be reflected on the dashboard by the end of the day or the following day, depending on 
                                     data processing and system refresh times."),
                                  h3("Dashboard Turnaround Time"),
                                  p("Wastewater utilities commonly collect samples on Sunday – Thursday, and occasionally on Friday and Saturday. Shipping and receiving 
                                    the samples at the laboratory may take 1 to 2 days. From there, it takes 1 to 7 days for the laboratory to process the sample and 
                                    submit that data to our team. Extra quality checks are conducted by the CDPH Cal-SuWers team, which may take up to 1 day. The overall 
                                    turnaround from when samples are collected to when results for individual sewersheds appear on this dashboard is usually 3–10 days. 
                                    Additional time may be required for various reasons, such as difficulty in reaching sewershed sampling locations, batching of samples, 
                                    or staff and resource constraints. The Cal-SuWers team reviews data one or two times per week, and occasionally removes data points that 
                                    are extremely high or extremely low (also called outliers)."),
                                  p("For state and regional summaries, each Friday update will include data up through the end of the previous week. So, on any given Friday, 
                                   state and regional summaries will be current up through the Saturday before."),
                                  h3("Data Limitations"),
                                  p("Wastewater surveillance for diseases is still a new field that is changing, and we are still learning about 
                  what the data means. Below are some important things to keep in mind when reviewing this data."),
                                  tags$ul(
                                    tags$li("Some buildings or locations (like prisons, universities, or hospitals) are not connected to community
                          wastewater treatment plants because they have their own smaller wastewater treatment plants. Also, 
                          homes with septic tanks aren’t connected to wastewater treatment plants. Because of this, our 
                          wastewater samples may not include all the people living and working in each sewershed’s community."),
                                    tags$li("People move in and out of sewersheds (e.g., commuters, tourists). Therefore, wastewater data might not 
                          fully represent the community served by the wastewater treatment plant, as it can be influenced by commuting patterns and visitors."),
                                    tags$li("Individuals may shed varying amounts of the virus when they are sick."),
                                    tags$li("When a small number of people are sick within a community, there is much less virus in the wastewater, so 
                          wastewater monitoring may not be able to detect any viruses."),
                                    tags$li("Wastewater is an environmental sample and can have many inputs (including human, animal, or industrial waste). 
                          Environmental samples vary day-to-day due to differences in these inputs and due to laboratory methods. Because of this, 
                          to find out if SARS-CoV-2 concentrations are changing, trends are more reliable than individual data points."),
                                    tags$li("We can’t tell exactly how many people are sick with a disease with wastewater data alone.")
                                  ),
                                  h3("WastewaterSCAN Data Limitations Notice:"),
                                  p("For data from WastewaterSCAN/SCAN programs (see “WastewaterSCAN” and “Sewage Coronavirus Alert Network (SCAN)” 
                  under “data_source”), content is licensed under CC BY-NC 4.0. These data were generated by the WastewaterSCAN/SCAN 
                  projects, philanthropically funded through a gift to Stanford University. All results are understood to be based on 
                  inputs that are experimental in nature and are not intended to diagnose or treat any disease. The results are 
                  provided “as is” and without warranty of any kind. Stanford is not liable for any claim arising out of or in 
                  connection with the disclosure of these results. By accessing or copying any part of the database, the user 
                  accepts the terms of this license. These data are being made available to inform public health decision making. 
                  Anyone seeking to use the database for other purposes or for research is required to contact the WastewaterSCAN/SCAN 
                  team at", 
                                    tags$a(href="mailto:wwscan_stanford_emory@lists.stanford.edu", "wwscan_stanford_emory@lists.stanford.edu", style = "color: #00008B;"), 
                                    "or Alexandria Boehm at", tags$a(href="mailto:aboehm@stanford.edu", "aboehm@stanford.edu", style = "color: #00008B;"), ". 
                  Any questions about the data, or the methods used to generate or produce any data products should be 
                  directed to the same emails. Please see the WastewaterSCAN website for more information on data licensing. 
                  Some of the methods are described in this ", 
                                    tags$a(href = "https://www.nature.com/articles/s41597-024-03969-8.epdf?sharing_token=kU_Vj0x77_tgCmGpC6n2RdRgN0jAjWel9jnR3ZoTv0P8VvARrPRF3g0t85lnnbWNaGYeN7T9wl_AIF9pPEh118hw0mLT0JOBA3lSnEmt7nxbHuxrLkumYDVvL7hgANirv6mOh6MiDPeA4XUqw8uszzaR2o-D142viYL-rWca9EI%3D", 
                                           "peer-reviewed paper", 
                                           style = "color: #00008B;", target = "_blank"),
                                    "."),
                                  p("Methodologies for producing wastewater data are not currently standardized, and analyses, comparisons, 
                  and aggregations should be done with caution. Wastewater is a complex environmental sample and inherent 
                  variability in measured concentrations is expected due to environmental variability, day-to-day differences 
                  in sewershed and population dynamics, differences in the amount of shedding between people and pathogens, 
                  and laboratory and sampling variability. Please see the",
                                    a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", 
                                      "CDPH Cal-SuWers", 
                                      style = "color: #00008B;", target = "_blank"),
                                    ", ",
                                    a(href = "https://www.cdc.gov/nwss/index.html", 
                                      "CDC NWSS", 
                                      style = "color: #00008B;", target = "_blank"),
                                    ", ",
                                    a(href = "https://archive.cdc.gov/www_cdc_gov/nwss/interpretation.html", 
                                      "CDC Public Health interpretation and Use of Wastewater Surveillance data", 
                                      style = "color: #00008B;", target = "_blank"),
                                    "webpages for more information.")
                              ),
                              br(),
                              br()
                      ),
                    
                      ### Dashboard Instructions --------------------------------------------------
                     
                      tabItem(tabName = "instructions",
                              
                              div(class = "instructions-section", 
                                  h2("Dashboard Instructions", style = "font-size: 35px;"),
                                  h3("Navigating the Dashboard", style = "font-size: 25px;"),
                                  tags$ul(
                                    tags$li("Use the sidebar on the left to navigate between different sections of the dashboard."),
                                    tags$li(
                                      "To access the ", tags$strong("Respiratory Virus Data"), 
                                      " section, either click the info box on the homepage or select it directly from the sidebar."
                                    )
                                  ),
                                  h3("Navigating the Respiratory Virus Data Section", style = "font-size: 25px;"),
                                  h4(tags$strong("Region Tab")),
                                  tags$ul(
                                    tags$li("Select ", tags$strong("Overview"), " to view virus concentrations across all regions."),
                                    tags$li("To explore a specific region, choose ", tags$strong("Each Region"), ", then either:"),
                                    tags$ul(
                                      tags$li("Click a region on the map, or"),
                                      tags$li("Select a region from the dropdown menu"),
                                      tags$li("This will display the region’s trend and level data.")
                                    )
                                  ),
                                  
                                  h4(tags$strong("Sewershed Tab")),
                                  tags$ul(
                                    tags$li("Click a sewershed on the map or select one from the dropdown menu to view its trend and level data."),
                                    tags$li(tags$strong("Statewide"), " is the default selection under ", tags$strong("Select Region"), "."),
                                    tags$li("When ", tags$strong("Statewide"), " is selected, the map and dropdown will display all sewersheds from all regions."),
                                    tags$li("To view sewersheds within a specific region, choose that region from the ", tags$strong("Select Region"), " dropdown.")
                                  ),
                                  p(strong(em("Tip:")), em("You can also hover over a sewershed area on the map to see the name, level, 
                                           and trend for that site. You may also select a sewershed by clicking on the 
                                           map to view data. However, please note that the sites on the map may be fewer than those in the
                                           dropdown due to the lack of shapefiles for all sewersheds.")),
                                  br(),
                                  h3("Viewing the Data", style = "font-size: 25px;"),
                                  p("Once you have selected a site or region, the summary text and plot will populate on the right-hand of the dashboard."),
                                  h4(strong("Step 1:"), "Adjust the Custom Timescale Plot to a desired date range"),
                                  tags$ul(
                                    tags$li("The default view of the plot is to show the entire time series of available data for that site or region."),
                                    tags$li("To adjust the date range, use the slider tool located under the plot to change the view to a desired time period.")
                                  ),
                                  p(
                                    strong(em("Tip:")), 
                                    em("To reset the plot, click the reset icon ("), 
                                    icon("refresh"),  # Adds the house icon
                                    em(") below the zoom in ("),
                                    icon("plus"),  # Zoom in icon
                                    em(") and zoom out ("),
                                    icon("minus"),  # Zoom out icon
                                    em(") icons.")
                                  ),
                                  h4(strong("Step 2:"), "For each sewershed site, you can choose to display individual data points by toggling the “Include Data Points” button."),
                                  tags$ul(
                                    tags$li("Horizontal dashed reference lines indicate the level cut-offs to determine low, medium, and high categories."),
                                    tags$li("A vertical reference line labeled “21 days ago” shows trends calculated from data collected in the past 21 days."),
                                    tags$li("The line represents an average of the concentrations of all samples that fall within 10 days of that date (center aligned)."),
                                    tags$li("Points on the plot represent individual sample results:"),
                                    tags$ul(
                                      tags$li("Hovering over data points will display the normalized concentration."),
                                      tags$li(HTML("Data points above the y-axis limit are marked with a closed upward-pointing triangle (&#9650;); otherwise, they are shown as circular symbols (&#9679;).")),
                                      tags$li(HTML("Data points with low concentrations are represented by an open downward-pointing triangle (&#x25BD;), indicating that they are below the limit of detection for laboratory analysis."))
                                    )
                                  ),
                                  p(
                                    tags$i(
                                      tags$b("Tip:"), 
                                      " To view another sewershed you can either select a new sewershed by clicking on the map or select another site from the “Choose a Wastewater Sewershed by County (Utility Name)” filter on the left-hand column."
                                    )
                                  ),
                                  br(),
                                  h3("Downloading the Data", style = "font-size: 25px;"),
                                  p(
                                    "To download and view the entire dataset (default setting), simply select all columns, and avoid 
                    applying any filters in the data table. If you'd like to download and view a specific subset of 
                    the data, select the relevant columns, and apply the necessary filters before downloading."
                                  ),
                                  br(),
                                  br()
                                  
                              )
                      ),
                      tabItem(tabName = "resources",
                              h2("Resources")
                      ),
                      
                      ### Data download UI --------------------------------------------------------
                      
                      tabItem(tabName = "download",
                              fluidPage(
                                
                                fluidRow(
                                  column(
                                    width = 12,  
                                    tabsetPanel(
                                      id = 'dataset',
                                     
                                      tabPanel("Wastewater Data",
                                               br(),
                                               DT::DTOutput("download_table1") %>% withSpinner(color = "#5A789A")
                                      ),
                                      tabPanel("Metrics Summary Data",
                                               br(),
                                               DT::DTOutput("download_table2") %>% withSpinner(color = "#5A789A")  # Metrics Summary Data Table
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
)
