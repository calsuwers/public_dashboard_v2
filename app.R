# =============================================================================
# FILE: app.R
# PROJECT: Cal-SuWers Public Dashboard v2
# DESCRIPTION:
#   Minimal entry point for the Shiny application.
#   Shiny automatically sources global.R, ui.R, and server.R before this file
#   runs — so all libraries, data, and function definitions are already loaded.
#
# HOW TO RUN:
#   1. Open this file in RStudio.
#   2. Click "Run App" (or run shiny::runApp() from the R console).
#
# FILE STRUCTURE:
#   app.R           ← This file (entry point)
#   global.R        ← Libraries, data loading, global objects
#   ui.R            ← User interface layout
#   server.R        ← Server logic, reactives, outputs
#   R/
#     functions.R   ← Custom helper functions (sourced by global.R)
#   www/
#     (CSS, images, favicon, analytics.js)
#   data_folder/
#     (CSV, RDS, shapefiles — paths configured in global.R)
# =============================================================================

# Launch the Shiny application
shinyApp(ui = ui, server = server)
