# ------------------------------------------------
# Load Libraries ---------------------------------
library(shiny)
# library(shinyMobile)

# Load helper files -----------------------------
source("helper.R")

# Load Data -------------------------------------
source("Weekly_Stat_Predictor.R")
source("position_modeling.R")
source("nfl_eda.R")

# Load UI ----------------------------------------
source("ui.R")

# Server Logic -----------------------------------
source("server.R")

# Run app ----------------------------------------
shinyApp(ui = ui, server = server)

