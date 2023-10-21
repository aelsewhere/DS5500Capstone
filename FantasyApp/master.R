# ------------------------------------------------
# Load Libraries ---------------------------------

library(shiny)
library(shinyMobile)



# Load helper files -----------------------------



# Create Master Variables ----------------------



# Load Data -------------------------------------




# Load UI ----------------------------------------
source("ui.R")


# Server Logic -----------------------------------
source("server.R")


# Run app ----------------------------------------
shinyApp(ui=ui, server=server)
